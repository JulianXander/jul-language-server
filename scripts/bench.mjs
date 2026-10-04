import { execFileSync } from 'child_process';
import { existsSync, readFileSync, statSync } from 'fs';
import { basename, resolve } from 'path';
import { pathToFileURL } from 'url';
import {
	alarmingDeviation,
	appendEntries,
	formatResult,
	getCommit,
	getDeviation,
	getDrift,
	getMachine,
	isComparable,
	noticeableDeviation,
	parseArgs,
	readPrevious,
	stats,
} from './bench-log.mjs';
import {
	collectPositions,
	findJulFiles,
	initialize,
	maxFileSize,
	serverPath,
	startServer,
} from './lsp-client.mjs';

/**
 * Wall-Clock Messung der Server-Latenzen über echtes LSP. Kein Test-Gate, nur Beleg für
 * Umbauten am Server. Misst, was der Editor merkt: Zeit bis Diagnostics und Antwortzeit der
 * positionsbasierten Features.
 * Aufruf: npm run bench [--save] [--note "grund"] [datei|ordner...]
 * Mit --save wird die Messung an scripts/bench-log-lsp.tsv angehängt, ohne nur verglichen.
 * Setzt einen gebauten Server voraus (npm run build).
 */

const requestRunCount = 3;
const maxPositionCount = 200;
// Zeile 47 (0-basiert 46), wo schnelles Tippen im yugioh-Projekt auffiel
const typingLine = 46;
// ~ 25 Anschläge pro Sekunde, schneller als normales Tippen
const keystrokeGap = 40;
const burstKeystrokeCount = 10;
const typingBurstCount = 5;
// 3 s ohne Pause über 100 ms
const continuousKeystrokeCount = 75;
const continuousRunCount = 3;

const logPath = resolve(import.meta.dirname, 'bench-log-lsp.tsv');
const chartScript = resolve(import.meta.dirname, 'bench-chart.mjs');
// jul-examples ist mit 886 Zeilen zu klein: dort schwankt der Median um mehr als die Alarmschwelle
const preferredTarget = resolve('C:/Projects/privat/yugioh');
const fallbackTarget = resolve(import.meta.dirname, '../../jul-examples');
// gemessen wird größtenteils Compiler-Code, der eigene Commit erklärt die Zahlen allein nicht
const compilerFolder = resolve(import.meta.dirname, '../../jul-compiler');

//#region messung

async function measureOpen(client, filePaths) {
	const durations = [];
	for (const filePath of filePaths) {
		const text = readFileSync(filePath, { encoding: 'utf8' });
		const uri = pathToFileURL(filePath).href;
		const diagnosticsPromise = client.waitForDiagnostics(uri);
		const start = performance.now();
		client.notify('textDocument/didOpen', {
			textDocument: { uri: uri, languageId: 'jul', version: 1, text: text },
		});
		await diagnosticsPromise;
		durations.push(performance.now() - start);
	}
	return durations;
}

/** hängt einen Kommentar an und entfernt ihn wieder: misst den Reparse, ohne die Datei zu ändern */
async function measureChange(client, filePath, runCount) {
	const text = readFileSync(filePath, { encoding: 'utf8' });
	const uri = pathToFileURL(filePath).href;
	const rows = text.split('\n');
	const endPosition = { line: rows.length - 1, character: rows[rows.length - 1].length };
	const insertedText = '\n# bench';
	const insertedEnd = { line: endPosition.line + 1, character: '# bench'.length };
	const durations = [];
	let version = 1;
	for (let run = 0; run < runCount; run++) {
		for (const change of [
			{ range: { start: endPosition, end: endPosition }, text: insertedText },
			{ range: { start: endPosition, end: insertedEnd }, text: '' },
		]) {
			version++;
			const diagnosticsPromise = client.waitForDiagnostics(uri);
			const start = performance.now();
			client.notify('textDocument/didChange', {
				textDocument: { uri: uri, version: version },
				contentChanges: [change],
			});
			await diagnosticsPromise;
			durations.push(performance.now() - start);
		}
	}
	return durations;
}

/**
 * Schnelles Tippen: keystrokeCount Ziffern hintereinander ans Ende einer Zeile, jeweils als eigene
 * didChange. Gemessen wird ab dem letzten Tastendruck:
 * - bis die Diagnostics des letzten Stands da sind (Version im publishDiagnostics)
 * - bis auch die Importeure gecheckt sind (eine Anfrage danach wird erst nach dem ganzen Handler
 *   beantwortet, der Server arbeitet einthreadig)
 * Ein Server, der pro Tastendruck alles neu rechnet, staut hier auf. Danach werden die Ziffern
 * wieder entfernt.
 * Zusätzlich: größter Abstand zwischen zwei Diagnostics der Datei vom ersten Anschlag bis zum
 * letzten Stand (maxGaps). Ein Debounce ohne Obergrenze liefert beim Dauertippen gar nichts, bis
 * die Pause kommt.
 */
async function measureTyping(client, filePath, burstCount, keystrokeCount) {
	const text = readFileSync(filePath, { encoding: 'utf8' });
	const uri = pathToFileURL(filePath).href;
	const rows = text.split('\n');
	const line = Math.min(typingLine, rows.length - 1);
	const character = rows[line].length;
	const toDiagnostics = [];
	const toDependents = [];
	const maxGaps = [];
	let version = 1000;
	for (let burst = 0; burst < burstCount; burst++) {
		let lastSend = 0;
		const publishTimes = [];
		const stopListening = client.listenDiagnostics((publishedUri) => {
			if (publishedUri === uri) {
				publishTimes.push(performance.now());
			}
		});
		const typingStart = performance.now();
		const finalVersion = version + keystrokeCount;
		const diagnosticsPromise = client.waitForDiagnostics(uri, finalVersion);
		for (let keystroke = 0; keystroke < keystrokeCount; keystroke++) {
			version++;
			const position = { line: line, character: character + keystroke };
			client.notify('textDocument/didChange', {
				textDocument: { uri: uri, version: version },
				contentChanges: [{ range: { start: position, end: position }, text: String((keystroke + 1) % 10) }],
			});
			lastSend = performance.now();
			await new Promise(resolveDelay => setTimeout(resolveDelay, keystrokeGap));
		}
		await diagnosticsPromise;
		stopListening();
		toDiagnostics.push(performance.now() - lastSend);
		const times = [typingStart, ...publishTimes];
		maxGaps.push(Math.max(...times.slice(1).map((time, index) => time - times[index])));
		await client.request('textDocument/documentSymbol', { textDocument: { uri: uri } });
		toDependents.push(performance.now() - lastSend);
		// Ziffern wieder entfernen
		version++;
		const revertPromise = client.waitForDiagnostics(uri, version);
		client.notify('textDocument/didChange', {
			textDocument: { uri: uri, version: version },
			contentChanges: [{
				range: {
					start: { line: line, character: character },
					end: { line: line, character: character + keystrokeCount },
				},
				text: '',
			}],
		});
		await revertPromise;
		await client.request('textDocument/documentSymbol', { textDocument: { uri: uri } });
	}
	return { toDiagnostics, toDependents, maxGaps };
}

async function measureRequest(client, method, filePath, positions, extraParams) {
	const uri = pathToFileURL(filePath).href;
	const durations = [];
	for (let run = 0; run < requestRunCount; run++) {
		for (const position of positions) {
			const start = performance.now();
			await client.request(method, {
				textDocument: { uri: uri },
				position: position,
				...extraParams,
			});
			durations.push(performance.now() - start);
		}
	}
	return durations;
}

//#endregion messung

async function main() {
	if (!existsSync(serverPath)) {
		console.error(`${serverPath} fehlt, bitte zuerst npm run build`);
		process.exitCode = 1;
		return;
	}
	const args = process.argv.slice(2);
	const { save, note, targets: targetArgs } = parseArgs(args);
	const targets = targetArgs.length
		? targetArgs.map(target => resolve(target))
		: [existsSync(preferredTarget) ? preferredTarget : fallbackTarget];
	const julFiles = targets
		.flatMap(findJulFiles)
		.filter(filePath => statSync(filePath).size <= maxFileSize);
	if (!julFiles.length) {
		console.error('keine .jul Dateien gefunden');
		process.exitCode = 1;
		return;
	}
	// die größte Datei trägt die Requestmessung, dort ist die Baumsuche am teuersten
	const largestFile = julFiles.reduce((largest, filePath) =>
		statSync(filePath).size > statSync(largest).size ? filePath : largest);
	const rows = readFileSync(largestFile, { encoding: 'utf8' }).split('\n');
	const positions = collectPositions(rows, maxPositionCount);

	const client = startServer();
	const results = [];
	try {
		await initialize(client, targets[0]);
		results.push({
			label: 'didOpen -> diagnostics',
			values: stats(await measureOpen(client, julFiles)),
		});
		results.push({
			label: 'didChange -> diagnostics',
			values: stats(await measureChange(client, largestFile, 5)),
		});
		const typing = await measureTyping(client, largestFile, typingBurstCount, burstKeystrokeCount);
		results.push({ label: 'tippen -> diagnostics', values: stats(typing.toDiagnostics) });
		results.push({ label: 'tippen -> importeure fertig', values: stats(typing.toDependents) });
		const continuous = await measureTyping(client, largestFile, continuousRunCount, continuousKeystrokeCount);
		results.push({ label: 'dauertippen -> diagnostics', values: stats(continuous.toDiagnostics) });
		results.push({ label: 'dauertippen -> max Abstand diagnostics', values: stats(continuous.maxGaps) });
		for (const [label, method, extraParams] of [
			['hover', 'textDocument/hover', {}],
			['definition', 'textDocument/definition', {}],
			['completion', 'textDocument/completion', { context: { triggerKind: 1 } }],
			['signatureHelp', 'textDocument/signatureHelp', { context: { triggerKind: 1, isRetrigger: false } }],
		]) {
			results.push({
				label: label,
				values: stats(await measureRequest(client, method, largestFile, positions, extraParams)),
			});
		}
		const symbolDurations = [];
		for (let run = 0; run < requestRunCount; run++) {
			const start = performance.now();
			await client.request('textDocument/documentSymbol', {
				textDocument: { uri: pathToFileURL(largestFile).href },
			});
			symbolDurations.push(performance.now() - start);
		}
		results.push({ label: 'documentSymbol', values: stats(symbolDurations) });
		const tokenDurations = [];
		for (let run = 0; run < requestRunCount; run++) {
			const start = performance.now();
			await client.request('textDocument/semanticTokens/full', {
				textDocument: { uri: pathToFileURL(largestFile).href },
			});
			tokenDurations.push(performance.now() - start);
		}
		results.push({ label: 'semanticTokens', values: stats(tokenDurations) });
	}
	finally {
		client.stop();
	}

	const target = basename(targets[0]);
	const machine = getMachine();
	const deps = `jul-compiler ${getCommit(compilerFolder)}`;
	const previous = readPrevious(logPath, target, machine);
	const header = `${julFiles.length} Dateien, größte ${basename(largestFile)} mit ${rows.length} Zeilen,`
		+ ` ${positions.length} Positionen, ${requestRunCount} Durchläufe`;
	console.log(header);
	console.log(`gemessen gegen ${deps}`);
	console.log(previous
		? `Vergleich mit dem letzten Eintrag für ${target} auf ${machine}, damals ${previous[results[0].label]?.deps ?? '-'}`
		: `keine frühere Messung für ${target} auf ${machine} in ${logPath}`);
	results.forEach(({ label, values }) =>
		console.log(formatResult(label, values, previous?.[label])));

	results.forEach(({ label, values }) => {
		const drift = getDrift(logPath, target, machine, label, values);
		if (drift && Math.abs(drift.deviation) >= noticeableDeviation) {
			const sign = drift.deviation >= 0 ? '+' : '';
			console.log(`  seit ${drift.first.timestamp} (${drift.first.commit}, ${drift.first.deps}) ${label}:`
				+ ` ${drift.first.median.toFixed(2)} ms -> ${values.median.toFixed(2)} ms`
				+ ` (${sign}${(drift.deviation * 100).toFixed(0)}%)`);
		}
	});

	const alarming = previous
		? results.filter(({ label, values }) =>
			previous[label]
			&& isComparable(values, previous[label])
			&& getDeviation(values, previous[label]) >= alarmingDeviation)
		: [];
	if (alarming.length) {
		console.log(`\nALARM: ${alarming.map(result => result.label).join(', ')}`
			+ ` \u00fcber ${(alarmingDeviation * 100).toFixed(0)}% langsamer als die letzte Messung.`
			+ ' Wall-Clock schwankt, aber nicht so weit - vor dem Protokollieren pr\u00fcfen.');
	}
	if (save) {
		appendEntries(logPath, results, target, note, deps);
		console.log(`\nprotokolliert: ${logPath}`);
		execFileSync(process.execPath, [chartScript], { stdio: 'inherit' });
	}
	else {
		console.log('\nzum Protokollieren: npm run bench -- --save --note "grund"');
	}
}

main();
