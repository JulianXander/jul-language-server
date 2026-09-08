import { fork } from 'child_process';
import { existsSync, readdirSync, readFileSync, statSync } from 'fs';
import { basename, join, resolve } from 'path';
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

/**
 * Wall-Clock Messung der Server-Latenzen über echtes LSP. Kein Test-Gate, nur Beleg für
 * Umbauten am Server. Misst, was der Editor merkt: Zeit bis Diagnostics und Antwortzeit der
 * positionsbasierten Features.
 * Aufruf: npm run bench [--save] [--note "grund"] [datei|ordner...]  (Default: jul-examples)
 * Mit --save wird die Messung an scripts/bench-log.tsv angehängt, ohne nur verglichen.
 * Setzt einen gebauten Server voraus (npm run build).
 */

const requestRunCount = 3;
const maxPositionCount = 200;
const requestTimeout = 30000;
// Der Server überspringt größere Dateien, sie würden nie Diagnostics senden
const maxFileSize = 100000;

const serverPath = resolve(import.meta.dirname, '../out/server.js');
const logPath = resolve(import.meta.dirname, 'bench-log.tsv');
// gemessen wird größtenteils Compiler-Code, der eigene Commit erklärt die Zahlen allein nicht
const compilerFolder = resolve(import.meta.dirname, '../../jul-compiler');

//#region lsp client

/** Minimaler LSP-Client über Node-IPC, nur so viel wie der Bench braucht. */
function startServer() {
	const child = fork(serverPath, ['--node-ipc'], { execArgv: [] });
	const pendingRequests = new Map();
	/** uri -> resolve, wird von publishDiagnostics bedient */
	const pendingDiagnostics = new Map();
	let nextId = 1;

	child.on('message', (message) => {
		if (message.id !== undefined && pendingRequests.has(message.id)) {
			const resolveRequest = pendingRequests.get(message.id);
			pendingRequests.delete(message.id);
			resolveRequest(message.result);
			return;
		}
		if (message.method === 'textDocument/publishDiagnostics') {
			const uri = message.params.uri;
			const resolveDiagnostics = pendingDiagnostics.get(uri);
			if (resolveDiagnostics) {
				pendingDiagnostics.delete(uri);
				resolveDiagnostics(message.params.diagnostics);
			}
		}
	});

	function notify(method, params) {
		child.send({ jsonrpc: '2.0', method: method, params: params });
	}

	function request(method, params) {
		const id = nextId++;
		return new Promise((resolveRequest, rejectRequest) => {
			const timeout = setTimeout(() => {
				pendingRequests.delete(id);
				rejectRequest(new Error(`timeout bei ${method}`));
			}, requestTimeout);
			pendingRequests.set(id, (result) => {
				clearTimeout(timeout);
				resolveRequest(result);
			});
			child.send({ jsonrpc: '2.0', id: id, method: method, params: params });
		});
	}

	/** muss vor der auslösenden Notification aufgerufen werden, sonst geht die Antwort verloren */
	function waitForDiagnostics(uri) {
		return new Promise((resolveDiagnostics, rejectDiagnostics) => {
			const timeout = setTimeout(() => {
				pendingDiagnostics.delete(uri);
				rejectDiagnostics(new Error(`timeout bei diagnostics für ${uri}`));
			}, requestTimeout);
			pendingDiagnostics.set(uri, (diagnostics) => {
				clearTimeout(timeout);
				resolveDiagnostics(diagnostics);
			});
		});
	}

	return {
		notify: notify,
		request: request,
		waitForDiagnostics: waitForDiagnostics,
		stop: () => child.kill(),
	};
}

async function initialize(client, rootPath) {
	await client.request('initialize', {
		processId: process.pid,
		rootUri: pathToFileURL(rootPath).href,
		capabilities: {
			textDocument: {
				completion: { completionItem: { snippetSupport: false } },
				hover: { contentFormat: ['markdown', 'plaintext'] },
				publishDiagnostics: { relatedInformation: true },
				synchronization: { dynamicRegistration: false },
			},
		},
		workspaceFolders: [{ uri: pathToFileURL(rootPath).href, name: 'bench' }],
	});
	client.notify('initialized', {});
}

//#endregion lsp client

//#region messung

function findJulFiles(target) {
	if (!existsSync(target)) {
		return [];
	}
	if (!statSync(target).isDirectory()) {
		return target.endsWith('.jul') ? [target] : [];
	}
	return readdirSync(target).flatMap(entry => {
		if (entry === 'out' || entry === 'node_modules' || entry === '.git') {
			return [];
		}
		return findJulFiles(join(target, entry));
	});
}

/** verteilt Positionen über alle nicht leeren Zeilen, jeweils in der Mitte des Zeileninhalts */
function collectPositions(rows) {
	const positions = [];
	rows.forEach((row, rowIndex) => {
		const trimmed = row.trim();
		if (!trimmed || trimmed.startsWith('#')) {
			return;
		}
		positions.push({ line: rowIndex, character: Math.floor(row.length / 2) });
	});
	const step = Math.max(1, Math.ceil(positions.length / maxPositionCount));
	return positions.filter((_position, index) => index % step === 0);
}

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
		: [resolve(import.meta.dirname, '../../jul-examples')];
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
	const positions = collectPositions(rows);

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
	}
	else {
		console.log('\nzum Protokollieren: npm run bench -- --save --note "grund"');
	}
}

main();
