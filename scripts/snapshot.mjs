import { existsSync, readFileSync, statSync, writeFileSync } from 'fs';
import { relative, resolve } from 'path';
import { fileURLToPath } from 'url';
import {
	collectPositions,
	findJulFiles,
	initialize,
	maxFileSize,
	openDocument,
	serverPath,
	startServer,
} from './lsp-client.mjs';

/**
 * Snapshot der Server-Antworten über echtes LSP. Sichert Umbauten an der Baumsuche ab:
 * der Bench sagt nur, dass es gleich schnell ist, dieser Test, dass es dasselbe findet.
 * Aufruf: npm test
 * Baseline neu schreiben: UPDATE_SNAPSHOT=1 npm test
 * Setzt einen gebauten Server voraus (npm run build).
 */

const maxPositionCount = 25;
const maxValueLength = 100;
const maxCompletionLabels = 3;

const target = resolve(import.meta.dirname, '../../jul-examples');
const baselinePath = resolve(import.meta.dirname, 'snapshot.baseline.txt');

//#region antworten normalisieren

/** einzeilig und gekürzt, der Snapshot soll Unterschiede zeigen und nicht Text spiegeln */
function shorten(value) {
	const singleLine = String(value).replace(/\s+/g, ' ').trim();
	return singleLine.length > maxValueLength
		? `${singleLine.slice(0, maxValueLength)}...`
		: singleLine;
}

function formatPosition(position) {
	return `${position.line + 1}:${position.character + 1}`;
}

function formatLocation(location) {
	// Verweise in die core-lib kommen mit dem virtuellen Schema jul-core-lib
	const path = location.uri.startsWith('file:')
		? relative(target, fileURLToPath(location.uri)).replaceAll('\\', '/')
		: location.uri;
	return `${path}:${formatPosition(location.range.start)}`;
}

function formatHover(hover) {
	if (!hover) {
		return 'kein';
	}
	const contents = hover.contents;
	return shorten(contents.value ?? contents);
}

function formatDefinition(definition) {
	if (!definition) {
		return 'kein';
	}
	const locations = Array.isArray(definition) ? definition : [definition];
	return locations.map(formatLocation).join(' ');
}

function formatCompletion(completion) {
	if (!completion) {
		return 'kein';
	}
	const items = Array.isArray(completion) ? completion : completion.items;
	const labels = items.slice(0, maxCompletionLabels).map(item => item.label);
	return `n=${items.length} ${labels.join(',')}`;
}

function formatSignatureHelp(signatureHelp) {
	if (!signatureHelp?.signatures?.length) {
		return 'kein';
	}
	const active = signatureHelp.signatures[signatureHelp.activeSignature ?? 0];
	return `${shorten(active.label)} param=${signatureHelp.activeParameter ?? '-'}`;
}

function formatSymbols(symbols, indent = '') {
	if (!symbols?.length) {
		return [`${indent}keine`];
	}
	return symbols.flatMap(symbol => [
		`${indent}${symbol.name} kind=${symbol.kind} ${formatPosition(symbol.range.start)}`,
		...formatSymbols(symbol.children, `${indent}  `),
	]);
}

//#endregion antworten normalisieren

/** ein fehlgeschlagener Request ist ein Befund und gehört in den Snapshot, nicht in einen Abbruch */
async function describe(client, method, params, format) {
	try {
		return format(await client.request(method, params));
	}
	catch (error) {
		return `FEHLER ${shorten(error.message)}`;
	}
}

/** absolute Pfade würden den Snapshot an diese Maschine binden */
function stripPaths(text) {
	return text
		.replaceAll(target, '<examples>')
		.replaceAll(target.replaceAll('\\', '/'), '<examples>');
}

async function collectSnapshot(client, filePaths) {
	const lines = [];
	for (const filePath of filePaths) {
		const relativePath = relative(target, filePath).replaceAll('\\', '/');
		// Diagnostics bleiben draußen: sie stehen schon im checker-snapshot des Compilers
		const { uri, text } = await openDocument(client, filePath);
		lines.push(`=== ${relativePath}`);
		lines.push('symbols');
		lines.push(await describe(
			client,
			'textDocument/documentSymbol',
			{ textDocument: { uri: uri } },
			symbols => formatSymbols(symbols, '  ').join('\n')));
		const positions = collectPositions(text.split('\n'), maxPositionCount);
		for (const position of positions) {
			const params = { textDocument: { uri: uri }, position: position };
			const label = formatPosition(position);
			lines.push(`  ${label} hover: `
				+ await describe(client, 'textDocument/hover', params, formatHover));
			lines.push(`  ${label} definition: `
				+ await describe(client, 'textDocument/definition', params, formatDefinition));
			lines.push(`  ${label} completion: `
				+ await describe(client, 'textDocument/completion',
					{ ...params, context: { triggerKind: 1 } }, formatCompletion));
			lines.push(`  ${label} signatureHelp: `
				+ await describe(client, 'textDocument/signatureHelp',
					{ ...params, context: { triggerKind: 1, isRetrigger: false } }, formatSignatureHelp));
		}
		lines.push('');
	}
	return stripPaths(lines.join('\n'));
}

/** meldet die erste abweichende Zeile, das reicht zum Einordnen und flutet die Ausgabe nicht */
function compareToBaseline(actual) {
	if (process.env['UPDATE_SNAPSHOT']) {
		writeFileSync(baselinePath, actual);
		console.log(`Baseline neu geschrieben: ${baselinePath}`);
		return true;
	}
	if (!existsSync(baselinePath)) {
		console.error('Baseline fehlt. Neu schreiben mit: UPDATE_SNAPSHOT=1 npm test');
		return false;
	}
	const expected = readFileSync(baselinePath, { encoding: 'utf8' });
	if (expected === actual) {
		console.log(`Snapshot unverändert (${actual.split('\n').length} Zeilen)`);
		return true;
	}
	const expectedRows = expected.split('\n');
	const actualRows = actual.split('\n');
	const differenceIndex = actualRows.findIndex((row, index) => row !== expectedRows[index]);
	console.error(`Snapshot weicht ab, zuerst in Zeile ${differenceIndex + 1}:`);
	console.error(`  erwartet: ${expectedRows[differenceIndex]}`);
	console.error(`  gemessen: ${actualRows[differenceIndex]}`);
	console.error(`  ${expectedRows.length} Zeilen erwartet, ${actualRows.length} gemessen`);
	console.error('Wenn die Änderung gewollt ist: UPDATE_SNAPSHOT=1 npm test');
	return false;
}

async function main() {
	if (!existsSync(serverPath)) {
		console.error(`${serverPath} fehlt, bitte zuerst npm run build`);
		process.exitCode = 1;
		return;
	}
	const julFiles = findJulFiles(target)
		.filter(filePath => statSync(filePath).size <= maxFileSize);
	if (!julFiles.length) {
		console.error(`keine .jul Dateien in ${target}`);
		process.exitCode = 1;
		return;
	}
	const client = startServer();
	let actual;
	try {
		await initialize(client, target);
		actual = await collectSnapshot(client, julFiles);
	}
	finally {
		client.stop();
	}
	if (!compareToBaseline(actual)) {
		process.exitCode = 1;
	}
}

main();
