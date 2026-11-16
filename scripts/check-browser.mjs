/**
 * Prüft, dass die Sprachfeatures im Browser laufen (src/browser.ts, Playground der Homepage). Läuft
 * nach dem Build auf out/browser.js, zusammen mit dem Browser-Einstieg des Compilers.
 *
 * 1. webpack bündelt beide Einstiege für target 'web'. Webpack 5 hat keine Polyfills für
 *    Node-Module, ein Import von path, fs & Co. bricht ab (Pfade über path-util.ts des Compilers).
 * 2. Das Bündel läuft in einem leeren VM-Kontext ohne process, Buffer und require, prüft ein kleines
 *    Projekt aus zwei Dateien und fragt Hover, Vervollständigung, Gehe zu Definition und
 *    Referenzen ab.
 */
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from 'fs';
import { tmpdir } from 'os';
import { dirname, join } from 'path';
import { fileURLToPath } from 'url';
import { createContext, runInContext } from 'vm';
import webpack from 'webpack';

const serverDirectory = join(dirname(fileURLToPath(import.meta.url)), '..');
const compilerDirectory = join(serverDirectory, '..', 'jul-compiler');
const workDirectory = mkdtempSync(join(tmpdir(), 'jul-lsp-browser-check-'));

const probeFiles = {
	'main.jul': '(double) = import(§./lib/math.jul§)\nanswer = double(21)\n',
	'lib/math.jul': 'double = (x: Integer) => x.multiply(2)\n',
};

try {
	const code = await bundle();
	if (code !== undefined) {
		await runProbe(code);
	}
}
finally {
	rmSync(workDirectory, { recursive: true, force: true });
}

/**
 * @returns der gebündelte Code, undefined bei Fehlern (schon ausgegeben)
 */
async function bundle() {
	const entry = join(workDirectory, 'entry.js');
	writeFileSync(entry, [
		`export * as compiler from 'jul-compiler/out/browser.js';`,
		`export * as languageService from ${JSON.stringify(join(serverDirectory, 'out', 'browser.js'))};`,
	].join('\n'));
	const outputDirectory = join(workDirectory, 'bundle');
	const stats = await new Promise((resolve, reject) => {
		webpack({
			mode: 'production',
			target: ['web', 'es2020'],
			entry: entry,
			output: {
				path: outputDirectory,
				filename: 'bundle.js',
				library: { type: 'var', name: 'bundled' },
			},
			// Derselbe Compiler für beide Einstiege, wie im Playground.
			resolve: { alias: { 'jul-compiler': compilerDirectory } },
			externalsType: 'import',
			externals: { typescript: 'typescript' },
			performance: { hints: false },
		}, (error, stats) => error ? reject(error) : resolve(stats));
	});
	if (stats.hasErrors()) {
		console.error('Browser-Bündel des Language Servers fehlgeschlagen (Node-Modul im Language Server?)');
		stats.toJson({ all: false, errors: true }).errors.forEach(error => {
			console.error(`  ${error.moduleName ?? ''}  ${error.message.split('\n')[0]}`);
		});
		process.exitCode = 1;
		return undefined;
	}
	return readFileSync(join(outputDirectory, 'bundle.js'), 'utf8');
}

async function runProbe(code) {
	// Nur, was auch ein Browser hat.
	const context = createContext({
		console: console,
		performance: performance,
		URL: URL,
		TextEncoder: TextEncoder,
		TextDecoder: TextDecoder,
		setTimeout: setTimeout,
		clearTimeout: clearTimeout,
		queueMicrotask: queueMicrotask,
		structuredClone: structuredClone,
	});
	runInContext(code, context);
	const { compiler, languageService } = context.bundled;
	compiler.initCompiler(readFileSync(join(compilerDirectory, 'out', 'runtime', 'core-lib.jul'), 'utf8'));
	const referenceIndex = new compiler.ReferenceIndex();
	const { documents, errors } = await compiler.check(probeFiles, { referenceIndex: referenceIndex });
	const errorCount = Object.values(errors).flat().length;
	if (errorCount) {
		fail('Probe hat Fehler:', JSON.stringify(errors, undefined, 2));
		return;
	}
	const workspace = { documents, referenceIndex };

	// main.jul Zeile 2: answer = double(21), double ab Spalte 9
	const hover = languageService.hover(workspace, 'main.jul', 1, 10)?.value ?? '';
	if (!hover.includes('Integer')) {
		fail('Hover auf double ohne Typ:', hover);
	}
	const completion = languageService.completion(workspace, 'main.jul', 1, 2).map(item => item.label);
	if (!completion.includes('answer')) {
		fail('Vervollständigung ohne answer:', completion.join(' '));
	}
	const definition = languageService.definition(workspace, 'main.jul', 1, 10);
	if (definition.length !== 1 || definition[0].filePath !== 'lib/math.jul') {
		fail('Gehe zu Definition landet nicht in lib/math.jul:', JSON.stringify(definition));
	}
	const references = languageService.references(workspace, 'main.jul', 1, 10, true)
		.map(location => location.filePath)
		.sort();
	if (references.join(' ') !== 'lib/math.jul main.jul main.jul') {
		fail('Referenzen auf double:', references.join(' '));
	}
	if (process.exitCode) {
		return;
	}
	const sizeKb = (Buffer.byteLength(code) / 1024).toFixed(0);
	console.log(`Browser-Bündel ok, Compiler und Language Server ${sizeKb} kB minifiziert ohne TypeScript, Hover, Vervollständigung, Definition und Referenzen geprüft`);
}

function fail(...messages) {
	messages.forEach(message => console.error(message));
	process.exitCode = 1;
}
