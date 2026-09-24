import { expect } from 'chai';
import { mkdtempSync, rmSync, writeFileSync } from 'fs';
import { tmpdir } from 'os';
import { join, resolve } from 'path';
import { checkTypes, ParsedDocuments } from 'jul-compiler/out/checker/checker.js';
import { ReferenceIndex } from 'jul-compiler/out/checker/reference-index.js';
import { errorInfos } from 'jul-compiler/out/compiler-errors.js';
import { parseCode } from 'jul-compiler/out/parser/parser.js';
import { ParsedFile } from 'jul-compiler/out/syntax-tree.js';
import { TextEdit } from 'vscode-languageserver';
import { createImportEdit, findImportCandidates, ImportCandidate } from './auto-import.js';

const folder = resolve('/auto-import-test');
const mainPath = join(folder, 'main.jul');

function parse(code: string, path: string): ParsedFile {
	const parsed = parseCode(code, path);
	const documents: ParsedDocuments = { [path]: parsed };
	checkTypes(parsed, documents, new ReferenceIndex());
	return parsed;
}

function applyTextEdit(code: string, edit: TextEdit): string {
	const lines = code.split('\n');
	function getOffset(line: number, character: number): number {
		let offset = 0;
		for (let index = 0; index < line; index++) {
			offset += lines[index]!.length + 1;
		}
		return offset + character;
	}
	const start = getOffset(edit.range.start.line, edit.range.start.character);
	const end = getOffset(edit.range.end.line, edit.range.end.character);
	return code.slice(0, start) + edit.newText + code.slice(end);
}

interface ImportEditTestCase {
	title: string;
	code: string;
	name: string;
	/** Dateiname im selben Ordner */
	targetFileName: string;
	/** Zeile der Verwendung */
	maxRowIndex: number;
	expected: string;
}

const importEditTestCases: ImportEditTestCase[] = [
	{
		title: 'legt bei einer Datei ohne Import eine neue Definition oben an',
		code: 'x = cardEffects\n',
		name: 'cardEffects',
		targetFileName: 'card-effects.jul',
		maxRowIndex: 0,
		expected: '(cardEffects) = import(§./card-effects.jul§)\nx = cardEffects\n',
	},
	{
		title: 'lässt einen führenden Kommentar oben stehen',
		code: '# Kommentar\nx = cardEffects\n',
		name: 'cardEffects',
		targetFileName: 'card-effects.jul',
		maxRowIndex: 1,
		expected: '# Kommentar\n(cardEffects) = import(§./card-effects.jul§)\nx = cardEffects\n',
	},
	{
		title: 'sortiert in ein einzeiliges Destructuring derselben Datei inline ein',
		code: '(cardEffects) = import(§./card-effects.jul§)\nx = attackPoints\n',
		name: 'attackPoints',
		targetFileName: 'card-effects.jul',
		maxRowIndex: 1,
		expected: '(attackPoints cardEffects) = import(§./card-effects.jul§)\nx = attackPoints\n',
	},
	{
		title: 'hängt an ein einzeiliges Destructuring hinten an',
		code: '(cardEffects) = import(§./card-effects.jul§)\nx = zebra\n',
		name: 'zebra',
		targetFileName: 'card-effects.jul',
		maxRowIndex: 1,
		expected: '(cardEffects zebra) = import(§./card-effects.jul§)\nx = zebra\n',
	},
	{
		title: 'sortiert in ein mehrzeiliges Destructuring als eigene Zeile ein',
		code: '(\n\tcardEffects\n\tzebra\n) = import(§./card-effects.jul§)\nx = monster\n',
		name: 'monster',
		targetFileName: 'card-effects.jul',
		maxRowIndex: 4,
		expected: '(\n\tcardEffects\n\tmonster\n\tzebra\n) = import(§./card-effects.jul§)\nx = monster\n',
	},
	{
		title: 'hängt an ein mehrzeiliges Destructuring hinten an',
		code: '(\n\tcardEffects\n\tzebra\n) = import(§./card-effects.jul§)\nx = zzz\n',
		name: 'zzz',
		targetFileName: 'card-effects.jul',
		maxRowIndex: 4,
		expected: '(\n\tcardEffects\n\tzebra\n\tzzz\n) = import(§./card-effects.jul§)\nx = zzz\n',
	},
	{
		title: 'sortiert einen neuen Import alphabetisch zwischen bestehende ein',
		code: '(a) = import(§./aaa.jul§)\n(z) = import(§./zzz.jul§)\nx = mid\n',
		name: 'mid',
		targetFileName: 'mmm.jul',
		maxRowIndex: 2,
		expected: '(a) = import(§./aaa.jul§)\n(mid) = import(§./mmm.jul§)\n(z) = import(§./zzz.jul§)\nx = mid\n',
	},
	{
		title: 'hängt einen neuen Import hinter den letzten bestehenden',
		code: '(a) = import(§./aaa.jul§)\n(z) = import(§./zzz.jul§)\nx = last\n',
		name: 'last',
		targetFileName: 'zzz2.jul',
		maxRowIndex: 2,
		expected: '(a) = import(§./aaa.jul§)\n(z) = import(§./zzz.jul§)\n(last) = import(§./zzz2.jul§)\nx = last\n',
	},
	{
		title: 'ignoriert Importe unterhalb der Verwendung',
		code: 'x = mid\n(z) = import(§./zzz.jul§)\n',
		name: 'mid',
		targetFileName: 'mmm.jul',
		maxRowIndex: 0,
		expected: '(mid) = import(§./mmm.jul§)\nx = mid\n(z) = import(§./zzz.jul§)\n',
	},
];

describe('createImportEdit', () => {
	importEditTestCases.forEach(testCase => {
		it(testCase.title, () => {
			const parsedFile = parse(testCase.code, mainPath);
			const candidate: ImportCandidate = {
				filePath: join(folder, testCase.targetFileName),
				importPath: './' + testCase.targetFileName,
			};
			const textEdit = createImportEdit(parsedFile, candidate, testCase.name, testCase.maxRowIndex);
			expect(textEdit).to.not.equal(undefined);
			const actual = applyTextEdit(testCase.code, textEdit!);
			expect(actual).to.equal(testCase.expected);
			// das Ergebnis muss wieder lesbar sein - ein reiner Textvergleich übersieht Zeilen-/Einrückungsfehler
			const syntaxErrors = parseCode(actual, mainPath).unchecked.errors
				.filter(error => errorInfos[error.code].type === 'syntax');
			expect(syntaxErrors).to.deep.equal([]);
		});
	});
});

describe('findImportCandidates', () => {
	function parseAll(files: { [fileName: string]: string; }): ParsedDocuments {
		const documents: ParsedDocuments = {};
		for (const fileName in files) {
			const path = join(folder, fileName);
			documents[path] = parseCode(files[fileName]!, path);
		}
		const referenceIndex = new ReferenceIndex();
		for (const path in documents) {
			checkTypes(documents[path]!, documents, referenceIndex);
		}
		return documents;
	}

	it('findet Top-Level-Definitionen anderer Dateien, sortiert nach Pfad', () => {
		const documents = parseAll({
			'main.jul': 'x = cardEffects\n',
			'zzz.jul': 'cardEffects = 2\n',
			'aaa.jul': 'cardEffects = 1\n',
		});
		const candidates = findImportCandidates('cardEffects', mainPath, documents);
		expect(candidates.map(candidate => candidate.importPath)).to.deep.equal([
			'./aaa.jul',
			'./zzz.jul',
		]);
	});

	it('schlägt die eigene Datei nicht vor', () => {
		const documents = parseAll({ 'main.jul': 'cardEffects = 1\n' });
		expect(findImportCandidates('cardEffects', mainPath, documents)).to.deep.equal([]);
	});

	it('schlägt json und yaml nicht vor', () => {
		const documents = parseAll({
			'main.jul': 'x = cardEffects\n',
			'data.json': '{"cardEffects": 1}',
		});
		expect(findImportCandidates('cardEffects', mainPath, documents)).to.deep.equal([]);
	});

	// Echte Dateien statt der virtuellen parseAll()-Dokumente: der Checker löst die Importe dann
	// tatsächlich auf, so wie im Projekt.
	describe('mit importierenden Dateien auf der Platte', () => {
		let realFolder: string;
		let realMainPath: string;
		let documents: ParsedDocuments;

		beforeEach(() => {
			realFolder = mkdtempSync(join(tmpdir(), 'jul-auto-import-test-'));
			realMainPath = join(realFolder, 'main.jul');
		});

		afterEach(() => {
			rmSync(realFolder, { recursive: true, force: true });
		});

		function parseAndCheckAll(files: { [fileName: string]: string; }): ParsedDocuments {
			documents = {};
			const referenceIndex = new ReferenceIndex();
			for (const fileName in files) {
				const path = join(realFolder, fileName);
				writeFileSync(path, files[fileName]!);
				documents[path] = parseCode(files[fileName]!, path);
			}
			for (const path in documents) {
				checkTypes(documents[path]!, documents, referenceIndex);
			}
			return documents;
		}

		it('schlägt nur die definierende Datei vor, nicht eine importierende', () => {
			const docs = parseAndCheckAll({
				'main.jul': 'x = cardEffects\n',
				'source.jul': 'cardEffects = 1\n',
				'reexport.jul': '(cardEffects) = import(§./source.jul§)\n',
			});
			const candidates = findImportCandidates('cardEffects', realMainPath, docs);
			expect(candidates.map(candidate => candidate.importPath)).to.deep.equal([
				'./source.jul',
			]);
		});

		it('schlägt einen Alias-Import nicht vor, Importe werden nicht weiterexportiert', () => {
			const docs = parseAndCheckAll({
				'main.jul': 'x = effects\n',
				'source.jul': 'cardEffects = 1\n',
				'reexport.jul': '(effects = cardEffects) = import(§./source.jul§)\n',
			});
			expect(findImportCandidates('effects', realMainPath, docs)).to.deep.equal([]);
		});
	});
});
