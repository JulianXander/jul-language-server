import { expect } from 'chai';
import { join, resolve } from 'path';
import { checkTypes, ParsedDocuments } from 'jul-compiler/out/checker/checker.js';
import { ReferenceIndex } from 'jul-compiler/out/checker/reference-index.js';
import { errorInfos } from 'jul-compiler/out/compiler-errors.js';
import { parseCode } from 'jul-compiler/out/parser/parser.js';
import { ParsedFile } from 'jul-compiler/out/syntax-tree.js';
import { reportAtCaller } from 'jul-compiler/src/test-util.js';
import { TextEdit } from 'vscode-languageserver';
import { createImportEdit, findImportCandidates, ImportCandidate } from './auto-import.js';

const folder = resolve('/auto-import-test');
const mainPath = join(folder, 'main.jul');

function parse(code: string, path: string): ParsedFile {
	const parsed = parseCode(code, path);
	const documents: ParsedDocuments = { [path]: parsed };
	checkTypes(parsed, documents, { cloneUnchecked: true, referenceIndex: new ReferenceIndex() });
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

describe('createImportEdit', () => {
	const expectImportEdit = reportAtCaller((code: string, { name, targetFileName, maxRowIndex, expected }: {
		name: string;
		/** Dateiname im selben Ordner */
		targetFileName: string;
		/** Zeile der Verwendung */
		maxRowIndex: number;
		expected: string;
	}) => {
		const parsedFile = parse(code, mainPath);
		const candidate: ImportCandidate = {
			filePath: join(folder, targetFileName),
			importPath: './' + targetFileName,
		};
		const textEdit = createImportEdit(parsedFile, candidate, name, maxRowIndex);
		expect(textEdit).to.not.equal(undefined);
		const actual = applyTextEdit(code, textEdit!);
		expect(actual).to.equal(expected);
		// das Ergebnis muss wieder lesbar sein - ein reiner Textvergleich übersieht Zeilen-/Einrückungsfehler
		const syntaxErrors = parseCode(actual, mainPath).unchecked.errors
			.filter(error => errorInfos[error.code].type === 'syntax');
		expect(syntaxErrors).to.deep.equal([]);
	});

	it('legt bei einer Datei ohne Import eine neue Definition oben an', () => {
		expectImportEdit('x = cardEffects\n', {
			name: 'cardEffects',
			targetFileName: 'card-effects.jul',
			maxRowIndex: 0,
			expected: `(cardEffects) = import(§./card-effects.jul§)
x = cardEffects
`,
		});
	});

	it('lässt einen führenden Kommentar oben stehen', () => {
		expectImportEdit(`# Kommentar
x = cardEffects
`, {
			name: 'cardEffects',
			targetFileName: 'card-effects.jul',
			maxRowIndex: 1,
			expected: `# Kommentar
(cardEffects) = import(§./card-effects.jul§)
x = cardEffects
`,
		});
	});

	it('sortiert in ein einzeiliges Destructuring derselben Datei inline ein', () => {
		expectImportEdit(`(cardEffects) = import(§./card-effects.jul§)
x = attackPoints
`, {
			name: 'attackPoints',
			targetFileName: 'card-effects.jul',
			maxRowIndex: 1,
			expected: `(attackPoints cardEffects) = import(§./card-effects.jul§)
x = attackPoints
`,
		});
	});

	it('hängt an ein einzeiliges Destructuring hinten an', () => {
		expectImportEdit(`(cardEffects) = import(§./card-effects.jul§)
x = zebra
`, {
			name: 'zebra',
			targetFileName: 'card-effects.jul',
			maxRowIndex: 1,
			expected: `(cardEffects zebra) = import(§./card-effects.jul§)
x = zebra
`,
		});
	});

	it('sortiert in ein mehrzeiliges Destructuring als eigene Zeile ein', () => {
		expectImportEdit(`(
	cardEffects
	zebra
) = import(§./card-effects.jul§)
x = monster
`, {
			name: 'monster',
			targetFileName: 'card-effects.jul',
			maxRowIndex: 4,
			expected: `(
	cardEffects
	monster
	zebra
) = import(§./card-effects.jul§)
x = monster
`,
		});
	});

	it('hängt an ein mehrzeiliges Destructuring hinten an', () => {
		expectImportEdit(`(
	cardEffects
	zebra
) = import(§./card-effects.jul§)
x = zzz
`, {
			name: 'zzz',
			targetFileName: 'card-effects.jul',
			maxRowIndex: 4,
			expected: `(
	cardEffects
	zebra
	zzz
) = import(§./card-effects.jul§)
x = zzz
`,
		});
	});

	it('sortiert einen neuen Import alphabetisch zwischen bestehende ein', () => {
		expectImportEdit(`(a) = import(§./aaa.jul§)
(z) = import(§./zzz.jul§)
x = mid
`, {
			name: 'mid',
			targetFileName: 'mmm.jul',
			maxRowIndex: 2,
			expected: `(a) = import(§./aaa.jul§)
(mid) = import(§./mmm.jul§)
(z) = import(§./zzz.jul§)
x = mid
`,
		});
	});

	it('hängt einen neuen Import hinter den letzten bestehenden', () => {
		expectImportEdit(`(a) = import(§./aaa.jul§)
(z) = import(§./zzz.jul§)
x = last
`, {
			name: 'last',
			targetFileName: 'zzz2.jul',
			maxRowIndex: 2,
			expected: `(a) = import(§./aaa.jul§)
(z) = import(§./zzz.jul§)
(last) = import(§./zzz2.jul§)
x = last
`,
		});
	});

	it('ignoriert Importe unterhalb der Verwendung', () => {
		expectImportEdit(`x = mid
(z) = import(§./zzz.jul§)
`, {
			name: 'mid',
			targetFileName: 'mmm.jul',
			maxRowIndex: 0,
			expected: `(mid) = import(§./mmm.jul§)
x = mid
(z) = import(§./zzz.jul§)
`,
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
			checkTypes(documents[path]!, documents, { cloneUnchecked: true, referenceIndex: referenceIndex });
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

	it('schlägt nur die definierende Datei vor, nicht eine importierende', () => {
		const documents = parseAll({
			'main.jul': 'x = cardEffects\n',
			'source.jul': 'cardEffects = 1\n',
			'reexport.jul': '(cardEffects) = import(§./source.jul§)\n',
		});
		const candidates = findImportCandidates('cardEffects', mainPath, documents);
		expect(candidates.map(candidate => candidate.importPath)).to.deep.equal([
			'./source.jul',
		]);
	});

	it('schlägt einen Alias-Import nicht vor, Importe werden nicht weiterexportiert', () => {
		const documents = parseAll({
			'main.jul': 'x = effects\n',
			'source.jul': 'cardEffects = 1\n',
			'reexport.jul': '(effects = cardEffects) = import(§./source.jul§)\n',
		});
		expect(findImportCandidates('effects', mainPath, documents)).to.deep.equal([]);
	});
});
