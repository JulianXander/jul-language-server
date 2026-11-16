import { expect } from 'chai';
import { join, resolve } from 'path';
import { ParsedDocuments } from 'jul-compiler/out/checker/checker.js';
import { ReferenceIndex } from 'jul-compiler/out/checker/reference-index.js';
import { createInMemoryHost, loadFile } from 'jul-compiler/out/compiler/project-loader.js';
import { completion, documentHighlights, formatting, prepareRename, tests, Workspace } from './browser.js';

const folder = resolve('/browser-test');
const mainPath = join(folder, 'main.jul');
const mathPath = join(folder, 'math.jul');
const cursor = '¦';

/**
 * Lädt alle Dateien wie der Playground. In main.jul markiert ¦ die Stelle, das Zeichen selbst wird
 * vor dem Parsen entfernt.
 */
function load(mainCodeWithCursor: string, otherFiles: { [filePath: string]: string; } = {}): {
	workspace: Workspace;
	rowIndex: number;
	columnIndex: number;
} {
	const rows = mainCodeWithCursor.split('\n');
	const rowIndex = rows.findIndex(row => row.includes(cursor));
	if (rowIndex === -1) {
		throw new Error(`${cursor} fehlt im Code`);
	}
	const columnIndex = rows[rowIndex]!.indexOf(cursor);
	const files = { ...otherFiles, [mainPath]: mainCodeWithCursor.replace(cursor, '') };
	const documents: ParsedDocuments = {};
	const referenceIndex = new ReferenceIndex();
	const host = createInMemoryHost(files, { referenceIndex: referenceIndex });
	Object.keys(files).forEach(filePath => {
		if (!documents[filePath]) {
			loadFile(filePath, documents, host);
		}
	});
	return { workspace: { documents, referenceIndex }, rowIndex, columnIndex };
}

describe('browser', () => {
	describe('completion', () => {
		it('im Importpfad stehen die anderen Dateien, ohne die eigene und ohne Testdateien', () => {
			const { workspace, rowIndex, columnIndex } = load(`(double) = import(§./¦§)`, {
				[mathPath]: `double = (x: Integer) => x.multiply(2)`,
				[join(folder, 'data.yaml')]: `a: 1`,
				[join(folder, 'math.test.jul')]: ``,
			});
			const labels = completion(workspace, mainPath, rowIndex, columnIndex).map(item => item.label);
			expect(labels.sort()).to.deep.equal(['./data.yaml', './math.jul']);
		});

		it('außerhalb eines Importpfads wie im Language Server', () => {
			const { workspace, rowIndex, columnIndex } = load(`answer = 42
x = ans¦`);
			const labels = completion(workspace, mainPath, rowIndex, columnIndex).map(item => item.label);
			expect(labels).to.include('answer');
		});
	});

	describe('documentHighlights', () => {
		it('nur Vorkommen in der eigenen Datei', () => {
			const { workspace, rowIndex, columnIndex } = load(`(double) = import(§./math.jul§)
x = ¦double(2)`, {
				[mathPath]: `double = (x: Integer) => x.multiply(2)`,
			});
			const locations = documentHighlights(workspace, mainPath, rowIndex, columnIndex);
			expect(locations.map(location => `${location.startRowIndex + 1}:${location.startColumnIndex + 1}`).sort())
				.to.deep.equal(['1:2', '2:5']);
		});
	});

	describe('prepareRename', () => {
		it('ein Builtin lässt sich nicht umbenennen', () => {
			const { workspace, rowIndex, columnIndex } = load(`x = ¦log(1)`);
			expect(prepareRename(workspace, mainPath, rowIndex, columnIndex)).to.equal(undefined);
		});

		it('ein eigenes Symbol liefert die Stelle unter dem Cursor', () => {
			const { workspace, rowIndex, columnIndex } = load(`answer = 42
x = ans¦wer`);
			const range = prepareRename(workspace, mainPath, rowIndex, columnIndex);
			expect(range && `${range.startRowIndex + 1}:${range.startColumnIndex + 1}-${range.endColumnIndex + 1}`).to.equal('2:5-11');
		});
	});

	describe('tests', () => {
		it('die Tests einer Testdatei mit Namen und Stelle', () => {
			const testPath = join(folder, 'main.test.jul');
			const { workspace } = load(`¦`, {
				[testPath]: `x = 1
test(§eins§ () => equal(x 1))`,
			});
			expect(tests(workspace, testPath)).to.deep.equal([{
				name: 'eins',
				range: { start: { line: 1, character: 0 }, end: { line: 1, character: 29 } },
			}]);
		});

		it('keine in einer Datei, die keine Testdatei ist', () => {
			const { workspace } = load(`¦`);
			expect(tests(workspace, mainPath)).to.deep.equal([]);
		});
	});

	describe('formatting', () => {
		it('lässt andere Dateien als .jul unberührt', () => {
			expect(formatting(`a:   1`, join(folder, 'data.yaml'))).to.deep.equal([]);
		});
	});
});
