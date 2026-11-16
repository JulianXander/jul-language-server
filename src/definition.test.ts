import { expect } from 'chai';
import { join, resolve } from 'path';
import { ParsedDocuments } from 'jul-compiler/out/checker/checker.js';
import { ReferenceIndex } from 'jul-compiler/out/checker/reference-index.js';
import { createInMemoryHost, loadFile } from 'jul-compiler/out/compiler/project-loader.js';
import { getDefinitionLocations } from './definition.js';

const folder = resolve('/definition-test');
const mainPath = join(folder, 'main.jul');
const mathPath = join(folder, 'math.jul');
const cursor = '¦';

/**
 * Die Ziele von Gehe zu Definition an der Stelle von ¦ in main.jul, als Dateiname (core-lib bei
 * einem Builtin) und Zeile:Spalte, 1-basiert.
 */
function definitionAt(mainCodeWithCursor: string, otherFiles: { [filePath: string]: string; } = {}): string[] {
	const rows = mainCodeWithCursor.split('\n');
	const rowIndex = rows.findIndex(row => row.includes(cursor));
	if (rowIndex === -1) {
		throw new Error(`${cursor} fehlt im Code`);
	}
	const columnIndex = rows[rowIndex]!.indexOf(cursor);
	const documents: ParsedDocuments = {};
	const referenceIndex = new ReferenceIndex();
	const host = createInMemoryHost(
		{ ...otherFiles, [mainPath]: mainCodeWithCursor.replace(cursor, '') },
		{ referenceIndex: referenceIndex });
	const parsed = loadFile(mainPath, documents, host);
	if (typeof parsed === 'string') {
		throw new Error(parsed);
	}
	return getDefinitionLocations(parsed, rowIndex, columnIndex, documents, referenceIndex).map(location =>
		`${location.filePath === undefined ? 'core-lib' : location.filePath.slice(folder.length + 1)} ${location.startRowIndex + 1}:${location.startColumnIndex + 1}`);
}

describe('getDefinitionLocations', () => {
	it('eine Referenz springt zu ihrer Definition', () => {
		const code = `a = 1
b = ¦a`;
		expect(definitionAt(code)).to.deep.equal(['main.jul 1:1']);
	});

	it('ein importierter Name springt durch den Import zur Definition in der anderen Datei', () => {
		const code = `(double) = import(§./math.jul§)
x = ¦double(2)`;
		const math = `
double = (x: Integer) => x.multiply(2)`;
		expect(definitionAt(code, { [mathPath]: math })).to.deep.equal(['math.jul 2:1']);
	});

	it('der Importpfad springt an den Anfang der importierten Datei', () => {
		const code = `(double) = import(§./ma¦th.jul§)`;
		const math = `double = (x: Integer) => x.multiply(2)`;
		expect(definitionAt(code, { [mathPath]: math })).to.deep.equal(['math.jul 1:1']);
	});

	it('ein Builtin springt in die core-lib', () => {
		const code = `x = ¦log(1)`;
		expect(definitionAt(code)).to.have.length(1);
		expect(definitionAt(code)[0]).to.match(/^core-lib /);
	});

	it('ein Feldzugriff auf ein Literal mit erwartetem Typ springt zum Feld des Typs', () => {
		const code = `MyType = [
	name: Text
]
a: MyType = [name = §a§]
e = a/na¦me`;
		expect(definitionAt(code)).to.deep.equal(['main.jul 2:2']);
	});

	it('ohne Symbol an der Stelle gibt es kein Ziel', () => {
		const code = `a = ¦1`;
		expect(definitionAt(code)).to.deep.equal([]);
	});
});
