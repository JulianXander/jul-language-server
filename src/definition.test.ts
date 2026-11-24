import { expect } from 'chai';
import { readFileSync } from 'fs';
import { join, resolve } from 'path';
import { ParsedDocuments } from 'jul-compiler/out/checker/checker.js';
import { ReferenceIndex } from 'jul-compiler/out/checker/reference-index.js';
import { coreLibPath } from 'jul-compiler/out/compiler/node-host.js';
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

/**
 * Die Stelle des Felds message in der Deklaration von Error in der core-lib, im Format von
 * definitionAt. Aus der Datei gelesen, damit der Test nicht an einer Zeilennummer hängt.
 */
function getErrorMessageFieldLocation(): string {
	const rows = readFileSync(coreLibPath, 'utf8').split('\n');
	const errorRowIndex = rows.findIndex(row => row.startsWith('Error = native('));
	const messageRowIndex = rows.findIndex((row, rowIndex) => rowIndex > errorRowIndex && row.trim().startsWith('message:'));
	const messageRow = rows[messageRowIndex] ?? '';
	return `core-lib ${messageRowIndex + 1}:${messageRow.indexOf('message') + 1}`;
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

	it('message an einem Error springt zum Feld in der Deklaration von Error in der core-lib', () => {
		const code = `e = error(§kaputt§)
m = e/mes¦sage`;
		expect(definitionAt(code)).to.deep.equal([getErrorMessageFieldLocation()]);
	});

	// Records sind geschlossen: message ist am Record Empty, die Deklaration liefert nur Error.
	it('message an einem Record oder Error springt zum Feld von Error', () => {
		const code = `Result = [value: Integer]
getResult = (flag: Boolean) :> Or(Result Error) =>
	?(flag)
		[true] => error(§kaputt§)
	[value = 1]
m = getResult(true)/mes¦sage`;
		expect(definitionAt(code)).to.deep.equal([getErrorMessageFieldLocation()]);
	});

	it('ohne Symbol an der Stelle gibt es kein Ziel', () => {
		const code = `a = ¦1`;
		expect(definitionAt(code)).to.deep.equal([]);
	});
});
