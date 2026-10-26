import { expect } from 'chai';
import { TextEdit } from 'vscode-languageserver';
import { TextDocument } from 'vscode-languageserver-textdocument';
import { getDocumentFormattingEdits, getOnTypeFormattingEdits } from './formatting.js';

const filePath = 'C:/formatting-test/main.jul';

function apply(code: string, edits: TextEdit[]): string {
	return TextDocument.applyEdits(TextDocument.create('file:///main.jul', 'jul', 1, code), edits);
}

/**
 * code enthält den getippten Umbruch, | markiert die Cursorposition danach.
 */
function afterEnter(codeWithCursor: string): string {
	const offset = codeWithCursor.indexOf('|');
	const code = codeWithCursor.replace('|', '');
	const before = code.substring(0, offset).split('\n');
	const position = { line: before.length - 1, character: before[before.length - 1]!.length };
	return apply(code, getOnTypeFormattingEdits(code, filePath, position));
}

describe('formatting', () => {
	//#region Dokument formatieren
	it('bricht eine Liste mit Umbruch hinter der Klammer um', () => {
		const code = 'x = [\n\t1 2\n]';
		expect(apply(code, getDocumentFormattingEdits(code, filePath))).to.equal('x = [\n\t1\n\t2\n]');
	});
	it('zieht eine Liste ohne Umbruch hinter der Klammer zusammen', () => {
		const code = 'x = [1\n\t2\n]';
		expect(apply(code, getDocumentFormattingEdits(code, filePath))).to.equal('x = [1 2]');
	});
	it('keine Edits für formatierten Code', () => {
		expect(getDocumentFormattingEdits('x = [1 2]', filePath)).to.deep.equal([]);
	});
	//#endregion Dokument formatieren

	//#region Enter
	it('Enter hinter der öffnenden Klammer', () => {
		expect(afterEnter('x = f(\n\t|a b c)')).to.equal('x = f(\n\ta\n\tb\n\tc\n)');
	});
	it('Enter zwischen zwei Feldern ohne tiefere Einrückung', () => {
		expect(afterEnter('x = f(a b\n|c)')).to.equal('x = f(\n\ta\n\tb\n\tc\n)');
	});
	it('Enter zwischen zwei Feldern mit Leerzeichen am Zeilenende', () => {
		expect(afterEnter('x = f(a \n\t|b)')).to.equal('x = f(\n\ta\n\tb\n)');
	});
	it('Enter in einer eingerückten Zeile', () => {
		expect(afterEnter('g = () =>\n\tx = f(a\n\t|b)\n\tx')).to.equal('g = () =>\n\tx = f(\n\t\ta\n\t\tb\n\t)\n\tx');
	});
	it('Enter in einer inneren Liste bricht nur diese um', () => {
		expect(afterEnter('x = [1 [2\n|3] 4]')).to.equal('x = [1 [\n\t2\n\t3\n] 4]');
	});
	it('Enter in einer neuen leeren Zeile ändert nichts', () => {
		expect(afterEnter('x = [\n\t1\n\t|\n]')).to.equal('x = [\n\t1\n\t\n]');
	});
	it('Enter außerhalb einer Liste ändert nichts', () => {
		expect(afterEnter('x = 1\n|y = 2')).to.equal('x = 1\ny = 2');
	});
	it('Enter mitten in einem Text ändert nichts', () => {
		expect(afterEnter('x = f(§ab\n|c§ d)')).to.equal('x = f(§ab\nc§ d)');
	});
	//#endregion Enter
});
