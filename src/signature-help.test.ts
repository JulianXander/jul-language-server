import { expect } from 'chai';
import { join, resolve } from 'path';
import { ParsedDocuments } from 'jul-compiler/out/checker/checker.js';
import { createInMemoryHost, loadFile } from 'jul-compiler/out/compiler/project-loader.js';
import { getSignatureHelp } from './signature-help.js';

const folder = resolve('/signature-help-test');
const filePath = join(folder, 'main.jul');
const cursor = '¦';

/** Signaturhilfe an der Stelle von ¦, das Zeichen selbst wird vor dem Parsen entfernt. */
function signatureHelpAt(codeWithCursor: string) {
	const rows = codeWithCursor.split('\n');
	const rowIndex = rows.findIndex(row => row.includes(cursor));
	if (rowIndex === -1) {
		throw new Error(`${cursor} fehlt im Code`);
	}
	const columnIndex = rows[rowIndex]!.indexOf(cursor);
	const code = codeWithCursor.replace(cursor, '');
	const documents: ParsedDocuments = {};
	const host = createInMemoryHost({ [filePath]: code }, { cloneUnchecked: true });
	const parsed = loadFile(filePath, documents, host);
	if (typeof parsed === 'string') {
		throw new Error(parsed);
	}
	return getSignatureHelp(parsed, rowIndex, columnIndex);
}

describe('signature-help', () => {
	// Die Hilfe erscheint, wenn der Cursor auf dem Funktionsnamen oder zwischen den Argumenten steht,
	// nicht auf einem Argument selbst.
	it('ein Aufruf nennt die Parameter der Funktion', () => {
		const help = signatureHelpAt(`f = (a: Integer b: Text) => a
¦f(1 §x§)`);
		expect(help?.signatures[0]?.parameters?.map(parameter => parameter.label)).to.deep.equal(['a', 'b']);
	});
	it('der aktive Parameter folgt dem Cursor', () => {
		const help = signatureHelpAt(`f = (a: Integer b: Text) => a
f(1 ¦ §x§)`);
		expect(help?.activeParameter).to.equal(1);
	});
	it('ohne Aufruf gibt es keine Hilfe', () => {
		expect(signatureHelpAt('a = ¦1')).to.equal(undefined);
	});
	// Der Wert hat schon einen gemeldeten Fehler, eine Signatur dazu wäre erfunden.
	it('der Aufruf eines Werts mit gemeldetem Fehler zeigt keine Hilfe', () => {
		const help = signatureHelpAt(`x = undefinedName
y = ¦x(1)`);
		expect(help).to.equal(undefined);
	});
});
