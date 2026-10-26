import { Position, TextEdit } from 'vscode-languageserver';
import { FormatEdit, formatCode, getDiffEdits, getFormatEdits } from 'jul-compiler/out/formatter/formatter.js';

/**
 * Dokument formatieren: alle Klammerlisten in die Form, die das Zeichen hinter ihrer öffnenden
 * Klammer vorgibt.
 */
export function getDocumentFormattingEdits(code: string, filePath: string): TextEdit[] {
	return getFormatEdits(code, filePath).map(toTextEdit);
}

/**
 * Enter in einer Klammerliste bricht sie um: jedes Feld in eine eigene Zeile. Das gilt hinter der
 * öffnenden Klammer wie zwischen zwei Feldern. Dafür wird der getippte Umbruch erst rückgängig
 * gemacht, weil VSCode die neue Zeile zwischen zwei Feldern nicht tiefer einrückt und die Liste
 * dann nicht mehr lesbar ist. Nichts passiert, wenn die neue Zeile leer ist (eine Zeile wird
 * begonnen, keine Liste umgebrochen) oder der Umbruch nicht zwischen den Feldern einer Liste steht.
 * position ist die Cursorposition nach dem Enter.
 */
export function getOnTypeFormattingEdits(code: string, filePath: string, position: Position): TextEdit[] {
	const rows = code.split('\n');
	const newRowIndex = position.line;
	const previousRow = rows[newRowIndex - 1];
	const newRow = rows[newRowIndex];
	if (previousRow === undefined || newRow === undefined || newRow.trim() === '') {
		return [];
	}
	const head = previousRow.trimEnd();
	const tail = newRow.trimStart();
	const separator = head.endsWith('(') || head.endsWith('[')
		? ''
		: ' ';
	const joinedRows = [
		...rows.slice(0, newRowIndex - 1),
		head + separator + tail,
		...rows.slice(newRowIndex + 1),
	];
	const joinedCode = joinedRows.join('\n');
	const formatted = formatCode(joinedCode, filePath, {
		at: { rowIndex: newRowIndex - 1, columnIndex: head.length },
		multiline: true,
	});
	if (formatted === joinedCode) {
		return [];
	}
	return getDiffEdits(code, formatted).map(toTextEdit);
}

function toTextEdit(edit: FormatEdit): TextEdit {
	return {
		range: {
			start: { line: edit.startRowIndex, character: edit.startColumnIndex },
			end: { line: edit.endRowIndex, character: edit.endColumnIndex },
		},
		newText: edit.newText,
	};
}
