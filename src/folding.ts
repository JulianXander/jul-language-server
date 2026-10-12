import { FoldingRange, FoldingRangeKind } from 'vscode-languageserver';
import {
	forEachChild,
	ParsedFile,
	PositionedExpression,
} from 'jul-compiler/out/syntax-tree.js';

const regionStart = /^\s*#region\b/;
const regionEnd = /^\s*#endregion\b/;
const onlyClosingBrackets = /^\s*[)\]]+\s*$/;

/**
 * Die Folding Ranges einer Datei: jeder mehrzeilige Ausdruck faltet als Ganzes, eine Funktion also
 * samt Parameterliste und Rumpf. Die Parameterliste allein faltet nie, denn sie ist nur ein Teil
 * der Funktion. Dazu kommen die Bereiche zwischen #region und #endregion.
 * Ein Server-Provider ersetzt VSCodes einrückungsbasierte Faltung, deshalb muss er alles abdecken,
 * was falten soll. rows ist der Quelltext zeilenweise.
 */
export function getFoldingRanges(parsedFile: ParsedFile | undefined, rows: string[]): FoldingRange[] {
	const expressions = (parsedFile?.checked ?? parsedFile?.unchecked)?.expressions;
	// Je Startzeile gilt der längste Bereich, VSCode behält ohnehin nur einen pro Zeile.
	const endRowByStartRow = new Map<number, number>();
	expressions?.forEach(expression => collectExpressionRanges(expression, rows, endRowByStartRow));
	const ranges: FoldingRange[] = [];
	endRowByStartRow.forEach((endLine, startLine) => ranges.push({ startLine, endLine }));
	ranges.push(...getRegionRanges(rows));
	return ranges;
}

function collectExpressionRanges(
	expression: PositionedExpression,
	rows: string[],
	endRowByStartRow: Map<number, number>,
): void {
	if (expression.type !== 'parameters') {
		const startRow = expression.startRowIndex;
		// Am Dateiende liegt die Endzeile hinter der letzten Zeile, ein leerer Rest gehört nicht zur Falte.
		let endRow = Math.min(expression.endRowIndex, rows.length - 1);
		while (endRow > startRow && (rows[endRow] ?? '').trim() === '') {
			endRow--;
		}
		// Die eigene schließende Klammer in eigener Zeile bleibt sichtbar. Gehört sie zu einem Kind,
		// das in einer späteren Zeile beginnt (letzter Aufruf im Rumpf), faltet das Kind selbst
		// und der Ausdruck umfasst sie ganz.
		if (endRow > startRow
			&& onlyClosingBrackets.test(rows[endRow] ?? '')
			&& !endsWithLaterStartingChild(expression, startRow, endRow)) {
			endRow--;
		}
		if (endRow > startRow && endRow > (endRowByStartRow.get(startRow) ?? -1)) {
			endRowByStartRow.set(startRow, endRow);
		}
	}
	forEachChild(expression, child => {
		collectExpressionRanges(child, rows, endRowByStartRow);
		return undefined;
	});
}

function endsWithLaterStartingChild(expression: PositionedExpression, startRow: number, endRow: number): boolean {
	return forEachChild(expression, child =>
		child.startRowIndex > startRow && Math.min(child.endRowIndex, endRow) === endRow
			? true
			: undefined) ?? false;
}

function getRegionRanges(rows: string[]): FoldingRange[] {
	const ranges: FoldingRange[] = [];
	const openStartRows: number[] = [];
	rows.forEach((row, rowIndex) => {
		if (regionStart.test(row)) {
			openStartRows.push(rowIndex);
		}
		else if (regionEnd.test(row)) {
			const startLine = openStartRows.pop();
			if (startLine !== undefined) {
				ranges.push({ startLine, endLine: rowIndex, kind: FoldingRangeKind.Region });
			}
		}
	});
	return ranges;
}
