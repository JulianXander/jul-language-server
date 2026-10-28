import { FoldingRange, FoldingRangeKind } from 'vscode-languageserver';
import {
	forEachChild,
	ParsedFile,
	PositionedExpression,
} from 'jul-compiler/out/syntax-tree.js';

const regionStart = /^\s*#region\b/;
const regionEnd = /^\s*#endregion\b/;

/**
 * Die Folding Ranges einer Datei: jeder mehrzeilige Ausdruck faltet als Ganzes, eine Funktion also
 * samt Parameterliste und Rumpf. Die Parameterliste allein faltet nie, denn sie ist nur ein Teil
 * der Funktion. Dazu kommen die Bereiche zwischen #region und #endregion.
 * Ein Server-Provider ersetzt VSCodes einrückungsbasierte Faltung, deshalb muss er alles abdecken,
 * was falten soll. rows ist der Quelltext zeilenweise.
 */
export function getFoldingRanges(parsedFile: ParsedFile | undefined, rows: string[]): FoldingRange[] {
	const expressions = (parsedFile?.checked ?? parsedFile?.syntax)?.expressions;
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
	if (expression.type !== 'parameters' && !isDestructuringWithValueOnClosingRow(expression)) {
		const startRow = expression.startRowIndex;
		// Am Dateiende liegt die Endzeile hinter der letzten Zeile, ein leerer Rest gehört nicht zur Falte.
		let endRow = Math.min(expression.endRowIndex, rows.length - 1);
		// Die Zeile mit der schließenden Klammer trägt bei einem Destructuring den Wert (den
		// Quellpfad eines imports) und bleibt sichtbar.
		if (expression.type === 'destructuringFields' && expression.parent?.type === 'destructuring') {
			endRow--;
		}
		while (endRow > startRow && (rows[endRow] ?? '').trim() === '') {
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

/**
 * Steht der Wert eines mehrzeiligen Destructurings in der Zeile der schließenden Klammer
 * (`) = import(§./datei.jul§)`), faltet das Destructuring nicht als Ganzes, sonst verschwindet
 * der Wert mit. Es faltet nur die Feldliste.
 */
function isDestructuringWithValueOnClosingRow(expression: PositionedExpression): boolean {
	return expression.type === 'destructuring'
		&& expression.value !== undefined
		&& expression.value.startRowIndex === expression.fields.endRowIndex;
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
