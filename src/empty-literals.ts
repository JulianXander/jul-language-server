import { Range } from 'vscode-languageserver';
import { forEachChild, ParsedFile, PositionedExpression } from 'jul-compiler/out/syntax-tree.js';
import { positionedToRange } from './util.js';

/**
 * Das Empty-Literal [] ist ein eigener Wert, wird im Editor aber wie ein leeres Klammernpaar
 * gefärbt: die bracket pair colorization übermalt jede Farbe aus Grammatik und Semantic Tokens.
 * Nur eine Decoration liegt darüber, und die braucht diese Positionen. Siehe extension.ts.
 *
 * Der Server schickt sie nach jedem Verarbeiten einer Änderung ungefragt mit. Eine Anfrage des
 * Clients müsste ausstehende Änderungen sofort verarbeiten und würde so das Zusammenfassen beim
 * Tippen aushebeln, siehe change-debouncer.ts.
 */
export const emptyLiteralsNotification = 'jul/emptyLiterals';

export type EmptyLiteralsParams = {
	uri: string;
	/** Version des Dokuments, zu der die Positionen gehören */
	version: number;
	ranges: Range[];
};

/**
 * Ohne checked (nicht geladen) gibt es keine.
 */
export function findEmptyLiterals(parsed: ParsedFile): Range[] {
	const ranges: Range[] = [];
	parsed.checked?.expressions?.forEach(expression => collectEmptyLiterals(expression, ranges));
	return ranges;
}

function collectEmptyLiterals(expression: PositionedExpression, ranges: Range[]): void {
	if (expression.type === 'empty') {
		ranges.push(positionedToRange(expression));
	}
	forEachChild(expression, child => {
		collectEmptyLiterals(child, ranges);
		return undefined;
	});
}
