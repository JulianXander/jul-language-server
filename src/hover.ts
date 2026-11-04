import { getFileCheckResult, getCalledFunctionType, getTypeInfo } from 'jul-compiler/out/checker/check-result.js';
import { MarkupContent } from 'vscode-languageserver';
import { ParsedDocuments } from 'jul-compiler/out/checker/checker.js';
import { resolvePlaceholders, typeToString } from 'jul-compiler/out/checker/type-algebra.js';
import { ParseBranching, ParsedFile, PositionedExpression, TypeInfo } from 'jul-compiler/out/syntax-tree.js';
import { findExpressionInParsedFile, getSymbolDefinition, getSymbolInfoTypeInfo } from './symbol-lookup.js';
import { getDeclaredType } from './util.js';

export function getHover(
	parsedFile: ParsedFile,
	rowIndex: number,
	columnIndex: number,
	/**
	 * Ordner der Datei, gegen ihn werden Importpfade aufgelöst.
	 */
	folderPath: string,
	parsedDocuments: ParsedDocuments,
): MarkupContent | undefined {
	const checkResult = getFileCheckResult(parsedFile);
	const { expression, scopes } = findExpressionInParsedFile(parsedFile, rowIndex, columnIndex);
	if (!expression) {
		return;
	}
	if (isStatementBranching(expression)) {
		return {
			kind: 'markdown',
			value: statementBranchingHint,
		};
	}

	const foundSymbol = getSymbolDefinition(checkResult, expression, scopes, folderPath, parsedDocuments);
	if (foundSymbol) {
		const symbol = foundSymbol.symbol;
		// Die aufgerufene Funktion zeigt die Signatur, gegen die dieser Aufruf geprüft wurde.
		const parent = expression.parent;
		const calledFunctionType = parent?.type === 'functionCall' && parent.functionExpression === expression
			? getCalledFunctionType(checkResult, parent)
			: undefined;
		// Der Typ gehört der Stelle, nicht dem Namen: in einem branch ist er hier verengt.
		// Die Beschreibung steht dagegen nur an der Definition.
		// Ein Feld im Zugriff zeigt den Typ des Zugriffs, nicht den der Felddeklaration: Hinter
		// einer womöglich leeren Quelle kommt Empty hinzu.
		const typeInfo = calledFunctionType
			? { type: calledFunctionType }
			: getTypeInfo(checkResult, expression)
			|| (parent?.type === 'nestedReference' && getDeclaredType(checkResult, expression))
			|| getSymbolInfoTypeInfo(foundSymbol, checkResult, parsedDocuments);
		return getTypeMarkdown(typeInfo, symbol.description);
	}

	const declaredType = getDeclaredType(checkResult, expression);
	if (declaredType) {
		return getTypeMarkdown(declaredType, undefined);
	}
}

//#region Branching als Anweisung

/**
 * Was ein Branching als Anweisung tut, steht nicht im Quelltext: Ein Treffer beendet die Funktion.
 * Der Hover auf dem ? sagt es.
 */
export const statementBranchingHint = 'Passt ein Arm, ist sein Wert das Ergebnis der Funktion und sie endet hier. Sonst geht es mit der nächsten Zeile weiter.';

/**
 * Ein Branching, das als Anweisung im Rumpf einer Funktion steht. Getroffen wird der Knoten selbst
 * nur am ?, sonst liegt ein Kind (Argumente, Arme, Rest) unter dem Cursor.
 */
function isStatementBranching(expression: PositionedExpression): expression is ParseBranching {
	const parent = expression.parent;
	return expression.type === 'branching'
		&& parent?.type === 'functionLiteral'
		&& parent.body.includes(expression);
}

//#endregion Branching als Anweisung

export function getTypeMarkdown(
	type: TypeInfo | undefined,
	description: string | undefined,
): MarkupContent {
	const typeString = type
		? `\`\`\`jul
${typeToString(resolvePlaceholders(type.type), 0, 0)}
\`\`\`
`
		: '';
	return {
		kind: 'markdown',
		value: typeString + (description ?? ''),
	};
}
