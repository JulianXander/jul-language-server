import { MarkupContent } from 'vscode-languageserver';
import { ParsedDocuments, resolvePlaceholders, typeToString } from 'jul-compiler/out/checker/checker.js';
import { ParsedFile, TypeInfo } from 'jul-compiler/out/syntax-tree.js';
import { findExpressionInParsedFile, getSymbolDefinition } from './symbol-lookup.js';
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
	const { expression, scopes } = findExpressionInParsedFile(parsedFile, rowIndex, columnIndex);
	if (!expression) {
		return;
	}

	const foundSymbol = getSymbolDefinition(expression, scopes, folderPath, parsedDocuments);
	if (foundSymbol) {
		const symbol = foundSymbol.symbol;
		// Der Typ gehört der Stelle, nicht dem Namen: in einem branch ist er hier verengt.
		// Die Beschreibung steht dagegen nur an der Definition.
		return getTypeMarkdown(('typeInfo' in expression && expression.typeInfo) || symbol.typeInfo, symbol.description);
	}

	const declaredType = getDeclaredType(expression);
	if (declaredType) {
		return getTypeMarkdown(declaredType, undefined);
	}
}

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
