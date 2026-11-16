import { getFileCheckResult } from 'jul-compiler/out/checker/check-result.js';
import { ParsedDocuments } from 'jul-compiler/out/checker/checker.js';
import { ReferenceIndex } from 'jul-compiler/out/checker/reference-index.js';
import { Positioned } from 'jul-compiler/out/compiler-errors.js';
import { getPathFromImport } from 'jul-compiler/out/parser/parser.js';
import { dirname } from 'jul-compiler/out/path-util.js';
import { ParsedFile, ParseFunctionCall } from 'jul-compiler/out/syntax-tree.js';
import { getFieldAccessTypeFields } from './references.js';
import { findExpressionInParsedFile, getSymbolDefinition, isImportPath } from './symbol-lookup.js';

export interface DefinitionLocation extends Positioned {
	/**
	 * undefined bei einem Builtin, die Stelle liegt dann in der core-lib.
	 */
	filePath: string | undefined;
}

/**
 * Gehe zu Definition für den Ausdruck an der Position: der Anfang der importierten Datei auf einem
 * Importpfad, die Felder des Typs auf einem Feldzugriff mit erwartetem Typ, sonst die Definition
 * des Symbols, durch Importe hindurch. Leer, wenn dort kein Symbol steht.
 */
export function getDefinitionLocations(
	parsedFile: ParsedFile,
	rowIndex: number,
	columnIndex: number,
	parsedDocuments: ParsedDocuments,
	referenceIndex: ReferenceIndex,
): DefinitionLocation[] {
	const documentPath = parsedFile.filePath;
	const folderPath = dirname(documentPath);
	const { expression, scopes } = findExpressionInParsedFile(parsedFile, rowIndex, columnIndex);
	if (!expression) {
		return [];
	}
	if (isImportPath(expression)) {
		const { fullPath } = getPathFromImport(expression.parent!.parent as ParseFunctionCall, folderPath);
		if (!fullPath) {
			return [];
		}
		return [{
			filePath: fullPath,
			startRowIndex: 0,
			startColumnIndex: 0,
			endRowIndex: 0,
			endColumnIndex: 0,
		}];
	}
	const foundSymbol = getSymbolDefinition(getFileCheckResult(parsedFile), expression, scopes, folderPath, parsedDocuments);
	if (!foundSymbol) {
		return [];
	}
	const typeFields = !foundSymbol.isBuiltIn
		&& getFieldAccessTypeFields(
			expression,
			{ symbol: foundSymbol.symbol, filePath: foundSymbol.filePath || documentPath },
			referenceIndex);
	if (typeFields) {
		return typeFields.map(typeField => toDefinitionLocation(typeField.filePath, typeField.symbol));
	}
	return [toDefinitionLocation(
		foundSymbol.isBuiltIn
			? undefined
			: foundSymbol.filePath || documentPath,
		foundSymbol.symbol)];
}

function toDefinitionLocation(filePath: string | undefined, positioned: Positioned): DefinitionLocation {
	return {
		filePath: filePath,
		startRowIndex: positioned.startRowIndex,
		startColumnIndex: positioned.startColumnIndex,
		endRowIndex: positioned.endRowIndex,
		endColumnIndex: positioned.endColumnIndex,
	};
}
