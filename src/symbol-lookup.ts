import { dirname } from 'path';
import { getPathFromImport, isImportFunctionCall } from 'jul-compiler/out/parser/parser.js';
import { isExportedSymbol } from 'jul-compiler/out/parser/parser-utils.js';
import { Positioned } from 'jul-compiler/out/compiler-errors.js';
import {
	CompileTimeType,
	forEachChild,
	ParseDestructuringField,
	ParsedFile,
	PositionedExpression,
	SymbolDefinition,
	SymbolTable,
} from 'jul-compiler/out/syntax-tree.js';
import { findSymbolInScopesWithBuiltIns, ParsedDocuments } from 'jul-compiler/out/checker/checker.js';
import { getFieldSymbolsFromDictionaryType } from 'jul-compiler/out/checker/reference-index.js';
import { getDeclaredResolvedType, getDeclaredType, getResolvedType } from './util.js';

// Ausdruck an einer Position finden und auf sein Symbol auflösen. Grundlage für Hover,
// Go-to-Definition, Rename und SignatureHelp; hängt nicht an der LSP-Verbindung.

//#region findExpression

/**
 * Liefert auch scopes
 */
export function findExpressionInParsedFile(
	parsedFile: ParsedFile,
	rowIndex: number,
	columnIndex: number,
): {
	expression: PositionedExpression | undefined;
	scopes: SymbolTable[];
} {
	const parsed2 = parsedFile.checked!;
	const scopes: SymbolTable[] = [
		parsed2.symbols,
	];
	const expressions = parsed2.expressions;
	const expression = expressions && findExpressionInExpressions(
		expressions,
		rowIndex,
		columnIndex,
		scopes);
	return {
		expression: expression,
		scopes: scopes,
	};
}

/**
 * Füllt scopes
 */
function findExpressionInExpressions(
	expressions: PositionedExpression[],
	rowIndex: number,
	columnIndex: number,
	scopes: SymbolTable[],
): PositionedExpression | undefined {
	const foundOuter = expressions.find(expression => {
		return isPositionInRange(rowIndex, columnIndex, expression);
	});
	if (!foundOuter) {
		return undefined;
	}
	const foundInner = findExpressionInExpression(foundOuter, rowIndex, columnIndex, scopes);
	return foundInner;
}

/**
 * Füllt scopes
 * Gibt die gegebene expression zurück, falls keine passende innere expression gefunden wurde.
 */
function findExpressionInExpression(
	expression: PositionedExpression,
	rowIndex: number,
	columnIndex: number,
	scopes: SymbolTable[],
): PositionedExpression {
	pushScope(expression, scopes);
	const found = forEachChild(expression, child =>
		isPositionInRange(rowIndex, columnIndex, child)
			? findExpressionInExpression(child, rowIndex, columnIndex, scopes)
			: undefined);
	return found ?? expression;
}

/** Nur diese beiden bringen einen eigenen Scope mit. */
export function pushScope(expression: PositionedExpression, scopes: SymbolTable[]): boolean {
	switch (expression.type) {
		case 'functionLiteral':
		case 'functionTypeLiteral':
			scopes.push(expression.symbols);
			return true;
		default:
			return false;
	}
}

function isPositionInRange(
	rowIndex: number,
	columnIndex: number,
	range: Positioned,
): boolean {
	return (range.startRowIndex < rowIndex
		|| (range.startRowIndex === rowIndex && range.startColumnIndex <= columnIndex))
		&& (range.endRowIndex > rowIndex
			|| (range.endRowIndex === rowIndex && range.endColumnIndex >= columnIndex));
}

//#endregion findExpression

//#region get Symbol

export interface SymbolInfo {
	isBuiltIn: boolean;
	symbol: SymbolDefinition;
	name: string;
	/**
	 * undefined, wenn Symbol in gleicher Datei gefunden
	 * Leerstring, wenn builtin.
	 */
	filePath?: string;
}

/**
 * Löst den Ausdruck auf das lokal gebundene Symbol auf, ohne durch Importe hindurchzufolgen.
 * Für Go-to-Definition/Hover wird das Ergebnis über `resolveThroughImports` weitergereicht
 * (siehe `getSymbolDefinition`); Rename/Find-All-References brauchen dagegen genau diese
 * ungefolgte, lokale Bindung, um sie alias-bewusst über `resolveCanonicalSymbol` (jul-compiler)
 * aufzulösen - ein Alias darf dort nicht wie beim Go-to-Definition blind mitgezogen werden.
 */
export function getRawSymbolDefinition(
	expression: PositionedExpression,
	scopes: SymbolTable[],
	folderPath: string,
	parsedDocuments: ParsedDocuments,
): SymbolInfo | undefined {
	switch (expression.type) {
		case 'reference': {
			const name = expression.name.name;
			const definition = findSymbolInScopesWithBuiltIns(name, scopes);
			return definition && {
				...definition,
				name: name,
			};
		}
		case 'definition': {
			// TODO GoToDefinition: bei import: go to source file symbol?
			// create dictionary type mit allen definitions?
			return undefined;
		}
		case 'destructuring': {
			// TODO stattdessen bei name case, destrucuring als parent expression?
			// TODO GoToDefinition: bei import: go to source file symbol
			// if (isImport(expression.value)) {
			// 	const importedPath = getPathFromImport(expression.value);
			// 	const importedFile = parsedDocuments[importedPath];
			// 	const importedSymbol = importedFile?.symbols[expression.fields];
			// }
			return undefined;
		}
		case 'name': {
			const parent = expression.parent;
			const name = expression.name;
			switch (parent?.type) {
				case 'destructuringField': {
					const importedSymbol = getImportedSymbol(parent, folderPath, parsedDocuments);
					if (importedSymbol) {
						return importedSymbol.symbol && {
							name: name,
							isBuiltIn: false,
							symbol: importedSymbol.symbol,
							filePath: importedSymbol.filePath,
						};
					}
					// Kein Import: der lokale Name bindet selbst, wie bei einer normalen Definition.
					// Den Typ trägt das Symbol im Scope, nicht das in destructuringFields.symbols.
					if (expression !== parent.name) {
						return undefined;
					}
					const definition = findSymbolInScopesWithBuiltIns(name, scopes);
					return definition && {
						...definition,
						name: name,
					};
				}
				case 'nestedReference': {
					const declaredSourceType = getDeclaredType(parent.source);
					const sourceType = getResolvedType(declaredSourceType ?? parent.source.typeInfo);
					const foundSymbol = sourceType && getSymbolFromDictionaryType(sourceType, name);
					return foundSymbol;
				}
				case 'singleDictionaryField':
				case 'singleDictionaryTypeField': {
					const declaredParentType = getDeclaredResolvedType(parent.parent!);
					const foundSymbol = declaredParentType && getSymbolFromDictionaryType(declaredParentType, name);
					return foundSymbol;
				}
				default: {
					const definition = findSymbolInScopesWithBuiltIns(name, scopes);
					return definition && {
						...definition,
						name: name,
					};
				}
			}
		}
		case 'binding':
		case 'data':
		case 'branching':
		case 'typeBranching':
		case 'destructuringField':
		case 'destructuringFields':
		case 'dictionary':
		case 'dictionaryType':
		case 'empty':
		case 'field':
		case 'float':
		case 'fraction':
		case 'functionCall':
		case 'functionLiteral':
		case 'functionTypeLiteral':
		case 'index':
		case 'integer':
		case 'list':
		case 'nestedReference':
		case 'object':
		case 'parameter':
		case 'parameters':
		case 'singleDictionaryField':
		case 'singleDictionaryTypeField':
		case 'spread':
		case 'text':
			return undefined;
		default: {
			const assertNever: never = expression;
			throw new Error(`Unexpected expression.type: ${(assertNever as PositionedExpression).type}`);
		}
	}
}

/**
 * Für Go-to-Definition/Hover: wie `getRawSymbolDefinition`, folgt aber zusätzlich durch Importe
 * (auch Aliase) bis zur tatsächlichen Deklaration durch, siehe `resolveThroughImports`.
 */
export function getSymbolDefinition(
	expression: PositionedExpression,
	scopes: SymbolTable[],
	folderPath: string,
	parsedDocuments: ParsedDocuments,
): SymbolInfo | undefined {
	const raw = getRawSymbolDefinition(expression, scopes, folderPath, parsedDocuments);
	if (!raw) {
		return undefined;
	}
	const rawFolderPath = raw.filePath
		? dirname(raw.filePath)
		: folderPath;
	return resolveThroughImports(raw, rawFolderPath, parsedDocuments);
}

export function getSymbolFromDictionaryType(
	dictionaryType: CompileTimeType,
	name: string,
): SymbolInfo | undefined {
	// TODO bei Union: Liste aller Treffer liefern statt nur des ersten?
	const found = getFieldSymbolsFromDictionaryType(dictionaryType, name)[0];
	return found && {
		name: name,
		isBuiltIn: found.filePath === '',
		symbol: found.symbol,
		filePath: found.filePath,
	};
}

export function getImportedSymbol(
	destructuringField: ParseDestructuringField,
	folderPath: string,
	parsedDocuments: ParsedDocuments,
): {
	symbol: SymbolDefinition | undefined;
	filePath: string;
} | undefined {
	if (destructuringField.parent?.type === 'destructuringFields') {
		const destructuring = destructuringField.parent.parent;
		if (destructuring?.type === 'destructuring'
			&& destructuring.value
			&& isImportFunctionCall(destructuring.value)) {
			// Einen Pfadfehler meldet schon der Parser.
			const { fullPath } = getPathFromImport(destructuring.value, folderPath);
			if (!fullPath) {
				return;
			}
			const importedDocument = parsedDocuments[fullPath];
			if (importedDocument) {
				const symbolName = destructuringField.source ?? destructuringField.name;
				const impordedExpressions = importedDocument.checked ?? importedDocument.unchecked;
				const importedSymbol = impordedExpressions.symbols[symbolName.name];
				return {
					symbol: importedSymbol && isExportedSymbol(importedSymbol)
						? importedSymbol
						: undefined,
					filePath: fullPath,
				};
			}
		}
	}
}

// Löst Verweise auf importierte Symbole direkt bis zur tatsächlichen Deklaration auf, statt an der
// lokalen Import-Zeile stehen zu bleiben. Ein Hop genügt, exportiert werden nur Definitionen.
export function resolveThroughImports(
	symbolInfo: SymbolInfo,
	folderPath: string,
	parsedDocuments: ParsedDocuments,
): SymbolInfo {
	const definition = symbolInfo.symbol.definition;
	if (definition?.type !== 'destructuringField') {
		return symbolInfo;
	}
	const imported = getImportedSymbol(definition, folderPath, parsedDocuments);
	if (!imported?.symbol) {
		return symbolInfo;
	}
	return {
		name: symbolInfo.name,
		isBuiltIn: false,
		symbol: imported.symbol,
		filePath: imported.filePath,
	};
}

//#endregion get Symbol
