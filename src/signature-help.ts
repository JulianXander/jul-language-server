import { ParameterInformation, SignatureHelp } from 'vscode-languageserver';
import { findSymbolInScopesWithBuiltIns } from 'jul-compiler/out/checker/checker.js';
import { typeToString } from 'jul-compiler/out/checker/type-algebra.js';
import { ParseFunctionCall, ParsedFile, SymbolDefinition, SymbolTable } from 'jul-compiler/out/syntax-tree.js';
import { getTypeMarkdown } from './hover.js';
import { findExpressionInParsedFile } from './symbol-lookup.js';
import { getParameterIndex, getResolvedType } from './util.js';

/**
 * Die Signaturhilfe für den Funktionsaufruf, in dem der Cursor steht. Keine Hilfe gibt es, wenn
 * der Cursor in keinem Aufruf steht, die Funktion kein Symbol ist oder ihr Wert schon als Fehler
 * gemeldet ist: Eine Signatur dazu wäre erfunden.
 */
export function getSignatureHelp(
	parsed: ParsedFile,
	rowIndex: number,
	columnIndex: number,
): SignatureHelp | undefined {
	// TODO find functiontLiteral, show param + return type
	const { expression, scopes } = findExpressionInParsedFile(parsed, rowIndex, columnIndex);
	if (expression?.parent?.type !== 'functionCall') {
		return undefined;
	}
	const functionCall = expression.parent;
	const functionSymbol = getFunctionSymbolFromFunctionCall(functionCall, scopes);
	if (!functionSymbol) {
		return undefined;
	}
	const normalizedFunctionType = getResolvedType(functionSymbol.symbol.typeInfo);
	if (normalizedFunctionType?.julType === 'invalid') {
		return undefined;
	}
	const functionType = functionSymbol.symbol.typeExpression;
	const parameterResults: ParameterInformation[] = [];
	if (functionType?.type === 'functionLiteral') {
		const paramsType = functionType.params;
		if (paramsType.type === 'parameters') {
			paramsType.singleFields.forEach(singleField => {
				parameterResults.push({
					label: singleField.name.name,
					documentation: getTypeMarkdown(singleField.typeInfo, singleField.description),
				});
			});
			const rest = paramsType.rest;
			if (rest) {
				parameterResults.push({
					label: rest.name.name,
					documentation: getTypeMarkdown(rest.typeInfo, rest.description),
				});
			}
		}
	}
	const parameterIndex = getParameterIndex(functionCall, rowIndex, columnIndex, parameterResults.length);
	return {
		signatures: [{
			label: normalizedFunctionType
				? typeToString(normalizedFunctionType, 0, 0)
				: functionSymbol.name,
			documentation: functionSymbol.symbol.description,
			parameters: parameterResults,
		}],
		activeParameter: parameterIndex,
		activeSignature: 0,
	};
}

function getFunctionSymbolFromFunctionCall(functionCall: ParseFunctionCall, scopes: SymbolTable[]): {
	name: string;
	isBuiltIn: boolean;
	symbol: SymbolDefinition;
} | undefined {
	const functionExpression = functionCall.functionExpression;
	if (functionExpression?.type === 'reference') {
		const functionName = functionExpression.name.name;
		const functionSymbol = findSymbolInScopesWithBuiltIns(functionName, scopes);
		return functionSymbol && {
			...functionSymbol,
			name: functionName,
		};
	}
}
