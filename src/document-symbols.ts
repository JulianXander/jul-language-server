import { Positioned } from 'jul-compiler/out/compiler-errors.js';
import { getCheckedEscapableName } from 'jul-compiler/out/parser/parser-utils.js';
import { discardName, ParsedFile, ParseValueExpression, PositionedExpression } from 'jul-compiler/out/syntax-tree.js';
import { DocumentSymbol, SymbolKind } from 'vscode-languageserver-types';
import { positionedToRange } from './util.js';

/**
 * Die Gliederung einer geprüften Datei: Definitionen, Felder und Parameter, verschachtelt wie im
 * Quelltext. undefined, solange die Datei nicht geprüft ist.
 */
export function getDocumentSymbols(parsedFile: ParsedFile): DocumentSymbol[] | undefined {
	const expressions = parsedFile.checked?.expressions;
	return expressions && getDocumentSymbolsFromExpressions(expressions);
}

function getDocumentSymbolsFromExpressions(expressions: PositionedExpression[]): DocumentSymbol[] {
	return expressions.flatMap(expression =>
		getDocumentSymbolsFromExpression(expression));
}

function getDocumentSymbolsFromExpression(expression: PositionedExpression): DocumentSymbol[] {
	switch (expression.type) {
		case 'branching':
		case 'typeBranching':
			return [
				...(expression.args
					? getDocumentSymbolsFromExpression(expression.args)
					: []),
				...getDocumentSymbolsFromExpressions(expression.branches),
			];
		case 'definition': {
			const children = [
				...(expression.typeGuard
					? getDocumentSymbolsFromExpression(expression.typeGuard)
					: []),
				...(expression.value
					? getDocumentSymbolsFromExpression(expression.value)
					: []),
			];
			// _ bindet nichts und gehört nicht in die Gliederung, was darin steht schon.
			if (expression.name.name === discardName) {
				return children;
			}
			return createDocumentSymbol(expression, expression.name, getSymbolKindFromExpression(expression.value), children);
		}
		case 'destructuring':
			return [
				...getDocumentSymbolsFromExpression(expression.fields),
				...(expression.value
					? getDocumentSymbolsFromExpression(expression.value)
					: []),
			];
		case 'destructuringField': {
			const children = [
				...(expression.typeGuard
					? getDocumentSymbolsFromExpression(expression.typeGuard)
					: []),
			];
			if (expression.name.name === discardName) {
				return children;
			}
			return createDocumentSymbol(expression, expression.name, SymbolKind.Field, children);
		}
		case 'destructuringFields':
			return getDocumentSymbolsFromExpressions(expression.fields);
		case 'dictionary':
			return getDocumentSymbolsFromExpressions(expression.fields);
		case 'dictionaryType':
			return getDocumentSymbolsFromExpressions(expression.fields);
		case 'functionCall':
			return [
				...(expression.prefixArgument
					? getDocumentSymbolsFromExpression(expression.prefixArgument)
					: []),
				...(expression.arguments
					? getDocumentSymbolsFromExpression(expression.arguments)
					: []),
			];
		case 'functionLiteral':
			return [
				...getDocumentSymbolsFromExpression(expression.params),
				...(expression.returnType
					? getDocumentSymbolsFromExpression(expression.returnType)
					: []),
				...getDocumentSymbolsFromExpressions(expression.body),
			];
		case 'functionTypeLiteral':
			return [
				...getDocumentSymbolsFromExpression(expression.params),
				...getDocumentSymbolsFromExpression(expression.returnType),
			];
		case 'list':
			return getDocumentSymbolsFromExpressions(expression.values);
		case 'parameter':
			return expression.name.name === discardName
				? []
				: createDocumentSymbol(expression, expression.name, SymbolKind.Variable);
		case 'parameters':
			return getDocumentSymbolsFromExpressions(expression.singleFields);
		case 'singleDictionaryField': {
			const children = expression.value && getDocumentSymbolsFromExpression(expression.value);
			return createDocumentSymbol(expression, expression.name, SymbolKind.Field, children);
		}
		case 'singleDictionaryTypeField': {
			const children = expression.typeGuard && getDocumentSymbolsFromExpression(expression.typeGuard);
			return createDocumentSymbol(expression, expression.name, SymbolKind.Field, children);
		}
		case 'binding':
		case 'data':
		case 'empty':
		case 'field':
		case 'float':
		case 'fraction':
		case 'index':
		case 'integer':
		case 'name':
		case 'nestedReference':
		case 'object':
		case 'reference':
		case 'spread':
		case 'text':
			return [];
		default: {
			const assertNever: never = expression;
			throw new Error(`Unexpected expression.type for getDocumentSymbolsFromExpression: ${(assertNever as PositionedExpression).type}`);
		}
	}
}

function getSymbolKindFromExpression(expression: ParseValueExpression | undefined): SymbolKind {
	return expression?.type === 'functionLiteral'
		? SymbolKind.Function
		: SymbolKind.Constant;
}

function createDocumentSymbol(
	definitionPosition: Positioned,
	nameExpression: PositionedExpression,
	kind: SymbolKind,
	children?: DocumentSymbol[],
): DocumentSymbol[] {
	const nameString = getCheckedEscapableName(nameExpression);
	if (nameString === undefined) {
		return [];
	}
	const documentSymbol: DocumentSymbol = {
		kind: kind,
		name: nameString,
		range: positionedToRange(definitionPosition),
		selectionRange: positionedToRange(nameExpression),
		children: children,
	};
	return [documentSymbol];
}
