import { CompletionItem, CompletionItemKind } from 'vscode-languageserver';
import {
	dereferenceIndexFromObject,
	dereferenceNameFromObject,
	isFunctionType,
	isParametersType,
	isTypeOfType,
	resolveAlias,
	resolvePlaceholders,
	typeToString,
} from 'jul-compiler/out/checker/checker.js';
import { getCheckedEscapableName } from 'jul-compiler/out/parser/parser-utils.js';
import {
	CompileTimeDictionary,
	CompileTimeType,
	Parameter,
	ParseFunctionCall,
	PositionedExpression,
	TypeInfo,
} from 'jul-compiler/out/syntax-tree.js';
import { map } from 'jul-compiler/out/util.js';

/**
 * Der Server zeigt und prüft Typen, verarbeitet sie aber nicht weiter - hier ist die aufgelöste
 * Form also durchgängig die richtige.
 */
export function getResolvedType(typeInfo: TypeInfo | undefined): CompileTimeType | undefined {
	return typeInfo && resolvePlaceholders(typeInfo.type);
}

/**
 * Erwarteter Typ des `prefixArgument` bei einem Infix-Aufruf (`a.f(...)`) - der erste Parameter
 * von `f`. Wird von getDeclaredType (Hover, Completion, ...) genutzt, nicht nur von Completion.
 */
export function getPrefixArgumentDeclaredType(functionCall: ParseFunctionCall): CompileTimeType | undefined {
	const functionExpression = functionCall.functionExpression;
	if (!functionExpression) {
		return undefined;
	}
	const functionType = getResolvedType(functionExpression.typeInfo);
	if (!functionType || !isFunctionType(functionType)) {
		return undefined;
	}
	const paramsType = functionType.ParamsType;
	if (!isParametersType(paramsType)) {
		return undefined;
	}
	return paramsType.singleNames[0]?.type ?? paramsType.rest?.type;
}

/**
 * Ermittelt den Index des Parameters, in dem der Cursor bei einem Funktionsaufruf steht -
 * für SignatureHelp und die Parameter-Hover-Anzeige.
 */
export function getParameterIndex(
	functionCall: ParseFunctionCall,
	rowIndex: number,
	columnIndex: number,
	parameterCount: number,
): number {
	const argsExpression = functionCall.arguments;
	let parameterIndex = functionCall.prefixArgument ? 1 : 0;
	const values = argsExpression?.type === 'list' ? argsExpression.values
		: argsExpression?.type === 'dictionary' ? argsExpression.fields
			: undefined;
	if (values) {
		values.forEach(value => {
			// values vor der aktuellen Position zählen
			if ((value.endRowIndex < rowIndex ||
				(value.endRowIndex === rowIndex && value.endColumnIndex < columnIndex))
				// TODO was wenn mehr values als Parameter (ohne Rest Parameter)?
				&& parameterIndex < parameterCount - 1
			) {
				parameterIndex++;
			}
		});
	}
	return parameterIndex;
}

export function getDeclaredResolvedType(expression: PositionedExpression): CompileTimeType | undefined {
	return getResolvedType(getDeclaredType(expression));
}

// TODO recursive getDeclaredType für List elements
export function getDeclaredType(expression: PositionedExpression): TypeInfo | undefined {
	switch (expression.type) {
		case 'functionCall':
			return expression.typeInfo;
		case 'name': {
			if (expression.parent?.type === 'nestedReference') {
				return getDeclaredType(expression.parent);
			}
			break;
		}
		case 'nestedReference': {
			const nestedKey = expression.nestedKey;
			if (!nestedKey) {
				return undefined;
			}
			const sourceType = getDeclaredResolvedType(expression.source);
			if (!sourceType) {
				return undefined;
			}
			switch (nestedKey.type) {
				case 'index': {
					const dereferencedType = dereferenceIndexFromObject(nestedKey.name, sourceType);
					if (!dereferencedType) {
						return undefined;
					}
					return { type: dereferencedType };
				}
				case 'name':
				case 'text': {
					const fieldName = getCheckedEscapableName(nestedKey);
					if (!fieldName) {
						return undefined;
					}
					const dereferencedType = dereferenceNameFromObject(fieldName, sourceType);
					if (!dereferencedType) {
						return undefined;
					}
					return { type: dereferencedType };
				}
				default: {
					const assertNever: never = nestedKey;
					throw new Error(`Unexpected nestedKey.type ${(assertNever as PositionedExpression).type}`);
				}
			}
		}
		case 'reference': {
			return expression.typeInfo;
		}
		default:
			break;
	}
	switch (expression.parent?.type) {
		case 'definition':
			if (expression.parent.value === expression) {
				if (expression.parent.typeGuard) {
					const typeGuardType = expression.parent.typeGuard.typeInfo;
					if (!typeGuardType) {
						return undefined;
					}
					const resolvedTypeGuardType = resolvePlaceholders(typeGuardType.type);
					if (isTypeOfType(resolvedTypeGuardType)) {
						return { type: resolvedTypeGuardType.value };
					}
					return typeGuardType;
				}
				else {
					return expression.typeInfo;
				}
			}
			else {
				return undefined;
			}
		case 'functionLiteral': {
			if (expression.parent.params !== expression) {
				return undefined;
			}
			const functionLiteralDeclaredType = getDeclaredResolvedType(expression.parent);
			if (!functionLiteralDeclaredType) {
				return undefined;
			}
			if (!isFunctionType(functionLiteralDeclaredType)) {
				return undefined;
			}
			return { type: functionLiteralDeclaredType.ParamsType };
		}
		case 'functionCall': {
			if (expression.parent.prefixArgument === expression) {
				const prefixArgumentType = getPrefixArgumentDeclaredType(expression.parent);
				return prefixArgumentType && { type: prefixArgumentType };
			}
			// function call arg
			if (expression.parent.arguments !== expression) {
				return undefined;
			}
			const functionExpression = expression.parent.functionExpression;
			if (!functionExpression) {
				return undefined;
			}
			const functionType = getResolvedType(functionExpression.typeInfo);
			if (!functionType) {
				return undefined;
			}
			if (!isFunctionType(functionType)) {
				return undefined;
			}
			return { type: functionType.ParamsType };
		}
		case 'list': {
			const list = expression.parent;
			const listType = getDeclaredResolvedType(list);
			if (!listType) {
				return undefined;
			}
			const functionCall = list.parent;
			if (functionCall?.type === 'functionCall'
				&& functionCall.arguments === list) {
				const dereferencedlistType = listType;
				// function call arg
				if (!isParametersType(dereferencedlistType)) {
					return undefined;
				}
				const parameterCount = dereferencedlistType.singleNames.length + (dereferencedlistType.rest ? 1 : 0);
				const parameterIndex = getParameterIndex(functionCall, expression.startRowIndex, expression.startColumnIndex, parameterCount);
				const currentParameter = parameterIndex < dereferencedlistType.singleNames.length
					? dereferencedlistType.singleNames[parameterIndex]
					: dereferencedlistType.rest;
				if (!currentParameter) {
					return undefined;
				}
				if (!currentParameter.type) {
					return undefined;
				}
				return { type: currentParameter.type };
			}
			const index = list.values.indexOf(expression as any);
			const elementType = dereferenceIndexFromObject(index, listType);
			if (!elementType) {
				return undefined;
			}
			return { type: elementType };
		}
		case 'singleDictionaryField': {
			const dictionary = expression.parent.parent;
			if (!dictionary) {
				return undefined;
			}
			const dictionaryDeclaredType = getDeclaredResolvedType(dictionary);
			if (!dictionaryDeclaredType) {
				return undefined;
			}
			const nameString = getCheckedEscapableName(expression.parent.name);
			if (!nameString) {
				return undefined;
			}
			const fieldType = dereferenceNameFromObject(nameString, dictionaryDeclaredType);
			if (!fieldType) {
				return undefined;
			}
			return { type: fieldType };
		}
		case undefined:
			return undefined;
		default:
			return undefined;
	}
}

export function getDictionaryFieldCompletionItemsFromType(declaredType: CompileTimeType): CompletionItem[] | undefined {
	const resolvedType = declaredType.julType === 'alias'
		? resolveAlias(declaredType)
		: declaredType;
	switch (resolvedType.julType) {
		case 'dictionaryLiteral':
			return dictionaryTypeToCompletionItems(resolvedType.Fields);
		case 'or': {
			const allCompletionItems: CompletionItem[] = [];
			resolvedType.ChoiceTypes.forEach(choiceType => {
				const completionItems = getDictionaryFieldCompletionItemsFromType(choiceType);
				completionItems?.forEach(newCompletionItem => {
					// Duplikate vermeiden
					if (!allCompletionItems?.some(existingCompletionItem => existingCompletionItem.label === newCompletionItem.label)) {
						allCompletionItems?.push(newCompletionItem);
					}
				});
			});
			return allCompletionItems;
		}
		case 'parameters': {
			// function call arg
			const allCompletionItems = resolvedType.singleNames.map((singleName, index) => {
				return parameterToCompletionItem(singleName, index, false);
			});
			return allCompletionItems;
		}
		default:
			return undefined;
	}
}

function parameterToCompletionItem(parameter: Parameter, index: number, isRest: boolean): CompletionItem {
	const completionItem: CompletionItem = {
		label: (isRest ? '...' : '') + parameter.name,
		kind: CompletionItemKind.Constant,
		detail: parameter.type
			? typeToString(parameter.type, 0, 0)
			: undefined,
		sortText: '' + index,
	};
	return completionItem;
}

export function dictionaryTypeToCompletionItems(
	fields: CompileTimeDictionary,
): CompletionItem[] {
	return map(
		fields,
		(type, name) => {
			const completionItem: CompletionItem = {
				label: name,
				kind: CompletionItemKind.Constant,
				detail: typeToString(type, 0, 0),
			};
			return completionItem;
		});
}
