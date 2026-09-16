import { CompletionItem, CompletionItemKind } from 'vscode-languageserver';
import {
	getTypeError,
	isFunctionType,
	isListType,
	isParametersType,
	isTupleType,
	isTypeOfType,
	resolveAlias,
	typeToString,
} from 'jul-compiler/out/checker/checker.js';
import {
	CompileTimeDictionary,
	CompileTimeType,
	Parameter,
	ParseFunctionCall,
	PositionedExpression,
	SymbolDefinition,
} from 'jul-compiler/out/syntax-tree.js';
import { map } from 'jul-compiler/out/util.js';
import { getDeclaredResolvedType, getParameterIndex, getResolvedType } from './util.js';

/**
 * Erkennt den Infix-Aufruf (`a.f(...)`, `prefixArgument` gesetzt), wenn die Cursorposition
 * entweder die Funktionsreferenz selbst ist oder der ganze Aufruf.
 */
export function getInfixFunctionCall(expression: PositionedExpression | undefined): ParseFunctionCall | undefined {
	if (!expression) {
		return undefined;
	}
	if (expression.type === 'functionCall'
		&& expression.prefixArgument) {
		return expression;
	}
	if (
		expression.type === 'reference'
		&& expression.parent?.type === 'functionCall'
		&& expression.parent.prefixArgument
		&& expression === expression.parent.functionExpression) {
		return expression.parent;
	}
	return undefined;
}

/**
 * Filtert bei der Funktionsauswahl im Infix-Aufruf (`a.f(...)`) auf Funktionen, deren erster
 * Parameter `prefixArgumentType` laut `getTypeError` überhaupt annehmen würde - das ist ein
 * harter Filter statt nur Sortierung, weil `getTypeError` dieselbe Regel anwendet, die der
 * Checker beim tatsächlichen Aufruf ohnehin durchsetzen würde (kein Raten, siehe
 * docs/completion-relevance.md in jul-compiler).
 */
export function getFirstArgumentSymbolFilter(
	prefixArgumentType: CompileTimeType | undefined,
): (symbol: SymbolDefinition) => boolean {
	return symbol => {
		if (!prefixArgumentType) {
			return false;
		}
		const symbolType = getResolvedType(symbol.typeInfo);
		if (isFunctionType(symbolType)) {
			const paramsType = symbolType.ParamsType;
			if (isParametersType(paramsType)) {
				let firstParameterType: CompileTimeType | undefined;
				if (paramsType.singleNames.length) {
					firstParameterType = paramsType.singleNames[0]?.type;
				}
				else if (paramsType.rest) {
					const restType = paramsType.rest?.type;
					if (isListType(restType)) {
						firstParameterType = restType.ElementType;
					}
					else if (isTupleType(restType)) {
						firstParameterType = restType.ElementTypes[0];
					}
				}
				if (!firstParameterType) {
					return false;
				}
				const typeError = getTypeError(undefined, prefixArgumentType, firstParameterType);
				return !typeError;
			}
		}
		return false;
	};
}

/**
 * Erkennt, ob eine Cursorposition ein Typ-Slot ist (z.B. hinter `:`) oder ein Wert-Slot -
 * für die Completion-Sortierung. `undefined`, wenn die Position nicht eindeutig einem der
 * beiden zuzuordnen ist (z.B. ein Top-Level-Statement, wo beides syntaktisch gültig ist);
 * dort wird bewusst nicht geraten.
 */
export function getExpectedPositionKind(
	expression: PositionedExpression | undefined,
): 'type' | 'value' | undefined {
	const parent = expression?.parent;
	if (!parent) {
		return undefined;
	}
	switch (parent.type) {
		case 'definition':
			if (parent.typeGuard === expression) {
				return 'type';
			}
			if (parent.value === expression) {
				return 'value';
			}
			return undefined;
		case 'parameter':
		case 'destructuringField':
		case 'singleDictionaryTypeField':
			return parent.typeGuard === expression ? 'type' : undefined;
		case 'singleDictionaryField':
			if (parent.typeGuard === expression) {
				return 'type';
			}
			if (parent.value === expression) {
				return 'value';
			}
			return undefined;
		case 'functionLiteral':
		case 'functionTypeLiteral':
			return parent.returnType === expression ? 'type' : undefined;
		case 'functionCall':
			// die aufgerufene Funktion ist immer ein Wert, auch bei Typkonstruktoren wie List/Or
			return parent.functionExpression === expression ? 'value' : undefined;
		default:
			return undefined;
	}
}

/**
 * Erwarteter Typ des Arguments an der Cursorposition. Bei einem Rest-Parameter ist das der
 * Elementtyp, nicht der Listentyp - ein einzelnes Argument wird gegen das Element geprüft.
 */
export function getExpectedArgumentType(
	functionCall: ParseFunctionCall,
	rowIndex: number,
	columnIndex: number,
): CompileTimeType | undefined {
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
	const parameterCount = paramsType.singleNames.length + (paramsType.rest ? 1 : 0);
	const parameterIndex = getParameterIndex(functionCall, rowIndex, columnIndex, parameterCount);
	if (parameterIndex < paramsType.singleNames.length) {
		return paramsType.singleNames[parameterIndex]?.type;
	}
	const restType = paramsType.rest?.type;
	if (isListType(restType)) {
		return restType.ElementType;
	}
	if (isTupleType(restType)) {
		return restType.ElementTypes[0];
	}
	return undefined;
}

/**
 * Übersetzt den erwarteten Typ an einer Position in die Sortier-Seite: wird dort ein Typ erwartet
 * (`Type`, `TypeOf(...)`, z.B. in `Or([] )`), gehören Typ-Symbole nach vorne, sonst Wert-Symbole.
 */
export function getPositionKindForExpectedType(
	expectedType: CompileTimeType | undefined,
): 'type' | 'value' | undefined {
	if (!expectedType) {
		return undefined;
	}
	return (expectedType.julType === 'type' || isTypeOfType(expectedType))
		? 'type'
		: 'value';
}

/**
 * Sortier-Seite an einer Argument-Position in einem Funktionsaufruf (z.B. `Or([] )`) - deckt
 * beide Fälle ab: `expression` ist die Argumentliste selbst (Cursor nach dem letzten Argument,
 * noch nichts getippt) oder ein bereits angefangener Argumentwert darin (`expression.parent`
 * ist die Liste).
 */
export function getArgumentPositionKind(
	expression: PositionedExpression | undefined,
	rowIndex: number,
	columnIndex: number,
): 'type' | 'value' | undefined {
	if (!expression) {
		return undefined;
	}
	const list = expression.type === 'list' ? expression
		: expression.parent?.type === 'list' ? expression.parent
			: undefined;
	if (!list) {
		return undefined;
	}
	const functionCall = list.parent;
	if (functionCall?.type !== 'functionCall' || functionCall.arguments !== list) {
		return undefined;
	}
	const expectedType = getExpectedArgumentType(functionCall, rowIndex, columnIndex);
	return getPositionKindForExpectedType(expectedType);
}

/**
 * Ob ein Symbol zur Typ-Seite gehört: entweder ist sein Wert selbst ein Typ (`Integer`, `MyType`),
 * oder es ist ein Typkonstruktor, also eine Funktion die einen Typ liefert (`And`, `Or`, `List`).
 */
export function isTypeSymbol(symbolType: CompileTimeType | undefined): boolean {
	if (isTypeOfType(symbolType)) {
		return true;
	}
	if (isFunctionType(symbolType)) {
		const returnType = symbolType.ReturnType;
		return returnType.julType === 'type' || isTypeOfType(returnType);
	}
	return false;
}

/**
 * `sortText` für ein Completion-Item: kein Filter, nur Umsortierung - an einer Typ-Position
 * stehen Typ-Symbole zuerst, an einer Wert-Position Wert-Symbole; beide bleiben aber immer
 * sichtbar (Prinzip Freiheit, siehe docs/completion-relevance.md in jul-compiler).
 * `undefined`, wenn `positionKind` unbekannt ist - dann wird nicht sortiert, sondern das
 * bisherige (alphabetische) Verhalten beibehalten.
 */
export function getCompletionSortText(
	name: string,
	isTypeSymbol: boolean,
	positionKind: 'type' | 'value' | undefined,
): string | undefined {
	if (!positionKind) {
		return undefined;
	}
	const bucket = isTypeSymbol === (positionKind === 'type') ? '0' : '1';
	return bucket + name;
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

/**
 * Completion-Items für die Felder eines Dictionary-Literals (`[]`, `[a = 1]`, ...), dessen
 * erwarteter Typ (declaredType) Felder vorschreibt.
 */
export function getDictionaryLiteralFieldCompletionItems(expression: PositionedExpression | undefined): CompletionItem[] | undefined {
	if (expression?.type === 'empty'
		|| expression?.type === 'dictionary'
		|| expression?.type === 'object') {
		const declaredType = getDeclaredResolvedType(expression);
		const allCompletionItems = declaredType && getDictionaryFieldCompletionItemsFromType(declaredType);
		if (!allCompletionItems) {
			return undefined;
		}
		// schon definierte Felder ausschließen
		if (expression.type === 'dictionary') {
			return allCompletionItems.filter(completionItem => {
				return !expression.symbols[completionItem.label];
			});
		}
		return allCompletionItems;
	}
	// Ein angefangener Feldname (z.B. `[f]`) parst noch nicht als dictionary, sondern als list mit
	// einer bloßen reference darin - der Parser erkennt "Dictionary-Feld" erst an einem `=`/`:`
	// danach. Ohne diesen Zweig verschwindet die Vervollständigung, sobald der erste Buchstabe steht.
	const list = expression?.type === 'list' ? expression
		: expression?.parent?.type === 'list' ? expression.parent
			: undefined;
	if (!list) {
		return undefined;
	}
	const declaredType = getDeclaredResolvedType(list);
	return declaredType && getDictionaryFieldCompletionItemsFromType(declaredType);
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
