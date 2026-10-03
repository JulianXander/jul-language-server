import { CompletionItem, CompletionItemKind } from 'vscode-languageserver';
import { dereferenceNameFromObject, getStreamGetValueType, isFunctionType, isListType, isParametersType, isSubtypeOf, isTupleType, isTypeOfType, resolveAlias, typeToString } from 'jul-compiler/out/checker/type-algebra.js';
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
 * Parameter `prefixArgumentType` annehmen könnte - das ist ein harter Filter statt nur Sortierung,
 * weil `isSubtypeOf` dieselbe Regel anwendet, die der Checker beim tatsächlichen Aufruf ohnehin
 * durchsetzen würde (kein Raten, siehe docs/completion-relevance.md in jul-compiler). Weg fällt
 * nur, was bewiesen nicht passt, unbekannt bleibt drin.
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
				return isSubtypeOf(prefixArgumentType, firstParameterType) !== false;
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
			return parent.value === expression ? 'value' : undefined;
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
			return dictionaryTypeToCompletionItems(resolvedType.Fields).map(withFieldAssignmentInsertText);
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
				return withFieldAssignmentInsertText(parameterToCompletionItem(singleName, index, false));
			});
			return allCompletionItems;
		}
		default:
			return undefined;
	}
}

/**
 * Ein Feld eines Dictionary-Literals (Wert, nicht Typ) braucht immer einen zugewiesenen Wert -
 * anders als im Dictionary-Typ ist `f1: Integer` ohne `= ...` hier kein gültiger Ausdruck.
 */
function withFieldAssignmentInsertText(completionItem: CompletionItem): CompletionItem {
	return {
		...completionItem,
		insertText: completionItem.label + ' = ',
	};
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

/**
 * Ob an der Cursorposition ein Feldname stehen kann und ob daneben auch ein Wert gültig ist:
 * - `'exclusive'`: nur ein Feldname ist gültig (z.B. `x: MyType = [f]`, `f(a = 1 r)`)
 * - `'mixed'`: Feldname und Wert sind gültig (einziges Argument, z.B. `f(r)`)
 * - `'none'`: kein Feldname möglich (z.B. nach einem positionalen Argument, `f(1 r)`)
 * Unterschieden wird nur in der Argumentliste eines Aufrufs, denn nur dort ist neben dem
 * benannten auch das positionale Argument gültig. Überall sonst bleibt es bei `'exclusive'`,
 * ob dort überhaupt Feldnamen angeboten werden, entscheidet der erwartete Typ.
 */
export function getFieldNamePositionKind(
	expression: PositionedExpression | undefined,
): 'exclusive' | 'mixed' | 'none' {
	// Ist das Argument selbst ein Dictionary-Literal, steht der Cursor in dessen Klammern.
	if (expression?.type === 'empty'
		|| expression?.type === 'dictionary'
		|| expression?.type === 'object') {
		return 'exclusive';
	}
	const list = expression?.type === 'list' ? expression
		: expression?.parent?.type === 'list' ? expression.parent
			: undefined;
	const functionCall = list?.parent;
	if (!list
		|| functionCall?.type !== 'functionCall'
		|| functionCall.arguments !== list) {
		return 'exclusive';
	}
	// Positionale und benannte Argumente lassen sich nicht mischen: steht schon ein
	// positionales Argument da, kann keines mehr benannt werden.
	return list.values.length === 1 && expression !== list
		? 'mixed'
		: 'none';
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

/**
 * Was nach `x/` angeboten wird, abhängig vom Typ von x: die Felder eines Werts, bei einem Typwert
 * dessen Typeigenschaften.
 */
export function getFieldReferenceCompletionItems(dereferencedType: CompileTimeType): CompletionItem[] {
	switch (dereferencedType.julType) {
		case 'dictionaryLiteral':
			return dictionaryTypeToCompletionItems(dereferencedType.Fields);
		case 'or':
			return dereferencedType.ChoiceTypes.flatMap(getFieldReferenceCompletionItems);
		case 'stream':
			return [
				{
					label: 'getValue',
					kind: CompletionItemKind.Function,
					detail: typeToString(getStreamGetValueType(dereferencedType), 0, 0),
				},
			];
		case 'typeOf':
			return getTypePropertyNames(resolveAlias(dereferencedType.value)).map(name => {
				const propertyType = dereferenceNameFromObject(name, dereferencedType);
				return {
					label: name,
					kind: CompletionItemKind.Constant,
					detail: propertyType && typeToString(propertyType, 0, 0),
				};
			});
		default:
			return [];
	}
}

/**
 * Die Eigenschaften, die ein Typwert dieser Art hat (`List(Integer)/ElementType`).
 */
function getTypePropertyNames(type: CompileTimeType): string[] {
	switch (type.julType) {
		case 'dictionary':
		case 'list':
		case 'tuple':
			return ['ElementType'];
		case 'dictionaryLiteral':
			return Object.keys(type.Fields);
		case 'function':
			return ['ParamsType', 'ReturnType'];
		case 'parameters':
			return type.singleNames.map(parameter => parameter.name);
		case 'stream':
			return ['ValueType'];
		default:
			return [];
	}
}
