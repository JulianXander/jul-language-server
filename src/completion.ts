import { CompletionItem, CompletionItemKind, InsertTextFormat } from 'vscode-languageserver';
import { dereferenceNameFromObject, getStreamGetValueType, isDictionaryLiteralType, isFunctionType, isListType, isParameterReference, isParametersType, isSubtypeOf, isTextLiteralType, isTupleType, isTypeOfType, resolveAlias, resolvePlaceholders, typeToString } from 'jul-compiler/out/checker/type-algebra.js';
import {
	CompileTimeDictionary,
	CompileTimeType,
	Parameter,
	ParseDestructuringFields,
	ParseFunctionCall,
	PositionedExpression,
	SymbolDefinition,
	SymbolTable,
	TextLiteralType,
} from 'jul-compiler/out/syntax-tree.js';
import { builtInSymbols, findSymbolInScopesWithBuiltIns } from 'jul-compiler/out/checker/checker.js';
import { isDefined, map } from 'jul-compiler/out/util.js';
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

//#region lambda snippet

/**
 * Der Aufruf, in dessen Argumentliste die Cursorposition liegt. `expression` ist die Argumentliste
 * selbst (leer: `empty`, sonst `list`) oder ein Argumentwert darin.
 */
function getCallOfArgumentPosition(expression: PositionedExpression | undefined): ParseFunctionCall | undefined {
	const argumentList = expression?.type === 'list' || expression?.type === 'empty'
		? expression
		: expression?.parent?.type === 'list'
			? expression.parent
			: undefined;
	const functionCall = argumentList?.parent;
	return functionCall?.type === 'functionCall' && functionCall.arguments === argumentList
		? functionCall
		: undefined;
}

/**
 * Completion-Item mit einem Lambda `(value index) => `, wenn an der Argumentposition eine Funktion
 * erwartet wird. Die Parameternamen sind durch den erwarteten Funktionstyp fest vorgegeben; ist ein
 * Name im Scope schon vergeben, bekommt der Parameter einen Alias (`(value2 = value index) => `),
 * damit er die äußere Definition nicht verdeckt.
 */
export function getLambdaCompletionItem(
	expression: PositionedExpression | undefined,
	rowIndex: number,
	columnIndex: number,
	scopes: SymbolTable[],
): CompletionItem | undefined {
	const functionCall = getCallOfArgumentPosition(expression);
	if (!functionCall) {
		return undefined;
	}
	const rawExpectedType = getExpectedArgumentType(functionCall, rowIndex, columnIndex);
	const expectedType = rawExpectedType && resolvePlaceholders(rawExpectedType);
	if (!isFunctionType(expectedType)
		|| !isParametersType(expectedType.ParamsType)) {
		return undefined;
	}
	const parameterNames = expectedType.ParamsType.singleNames.map(parameter => parameter.name);
	const usedNames = new Set(parameterNames);
	const isDefinedInScope = (name: string) => scopes.some(symbols => name in symbols);
	const escapeSnippet = (text: string) => text.replaceAll('$', '\\$');
	// Tabstops nur für die Aliasnamen, in Parameterreihenfolge und vor dem Funktionskörper ($0)
	let tabstopCount = 0;
	const labelParts: string[] = [];
	const snippetParts: string[] = [];
	parameterNames.forEach(name => {
		if (!isDefinedInScope(name)) {
			labelParts.push(name);
			snippetParts.push(escapeSnippet(name));
			return;
		}
		let counter = 2;
		while (usedNames.has(name + counter) || isDefinedInScope(name + counter)) {
			counter++;
		}
		const alias = name + counter;
		usedNames.add(alias);
		tabstopCount++;
		labelParts.push(alias + ' = ' + name);
		snippetParts.push(`\${${tabstopCount}:${escapeSnippet(alias)}} = ${escapeSnippet(name)}`);
	});
	return {
		label: '(' + labelParts.join(' ') + ') =>',
		kind: CompletionItemKind.Snippet,
		detail: 'Lambda',
		insertText: '(' + snippetParts.join(' ') + ') => $0',
		insertTextFormat: InsertTextFormat.Snippet,
		// vor allen Symbolen
		sortText: '!',
		preselect: true,
	};
}

//#endregion lambda snippet

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
			return dictionaryTypeToCompletionItems(resolvedType.FieldTypes).map(withFieldAssignmentInsertText);
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
 * Der Wert, hinter dessen schließender Klammer oder Textbegrenzung der Cursor steht (`f(1)|`,
 * `[]|`, `[a = 1]|`, `5|`). Dort kann ein Infix-Aufruf mit dem Wert als Präfixargument anschließen
 * (`f(1).g()`). Bei einem Aufruf ist das der Aufruf selbst, nicht seine Argumentliste.
 */
export function getValueBeforeCursor(
	expression: PositionedExpression | undefined,
	rowIndex: number,
	columnIndex: number,
): PositionedExpression | undefined {
	if (!expression
		|| expression.endRowIndex !== rowIndex
		|| expression.endColumnIndex !== columnIndex) {
		return undefined;
	}
	const parent = expression.parent;
	if (parent?.type === 'functionCall' && parent.arguments === expression) {
		return parent;
	}
	switch (expression.type) {
		case 'empty':
		case 'list':
		case 'dictionary':
		case 'object':
		case 'text':
		case 'integer':
		case 'float':
		case 'fraction':
			return expression;
		default:
			return undefined;
	}
}

/**
 * Einfügetext einer Funktion im Infix-Aufruf (`a.f(...)`): Name samt Klammern. Der Cursor steht in
 * den Klammern, wenn die Funktion außer dem Präfixargument weitere Parameter hat, sonst dahinter.
 * `existingCall` ist der schon getippte Infix-Aufruf (Punkt steht), `'afterValue'` der Cursor
 * hinter einem Wert ohne Punkt, dann gehört der Punkt mit in den Einfügetext.
 * `undefined`, wenn der Aufruf schon Klammern hat oder das Symbol keine Funktion ist.
 */
export function getInfixCallInsertText(
	name: string,
	symbolType: CompileTimeType | undefined,
	existingCall: ParseFunctionCall | 'afterValue',
): string | undefined {
	if (existingCall !== 'afterValue' && existingCall.arguments || !isFunctionType(symbolType)) {
		return undefined;
	}
	const paramsType = symbolType.ParamsType;
	const hasFurtherParameters = isParametersType(paramsType)
		&& (paramsType.singleNames.length > 1 || !!paramsType.rest);
	return (existingCall === 'afterValue' ? '.' : '')
		+ name.replaceAll('$', '\\$')
		+ (hasFurtherParameters ? '($0)' : '()$0');
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
		let allCompletionItems = declaredType && getDictionaryFieldCompletionItemsFromType(declaredType);
		if (!allCompletionItems) {
			return undefined;
		}
		// Das Präfixargument (`a.f(...)`) belegt den ersten Parameter schon.
		if (declaredType?.julType === 'parameters'
			&& expression.parent?.type === 'functionCall'
			&& expression.parent.arguments === expression
			&& expression.parent.prefixArgument) {
			allCompletionItems = allCompletionItems.slice(1);
		}
		// schon definierte Felder ausschließen
		if (expression.type === 'dictionary') {
			return allCompletionItems.filter(completionItem => {
				return !expression.symbols[completionItem.label];
			});
		}
		return allCompletionItems;
	}
	// Steht schon ein Feld mit `=` im Literal (`[a = 1 f]`), bleibt der Knoten ein unaufgelöstes `data`,
	// und der angefangene Feldname ist ein field ohne Wert.
	const field = expression?.type === 'reference' && expression.parent?.type === 'field'
		? expression.parent
		: undefined;
	const data = field?.parent?.type === 'data' ? field.parent : undefined;
	if (data) {
		const declaredType = getDeclaredResolvedType(data);
		const allCompletionItems = declaredType && getDictionaryFieldCompletionItemsFromType(declaredType);
		return allCompletionItems?.filter(completionItem => {
			return !data.fields.some(otherField => {
				return otherField !== field
					&& otherField.definition
					&& otherField.name.type === 'reference'
					&& otherField.name.name.name === completionItem.label;
			});
		});
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
	// Eine leere Argumentliste erlaubt Feldnamen und positionale Argumente.
	if (expression?.type === 'empty'
		&& expression.parent?.type === 'functionCall'
		&& expression.parent.arguments === expression) {
		return 'mixed';
	}
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
			return dictionaryTypeToCompletionItems(dereferencedType.FieldTypes);
		case 'or':
			return dereferencedType.ChoiceTypes.flatMap(getFieldReferenceCompletionItems);
		case 'error': {
			const messageType = dereferenceNameFromObject('message', dereferencedType);
			return [
				{
					label: 'message',
					kind: CompletionItemKind.Constant,
					detail: messageType && typeToString(messageType, 0, 0),
				},
			];
		}
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
			return Object.keys(type.FieldTypes);
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

//#region getCompletionItems

function textLiteralTypeToCompletionItem(value: TextLiteralType): CompletionItem {
	const completionItem: CompletionItem = {
		label: value.value,
		kind: CompletionItemKind.Constant,
	};
	return completionItem;
}

/**
 * @param infixFunctionCall der schon getippte Infix-Aufruf, oder `'afterValue'` hinter einem Wert
 * ohne Punkt: Funktionen werden dann samt Klammern (und Punkt) als Snippet eingefügt.
 */
function symbolsToCompletionItems(
	scopes: SymbolTable[],
	symbolFilter?: (symbol: SymbolDefinition, name: string) => boolean,
	positionKind?: 'type' | 'value',
	infixFunctionCall?: ParseFunctionCall | 'afterValue',
): CompletionItem[] {
	return scopes.flatMap(symbols => {
		return map(
			symbols,
			(symbol, name) => {
				const showSymbol = symbolFilter?.(symbol, name) ?? true;
				if (!showSymbol) {
					return undefined;
				}
				const symbolType = getResolvedType(symbol.typeInfo);
				const isFunction = isFunctionType(symbolType);
				const sortText = getCompletionSortText(name, isTypeSymbol(symbolType), positionKind);
				const completionItem: CompletionItem = {
					label: name,
					kind: isFunction
						? CompletionItemKind.Function
						: CompletionItemKind.Constant,
					detail: symbolType && typeToString(symbolType, 0, 0),
					documentation: symbol.description,
					sortText: sortText,
				};
				const infixInsertText = infixFunctionCall
					&& getInfixCallInsertText(name, symbolType, infixFunctionCall);
				if (infixInsertText) {
					completionItem.insertText = infixInsertText;
					completionItem.insertTextFormat = InsertTextFormat.Snippet;
				}
				return completionItem;
			}).filter(isDefined);
	});
}

/**
 * Die Completion-Items an einer Position, ohne Dateisystem und LSP-Verbindung. Nicht enthalten sind
 * die eingebettete Sprache (html) und Importpfade, die brauchen Dokument bzw. Dateisystem.
 * @param isCoreLib in der core-lib sind die builtInSymbols bereits der unterste Scope,
 * sonst stünde jeder builtIn Name doppelt in der completion.
 */
export function getCompletionItems(
	expression: PositionedExpression | undefined,
	scopes: SymbolTable[],
	rowIndex: number,
	columnIndex: number,
	isCoreLib: boolean,
): CompletionItem[] | undefined {
	const positionKind = getExpectedPositionKind(expression)
		?? getArgumentPositionKind(expression, rowIndex, columnIndex);

	//#region Text literal with declared type
	if (expression?.type === 'text') {
		const declaredType = getDeclaredResolvedType(expression);
		switch (declaredType?.julType) {
			case 'textLiteral':
				return [textLiteralTypeToCompletionItem(declaredType)];
			case 'or':
				return declaredType.ChoiceTypes
					.filter(isTextLiteralType)
					.map(textLiteralTypeToCompletionItem);
			default:
				break;
		}
	}
	//#endregion Text literal with declared type

	const allScopes = isCoreLib
		? scopes
		: [...scopes, builtInSymbols];
	const lambdaCompletionItems = [getLambdaCompletionItem(expression, rowIndex, columnIndex, allScopes)].filter(isDefined);

	//#region infix function call (bei infix function reference)
	const infixFunctionCall = getInfixFunctionCall(expression);
	if (infixFunctionCall) {
		const prefixArgumentTypeRaw = infixFunctionCall.prefixArgument!.typeInfo!.type;
		let prefixArgumentType: CompileTimeType | undefined;
		if (isParameterReference(prefixArgumentTypeRaw)) {
			const dereferenced = findSymbolInScopesWithBuiltIns(prefixArgumentTypeRaw.name, scopes);
			prefixArgumentType = dereferenced?.symbol.typeInfo?.type;
		}
		else {
			prefixArgumentType = prefixArgumentTypeRaw;
		}
		// hier wird immer eine Funktion ausgewählt (nie ein Typ) - unabhängig davon, ob
		// `expression` die Referenz selbst oder der ganze Aufruf ist (siehe getInfixFunctionCall)
		return symbolsToCompletionItems(allScopes, getFirstArgumentSymbolFilter(prefixArgumentType), 'value', infixFunctionCall);
	}
	//#endregion infix function call (bei infix function reference)

	//#region infix function call hinter einem Wert
	const valueBeforeCursor = getValueBeforeCursor(expression, rowIndex, columnIndex);
	if (valueBeforeCursor) {
		const resultType = getResolvedType('typeInfo' in valueBeforeCursor ? valueBeforeCursor.typeInfo : undefined);
		const isInfixCandidate = getFirstArgumentSymbolFilter(resultType);
		return symbolsToCompletionItems(
			allScopes,
			symbol => isFunctionType(getResolvedType(symbol.typeInfo)) && isInfixCandidate(symbol),
			'value',
			'afterValue');
	}
	//#endregion infix function call hinter einem Wert

	//#region / field reference
	if (expression?.type === 'nestedReference') {
		const dereferencedType = getResolvedType(expression.source?.typeInfo);
		return dereferencedType && getFieldReferenceCompletionItems(dereferencedType);
	}
	//#endregion / field reference

	//#region dictionary literal field
	const dictionaryLiteralFieldCompletionItems = getDictionaryLiteralFieldCompletionItems(expression);
	if (dictionaryLiteralFieldCompletionItems) {
		switch (getFieldNamePositionKind(expression)) {
			case 'exclusive':
				return dictionaryLiteralFieldCompletionItems;
			case 'mixed':
				// positionale Argumente sind der Normalfall, die Feldnamen kommen dahinter
				return [
					...lambdaCompletionItems,
					...symbolsToCompletionItems(allScopes, undefined, positionKind),
					...dictionaryLiteralFieldCompletionItems.map(completionItem => ({
						...completionItem,
						sortText: '2' + (completionItem.sortText ?? completionItem.label),
					})),
				];
			case 'none':
				break;
		}
	}
	//#endregion dictionary literal field

	//#region destructuring definition field
	let destructuringFields: ParseDestructuringFields | undefined;
	if (expression?.type === 'destructuringFields') {
		destructuringFields = expression;
	}
	if (expression?.parent?.type === 'destructuringFields') {
		destructuringFields = expression.parent;
	}
	if (expression?.parent?.parent?.type === 'destructuringFields'
		&& expression.parent.type === 'destructuringField'
		&& expression === expression.parent.name
	) {
		destructuringFields = expression.parent.parent;
	}
	if (destructuringFields) {
		const destructuring = destructuringFields.parent;
		if (destructuring?.type === 'destructuring') {
			const destructuredValue = destructuring.value;
			// schon definierte Felder ausschließen
			const isNotDefinedYet = (symbol: SymbolDefinition, name: string) => {
				return !destructuring.fields.symbols[name];
			};
			switch (destructuredValue?.type) {
				case 'dictionary':
				case 'dictionaryType':
					return symbolsToCompletionItems([destructuredValue.symbols], isNotDefinedYet, positionKind);
				default: {
					const dereferencedType = getResolvedType(destructuredValue?.typeInfo);
					if (isDictionaryLiteralType(dereferencedType)) {
						return dictionaryTypeToCompletionItems(dereferencedType.FieldTypes)
							.filter(completionItem => !destructuring.fields.symbols[completionItem.label]);
					}
					return [];
				}
			}
		}
	}
	//#endregion destructuring definition field

	//#region function literal parameter name
	if (expression?.type === 'parameters') {
		const innerParamsType = getDeclaredResolvedType(expression);
		if (isParametersType(innerParamsType)) {
			const completionItems: CompletionItem[] = [];
			innerParamsType.singleNames.forEach((singleName, index) => {
				const isAlreadyDeclared = expression.singleFields.some(declaredParameter =>
					declaredParameter.source?.name === singleName.name
					|| (!declaredParameter.source && declaredParameter.name.name === singleName.name));
				if (!isAlreadyDeclared) {
					completionItems.push(parameterToCompletionItem(singleName, index, false));
				}
			});
			if (innerParamsType.rest && !expression.rest) {
				completionItems.push(parameterToCompletionItem(innerParamsType.rest, innerParamsType.singleNames.length, true));
			}
			return completionItems;
		}
	}
	//#endregion function literal parameter name

	return [
		...lambdaCompletionItems,
		...symbolsToCompletionItems(allScopes, undefined, positionKind),
	];
}

//#endregion getCompletionItems
