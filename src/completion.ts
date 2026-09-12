import {
	getTypeError,
	isFunctionType,
	isListType,
	isParametersType,
	isTupleType,
	isTypeOfType,
} from 'jul-compiler/out/checker/checker.js';
import { CompileTimeType, ParseFunctionCall, PositionedExpression, SymbolDefinition } from 'jul-compiler/out/syntax-tree.js';
import { getResolvedType } from './util.js';

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
