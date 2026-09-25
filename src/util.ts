import {
	dereferenceIndexFromObject,
	dereferenceNameFromObject,
	isFunctionType,
	isParametersType,
	resolvePlaceholders,
} from 'jul-compiler/out/checker/checker.js';
import { Positioned } from 'jul-compiler/out/compiler-errors.js';
import { getCheckedEscapableName } from 'jul-compiler/out/parser/parser-utils.js';
import {
	CompileTimeType,
	ParseFunctionCall,
	PositionedExpression,
	TypeInfo,
} from 'jul-compiler/out/syntax-tree.js';
import { Range } from 'vscode-languageserver';
import { URI } from 'vscode-uri';

export function positionedToRange(positioned: Positioned): Range {
	return {
		start: {
			line: positioned.startRowIndex,
			character: positioned.startColumnIndex,
		},
		end: {
			line: positioned.endRowIndex,
			character: positioned.endColumnIndex,
		},
	};
}

export function pathToUri(path: string): string {
	return URI.file(path).toString();
}

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

/**
 * Der Typ, den die Stelle verlangt, an der der Ausdruck steht. Den merkt sich der Checker am
 * Ausdruck (expectedType), samt der Zuordnung von Argumenten, Elementen und Feldern. Für
 * Referenzen und Aufrufe ist es dagegen der eigene Typ.
 */
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
	if ('expectedType' in expression && expression.expectedType) {
		return { type: expression.expectedType };
	}
	// Stellen, an denen der Checker keinen erwarteten Typ merkt.
	switch (expression.parent?.type) {
		case 'definition':
			// Ohne Typguard verlangt die Definition nichts, angezeigt wird der eigene Typ des Werts.
			return expression.parent.value === expression && !expression.parent.typeGuard
				? expression.typeInfo
				: undefined;
		case 'functionCall': {
			// Das Präfix-Argument wird inferiert, bevor die aufgerufene Funktion bekannt ist.
			if (expression.parent.prefixArgument === expression) {
				const prefixArgumentType = getPrefixArgumentDeclaredType(expression.parent);
				return prefixArgumentType && { type: prefixArgumentType };
			}
			// Die Argumentliste als Ganzes, z.B. für die Vervollständigung benannter Argumente.
			if (expression.parent.arguments !== expression) {
				return undefined;
			}
			const functionType = getResolvedType(expression.parent.functionExpression?.typeInfo);
			if (!functionType || !isFunctionType(functionType)) {
				return undefined;
			}
			return { type: functionType.ParamsType };
		}
		default:
			return undefined;
	}
}

