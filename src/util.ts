import { isFunctionType, isParametersType, resolvePlaceholders } from 'jul-compiler/out/checker/checker.js';
import { CompileTimeType, ParseFunctionCall, TypeInfo } from 'jul-compiler/out/syntax-tree.js';

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
