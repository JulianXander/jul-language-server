import { ParseFunctionCall } from 'jul-compiler/out/syntax-tree.js';

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
