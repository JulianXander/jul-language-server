import { expect } from 'chai';
import { checkTypes, ParsedDocuments } from 'jul-compiler/out/checker/checker.js';
import { ReferenceIndex } from 'jul-compiler/out/checker/reference-index.js';
import { parseCode } from 'jul-compiler/out/parser/parser.js';
import { ParseFunctionCall } from 'jul-compiler/out/syntax-tree.js';
import { getParameterIndex } from './util.js';

/** parst code und liefert den zweiten Top-Level-Ausdruck als functionCall zurück */
function getFunctionCall(code: string): ParseFunctionCall {
	const path = 'parameter-index.test.jul';
	const parsed = parseCode(code, path);
	const documents: ParsedDocuments = { [path]: parsed };
	checkTypes(parsed, documents, new ReferenceIndex());
	const functionCall = parsed.checked!.expressions![1];
	if (functionCall?.type !== 'functionCall') {
		throw new Error(`Erwartet functionCall, bekommen ${functionCall?.type}`);
	}
	return functionCall;
}

describe('getParameterIndex', () => {
	const declaration = 'typeFunction = (a: Integer b: Integer): Integer => a\n';

	it('zählt Argumente bei einer positionellen Argumentliste', () => {
		const functionCall = getFunctionCall(`${declaration}typeFunction(1 2)\n`);
		// Cursor auf dem zweiten Argument (Zeile 2, 1-basiert Zeile 1)
		const parameterIndex = getParameterIndex(functionCall, 1, 16, 2);
		expect(parameterIndex).to.equal(1);
	});

	it('zählt Argumente bei benannten Argumenten (dictionary)', () => {
		const functionCall = getFunctionCall(`${declaration}typeFunction(a = 1 b = 2)\n`);
		// Cursor auf dem zweiten Feld "b = 2"
		const parameterIndex = getParameterIndex(functionCall, 1, 21, 2);
		expect(parameterIndex).to.equal(1);
	});
});
