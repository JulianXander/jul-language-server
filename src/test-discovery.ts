import { Range } from 'vscode-languageserver';
import { getTestName } from 'jul-compiler/out/parser/parser-utils.js';
import { ParsedFile } from 'jul-compiler/out/syntax-tree.js';
import { isTestFilePath } from 'jul-compiler/out/util.js';
import { positionedToRange } from './util.js';

export interface DiscoveredTest {
	name: string;
	/**
	 * Der ganze test-Aufruf.
	 */
	range: Range;
}

/**
 * Die Tests einer *.test.jul-Datei für den Test Explorer: test-Aufrufe auf oberster Ebene mit
 * literalem Namen. Alles andere meldet der Checker (JUL2701/JUL2702) und hat keine feste Identität.
 * Bei doppeltem Namen (JUL2703) zählt nur das erste Vorkommen, der Name ist die Identität.
 */
export function findTests(parsed: ParsedFile): DiscoveredTest[] {
	if (!isTestFilePath(parsed.filePath)) {
		return [];
	}
	const expressions = (parsed.checked ?? parsed.unchecked).expressions ?? [];
	const tests: DiscoveredTest[] = [];
	expressions.forEach(expression => {
		if (expression.type !== 'functionCall'
			|| expression.functionExpression?.type !== 'reference'
			|| expression.functionExpression.name.name !== 'test') {
			return;
		}
		const name = getTestName(expression);
		if (name === undefined
			|| tests.some(test => test.name === name)) {
			return;
		}
		tests.push({ name: name, range: positionedToRange(expression) });
	});
	return tests;
}
