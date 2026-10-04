import { expect } from 'chai';
import { join, resolve } from 'path';
import { ParsedDocuments } from 'jul-compiler/out/checker/checker.js';
import { createInMemoryHost, loadFile } from 'jul-compiler/out/compiler/project-loader.js';
import { reportAtCaller } from 'jul-compiler/src/test-util.js';
import { findEmptyLiterals } from './empty-literals.js';

const folder = resolve('/empty-literals-test');
const filePath = join(folder, 'main.jul');

/** die Fundstellen als "zeile:spalte-spalte", 0-basiert */
function emptyLiteralsOf(code: string): string[] {
	const documents: ParsedDocuments = {};
	const host = createInMemoryHost({ [filePath]: code }, { cloneUnchecked: true });
	const parsed = loadFile(filePath, documents, host);
	if (typeof parsed === 'string') {
		throw new Error(parsed);
	}
	return findEmptyLiterals(parsed).map(range =>
		`${range.start.line}:${range.start.character}-${range.end.character}`);
}

const expectEmptyLiterals = reportAtCaller((code: string, expected: string[]) => {
	expect(emptyLiteralsOf(code)).to.deep.equal(expected);
});

describe('empty literals', () => {
	it('findet ein Empty-Literal auf oberster Ebene', () => {
		expectEmptyLiterals(`x = []`, ['0:4-6']);
	});

	it('findet ein Empty-Literal in einer Liste', () => {
		expectEmptyLiterals(`x = [1 []]`, ['0:7-9']);
	});

	it('findet ein Empty-Literal in einem Funktionsrumpf', () => {
		expectEmptyLiterals(`f = () =>
	[]`, ['1:1-3']);
	});

	it('eine gefüllte Liste ist kein Empty-Literal', () => {
		expectEmptyLiterals(`x = [1 2]`, []);
	});
});
