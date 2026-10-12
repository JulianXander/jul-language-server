import { expect } from 'chai';
import { join, resolve } from 'path';
import { ParsedDocuments } from 'jul-compiler/out/checker/checker.js';
import { createInMemoryHost, loadFile } from 'jul-compiler/out/compiler/project-loader.js';
import { getFoldingRanges } from './folding.js';

const folder = resolve('/folding-test');
const filePath = join(folder, 'main.jul');

/** Die Bereiche als [startLine, endLine], aufsteigend nach Start. */
function foldsOf(code: string): [number, number][] {
	const documents: ParsedDocuments = {};
	const host = createInMemoryHost({ [filePath]: code }, { cloneUnchecked: true });
	const parsed = loadFile(filePath, documents, host);
	if (typeof parsed === 'string') {
		throw new Error(parsed);
	}
	return getFoldingRanges(parsed, code.split('\n'))
		.map((range): [number, number] => [range.startLine, range.endLine])
		.sort((a, b) => a[0] - b[0]);
}

describe('folding', () => {
	it('eine mehrzeilige Parameterliste faltet zusammen mit dem Rumpf', () => {
		const code = `f = (
	a: Integer
	b: Integer
) =>
	a + b`;
		expect(foldsOf(code)).to.deep.equal([[0, 4]]);
	});
	it('eine einzeilige Funktion faltet nicht', () => {
		expect(foldsOf('f = (a: Integer) => a')).to.deep.equal([]);
	});
	it('eine Funktion mit einzeiliger Parameterliste faltet über den Rumpf', () => {
		const code = `f = (a: Integer) =>
	b = a + 1
	b`;
		expect(foldsOf(code)).to.deep.equal([[0, 2]]);
	});
	it('eine schließende Klammer in eigener Zeile faltet mit', () => {
		const code = `list = [
	1
	2
]`;
		expect(foldsOf(code)).to.deep.equal([[0, 3]]);
	});
	it('eine Funktion, deren Rumpf mit einer Klammerzeile endet, faltet samt dieser Zeile', () => {
		const code = `f = (a: Integer) =>
	log(
		a
	)`;
		expect(foldsOf(code)).to.deep.equal([[0, 3], [1, 3]]);
	});
	it('ein Destructuring faltet nur die Feldliste, der Quellpfad bleibt sichtbar', () => {
		const code = `(
	a
	b
) = import(§./other.jul§)`;
		expect(foldsOf(code)).to.deep.equal([[0, 2]]);
	});
	it('eine verschachtelte Funktion faltet zusätzlich', () => {
		const code = `f = (a: Integer) =>
	g = (b: Integer) =>
		b + a
	g`;
		expect(foldsOf(code)).to.deep.equal([[0, 3], [1, 2]]);
	});
	it('#region faltet bis #endregion', () => {
		const code = `#region a
x = 1
y = 2
#endregion a`;
		expect(foldsOf(code)).to.deep.equal([[0, 3]]);
	});
});
