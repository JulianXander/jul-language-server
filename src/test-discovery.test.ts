import { expect } from 'chai';
import { join, resolve } from 'path';
import { ParsedDocuments } from 'jul-compiler/out/checker/checker.js';
import { createInMemoryHost, loadFile } from 'jul-compiler/out/project-loader.js';
import { DiscoveredTest, findTests } from './test-discovery.js';

const folder = resolve('/test-discovery-test');

function findTestsIn(code: string, fileName: string = 'a.test.jul'): DiscoveredTest[] {
	const filePath = join(folder, fileName);
	const documents: ParsedDocuments = {};
	const host = createInMemoryHost({ [filePath]: code }, { cloneUnchecked: true });
	const parsed = loadFile(filePath, documents, host);
	if (typeof parsed === 'string') {
		throw new Error(parsed);
	}
	return findTests(parsed);
}

describe('findTests', () => {
	it('findet Tests mit literalem Namen samt Range des Aufrufs', () => {
		expect(findTestsIn('test(§a§ () => true)\n\ntest(\n\t§b§\n\t() => true\n)')).to.deep.equal([
			{
				name: 'a',
				range: { start: { line: 0, character: 0 }, end: { line: 0, character: 20 } },
			},
			{
				name: 'b',
				range: { start: { line: 2, character: 0 }, end: { line: 5, character: 1 } },
			},
		]);
	});
	it('findet benannte Argumente und die Präfixform', () => {
		expect(findTestsIn('test(callback = () => true name = §a§)\n§b§.test(() => true)').map(test => test.name))
			.to.deep.equal(['a', 'b']);
	});
	it('liefert für Nicht-Testdateien nichts', () => {
		expect(findTestsIn('test(§a§ () => true)', 'a.jul')).to.deep.equal([]);
	});
	it('übergeht Tests in Funktionen', () => {
		expect(findTestsIn('f = () =>\n\ttest(§a§ () => true)')).to.deep.equal([]);
	});
	it('übergeht Tests ohne literalen Namen', () => {
		expect(findTestsIn('name = §a§\ntest(name () => true)')).to.deep.equal([]);
	});
	it('nimmt bei doppeltem Namen nur das erste Vorkommen', () => {
		expect(findTestsIn('test(§a§ () => true)\ntest(§a§ () => false)').map(test => test.range.start.line))
			.to.deep.equal([0]);
	});
});
