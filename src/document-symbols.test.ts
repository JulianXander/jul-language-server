import { expect } from 'chai';
import { join, resolve } from 'path';
import { ParsedDocuments } from 'jul-compiler/out/checker/checker.js';
import { createInMemoryHost, loadFile } from 'jul-compiler/out/compiler/project-loader.js';
import { DocumentSymbol, SymbolKind } from 'vscode-languageserver-types';
import { getDocumentSymbols } from './document-symbols.js';

const folder = resolve('/document-symbols-test');
const filePath = join(folder, 'main.jul');

function symbolsOf(code: string): DocumentSymbol[] | undefined {
	const documents: ParsedDocuments = {};
	const parsed = loadFile(filePath, documents, createInMemoryHost({ [filePath]: code }));
	if (typeof parsed === 'string') {
		throw new Error(parsed);
	}
	return getDocumentSymbols(parsed);
}

const kindNames: { [kind: number]: string; } = {
	[SymbolKind.Function]: 'Function',
	[SymbolKind.Constant]: 'Constant',
	[SymbolKind.Variable]: 'Variable',
	[SymbolKind.Field]: 'Field',
};

/** Name und Art, die Kinder eingerückt darunter. */
function describeSymbols(symbols: DocumentSymbol[] | undefined, indent = ''): string[] {
	return (symbols ?? []).flatMap(symbol => [
		`${indent}${symbol.name} ${kindNames[symbol.kind] ?? symbol.kind}`,
		...describeSymbols(symbol.children, indent + '\t'),
	]);
}

describe('getDocumentSymbols', () => {
	it('eine Funktion trägt ihre Parameter als Kinder', () => {
		const code = `double = (x: Integer) => x.multiply(2)
answer = 42`;
		expect(describeSymbols(symbolsOf(code))).to.deep.equal([
			'double Function',
			'\tx Variable',
			'answer Constant',
		]);
	});

	it('_ steht nicht in der Gliederung, was darin definiert ist schon', () => {
		const code = `_ = [inner = 1]`;
		expect(describeSymbols(symbolsOf(code))).to.deep.equal([
			'inner Field',
		]);
	});

	it('die Felder eines Dictionary-Typs stehen unter seiner Definition', () => {
		const code = `Person = [
	name: Text
]`;
		expect(describeSymbols(symbolsOf(code))).to.deep.equal([
			'Person Constant',
			'\tname Field',
		]);
	});
});
