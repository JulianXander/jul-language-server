import { expect } from 'chai';
import { join, resolve } from 'path';
import { ParsedDocuments } from 'jul-compiler/out/checker/checker.js';
import { createInMemoryHost, loadFile } from 'jul-compiler/out/compiler/project-loader.js';
import { reportAtCaller } from 'jul-compiler/src/test-util.js';
import { getSemanticTokens, semanticTokenLegend } from './semantic-tokens.js';

const folder = resolve('/semantic-tokens-test');
const filePath = join(folder, 'main.jul');

/**
 * Die Tokens als "name: typ(modifier,...)" in Quelltextreihenfolge, readonly weggelassen:
 * das trägt jede Bindung, es würde jeden Eintrag verlängern, ohne etwas zu unterscheiden.
 */
function tokensOf(code: string, otherFiles: Record<string, string> = {}): string[] {
	const documents: ParsedDocuments = {};
	const host = createInMemoryHost({ [filePath]: code, ...otherFiles }, { cloneUnchecked: true });
	const parsed = loadFile(filePath, documents, host);
	if (typeof parsed === 'string') {
		throw new Error(parsed);
	}
	const rows = code.split('\n');
	const data = getSemanticTokens(parsed).data;
	const result: string[] = [];
	let line = 0;
	let character = 0;
	for (let index = 0; index < data.length; index += 5) {
		const [deltaLine, deltaCharacter, length, tokenType, tokenModifiers] = data.slice(index, index + 5) as [number, number, number, number, number];
		line += deltaLine;
		character = deltaLine ? deltaCharacter : character + deltaCharacter;
		const modifiers = semanticTokenLegend.tokenModifiers
			.filter((modifier, bit) => modifier !== 'readonly' && tokenModifiers & (1 << bit));
		const text = rows[line]!.slice(character, character + length);
		result.push(`${text}: ${semanticTokenLegend.tokenTypes[tokenType]}(${modifiers.join(',')})`);
	}
	return result;
}

const expectToken = reportAtCaller((code: string, expected: string) => {
	expect(tokensOf(code)).to.include(expected);
});

describe('semantic tokens', () => {
	it('eine Funktion ist function', () => {
		expectToken(`f = (a: Integer) => a
x = f(1)`, 'f: function()');
	});

	it('ein Typ ist type', () => {
		expectToken(`x: Integer = 1`, 'Integer: type(defaultLibrary)');
	});

	it('ein eingebauter Typkonstruktor ist typeConstructor', () => {
		expectToken(`x: List(Integer) = [1]`, 'List: typeConstructor(defaultLibrary)');
	});

	it('Or ist typeConstructor', () => {
		expectToken(`x: Or(Integer Text) = 1`, 'Or: typeConstructor(defaultLibrary)');
	});

	it('eine selbst definierte Funktion, die einen Typ liefert, ist an der Definition typeConstructor', () => {
		expectToken(`Pair = (T: Type) => List(T)`, 'Pair: typeConstructor(declaration)');
	});

	it('eine selbst definierte Funktion, die einen Typ liefert, ist an der Referenz typeConstructor', () => {
		expectToken(`Pair = (T: Type) => List(T)
x: Pair(Integer) = [1]`, 'Pair: typeConstructor()');
	});

	it('ein Parameter vom Typ Type ist an der Deklaration type', () => {
		expectToken(`f = (T: Type) => 0`, 'T: type(declaration)');
	});

	it('ein Parameter vom Typ Type ist an der Referenz type', () => {
		expectToken(`f = (T: Type a: T) => a`, 'T: type()');
	});

	it('ein Parameter mit Wert-Typ bleibt parameter', () => {
		expectToken(`f = (a: Integer) => a`, 'a: parameter(declaration)');
	});

	it('ein importierter Typ im Destructuring ist type', () => {
		const otherFiles = { [join(folder, 'other.jul')]: 'GameState = Or(1 2)\nf = (a: Integer) => a' };
		const tokens = tokensOf(`(
	f
	GameState
) = import(§./other.jul§)`, otherFiles);
		expect(tokens).to.include('GameState: type(declaration)');
		expect(tokens).to.include('f: function(declaration)');
	});

	it('eine Funktion, die eine Funktion liefert, bleibt function', () => {
		expectToken(`f = () => (x: Integer) => x
g = f()`, 'f: function()');
	});

	it('eine Funktion, deren Rückgabe Any ist, bleibt function', () => {
		expectToken(`f = (a: Any) => a
x = f(1)`, 'f: function()');
	});
});
