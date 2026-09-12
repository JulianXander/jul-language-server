import { expect } from 'chai';
import { checkTypes, ParsedDocuments } from 'jul-compiler/out/checker/checker.js';
import { ReferenceIndex } from 'jul-compiler/out/checker/reference-index.js';
import { parseCode } from 'jul-compiler/out/parser/parser.js';
import { ParsedFile } from 'jul-compiler/out/syntax-tree.js';
import { getResolvedType } from './util.js';
import { builtInSymbols } from 'jul-compiler/out/checker/checker.js';
import { getCompletionSortText, getExpectedPositionKind, getFirstArgumentSymbolFilter, getInfixFunctionCall, isTypeSymbol } from './completion.js';

function parse(code: string): ParsedFile {
	const path = 'completion.test.jul';
	const parsed = parseCode(code, path);
	const documents: ParsedDocuments = { [path]: parsed };
	checkTypes(parsed, documents, new ReferenceIndex());
	return parsed;
}

describe('getExpectedPositionKind', () => {
	it('erkennt den typeGuard einer definition als Typ-Position', () => {
		const parsed = parse('a: Integer = 5\n');
		const definition = parsed.checked!.expressions![0];
		if (definition?.type !== 'definition') {
			throw new Error(`Erwartet definition, bekommen ${definition?.type}`);
		}
		expect(getExpectedPositionKind(definition.typeGuard)).to.equal('type');
	});

	it('erkennt den value einer definition als Wert-Position', () => {
		const parsed = parse('a: Integer = 5\n');
		const definition = parsed.checked!.expressions![0];
		if (definition?.type !== 'definition') {
			throw new Error(`Erwartet definition, bekommen ${definition?.type}`);
		}
		expect(getExpectedPositionKind(definition.value)).to.equal('value');
	});

	it('erkennt den typeGuard eines Parameters als Typ-Position', () => {
		const parsed = parse('f = (a: Integer) :> Integer => a\n');
		const definition = parsed.checked!.expressions![0];
		if (definition?.type !== 'definition' || definition.value?.type !== 'functionLiteral') {
			throw new Error('Erwartet definition mit functionLiteral value');
		}
		const params = definition.value.params;
		if (params.type !== 'parameters') {
			throw new Error(`Erwartet parameters, bekommen ${params.type}`);
		}
		const parameter = params.singleFields[0];
		expect(getExpectedPositionKind(parameter?.typeGuard)).to.equal('type');
	});

	it('erkennt den returnType eines functionLiteral als Typ-Position', () => {
		const parsed = parse('f = (a: Integer) :> Integer => a\n');
		const definition = parsed.checked!.expressions![0];
		if (definition?.type !== 'definition' || definition.value?.type !== 'functionLiteral') {
			throw new Error('Erwartet definition mit functionLiteral value');
		}
		expect(getExpectedPositionKind(definition.value.returnType)).to.equal('type');
	});

	it('erkennt den typeGuard eines Dictionary-Felds als Typ-Position, den value als Wert-Position', () => {
		const parsed = parse('x = [a: Integer = 1]\n');
		const definition = parsed.checked!.expressions![0];
		if (definition?.type !== 'definition' || definition.value?.type !== 'dictionary') {
			throw new Error('Erwartet definition mit dictionary value');
		}
		const field = definition.value.fields[0];
		if (field?.type !== 'singleDictionaryField') {
			throw new Error(`Erwartet singleDictionaryField, bekommen ${field?.type}`);
		}
		expect(getExpectedPositionKind(field.typeGuard)).to.equal('type');
		expect(getExpectedPositionKind(field.value)).to.equal('value');
	});

	it('liefert undefined für eine Top-Level-Stelle, an der beides gültig ist', () => {
		const parsed = parse('a\n');
		const expression = parsed.checked!.expressions![0];
		expect(getExpectedPositionKind(expression)).to.equal(undefined);
	});

	it('erkennt die aufgerufene Funktion eines functionCall als Wert-Position', () => {
		const parsed = parse('f = (a: Integer) :> Integer => a\nf(1)\n');
		const functionCall = parsed.checked!.expressions![1];
		if (functionCall?.type !== 'functionCall') {
			throw new Error(`Erwartet functionCall, bekommen ${functionCall?.type}`);
		}
		expect(getExpectedPositionKind(functionCall.functionExpression)).to.equal('value');
	});
});

describe('getCompletionSortText', () => {
	it('liefert kein sortText, wenn die Position unbekannt ist', () => {
		expect(getCompletionSortText('Integer', true, undefined)).to.equal(undefined);
	});

	it('zieht Typ-Symbole an einer Typ-Position nach vorne', () => {
		expect(getCompletionSortText('MyType', true, 'type')).to.equal('0MyType');
		expect(getCompletionSortText('someValue', false, 'type')).to.equal('1someValue');
	});

	it('zieht Wert-Symbole an einer Wert-Position nach vorne', () => {
		expect(getCompletionSortText('someValue', false, 'value')).to.equal('0someValue');
		expect(getCompletionSortText('MyType', true, 'value')).to.equal('1MyType');
	});
});

describe('getFirstArgumentSymbolFilter', () => {
	it('lässt nur Funktionen durch, deren erster Parameter den Argumenttyp annimmt', () => {
		const parsed = parse('a: Integer = 1\nf = (b: Integer) :> Integer => b\ng = (b: Text) :> Text => b\n');
		const symbols = parsed.checked!.symbols;
		const prefixArgumentType = getResolvedType(symbols['a']?.typeInfo);
		const filter = getFirstArgumentSymbolFilter(prefixArgumentType);
		expect(filter(symbols['f']!)).to.equal(true);
		expect(filter(symbols['g']!)).to.equal(false);
	});

	it('lässt nichts durch, wenn der Argumenttyp unbekannt ist', () => {
		const parsed = parse('f = (b: Integer) :> Integer => b\n');
		const filter = getFirstArgumentSymbolFilter(undefined);
		expect(filter(parsed.checked!.symbols['f']!)).to.equal(false);
	});
});

describe('isTypeSymbol', () => {
	it('erkennt ein Symbol, dessen Wert selbst ein Typ ist', () => {
		expect(isTypeSymbol(getResolvedType(builtInSymbols['Integer']?.typeInfo))).to.equal(true);
	});

	// Realer Fall: bei "decks." standen And/Or/List zwischen den Wert-Funktionen (all, assume),
	// weil ihr eigener Typ ein Funktionstyp ist und nicht TypeOf(...). Sie liefern aber einen Typ
	// (core-lib.jul: "(...ChoiceTypes: List(Type)) :> Type") und gehören damit auf die Typ-Seite.
	it('erkennt einen Typkonstruktor, also eine Funktion die einen Typ liefert', () => {
		expect(isTypeSymbol(getResolvedType(builtInSymbols['And']?.typeInfo))).to.equal(true);
	});

	it('erkennt eine normale Wertfunktion nicht als Typ-Symbol', () => {
		expect(isTypeSymbol(getResolvedType(builtInSymbols['log']?.typeInfo))).to.equal(false);
	});
});

describe('getInfixFunctionCall', () => {
	// Realer Fall aus C:\Projects\privat\yugioh\src\ui\main-menu.jul:23 - "decks." während des
	// Tippens: der functionCall hat nur prefixArgument, functionExpression fehlt noch komplett.
	it('erkennt einen unvollständigen Infix-Aufruf ohne Funktionsname (nur prefixArgument)', () => {
		const parsed = parse('decks: Or([] List(Integer)) = []\ndecks.\n');
		const functionCall = parsed.checked!.expressions![1];
		if (functionCall?.type !== 'functionCall') {
			throw new Error(`Erwartet functionCall, bekommen ${functionCall?.type}`);
		}
		expect(functionCall.functionExpression).to.equal(undefined);
		expect(getInfixFunctionCall(functionCall)).to.equal(functionCall);
	});
});
