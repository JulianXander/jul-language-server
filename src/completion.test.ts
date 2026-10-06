import { expect } from 'chai';
import { checkTypes, ParsedDocuments } from 'jul-compiler/out/checker/checker.js';
import { ReferenceIndex } from 'jul-compiler/out/checker/reference-index.js';
import { parseCode } from 'jul-compiler/out/parser/parser.js';
import { ParsedFile, ParseFunctionCall, PositionedExpression } from 'jul-compiler/out/syntax-tree.js';
import { getDeclaredResolvedType, getResolvedType } from './util.js';
import { findExpressionInParsedFile } from './symbol-lookup.js';
import { builtInSymbols } from 'jul-compiler/out/checker/checker.js';
import { getArgumentPositionKind, getCompletionSortText, getDictionaryFieldCompletionItemsFromType, getFieldReferenceCompletionItems, getDictionaryLiteralFieldCompletionItems, getFieldNamePositionKind, getExpectedArgumentType, getExpectedPositionKind, getFirstArgumentSymbolFilter, getInfixFunctionCall, getLambdaCompletionItem, getPositionKindForExpectedType, isTypeSymbol } from './completion.js';

function parse(code: string): ParsedFile {
	const path = 'completion.test.jul';
	const parsed = parseCode(code, path);
	const documents: ParsedDocuments = { [path]: parsed };
	checkTypes(parsed, documents, { cloneUnchecked: true, referenceIndex: new ReferenceIndex() });
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

	it('erkennt den value eines Dictionary-Felds als Wert-Position', () => {
		const parsed = parse('x = [a = 1]\n');
		const definition = parsed.checked!.expressions![0];
		if (definition?.type !== 'definition' || definition.value?.type !== 'dictionary') {
			throw new Error('Erwartet definition mit dictionary value');
		}
		const field = definition.value.fields[0];
		if (field?.type !== 'singleDictionaryField') {
			throw new Error(`Erwartet singleDictionaryField, bekommen ${field?.type}`);
		}
		expect(getExpectedPositionKind(field.value)).to.equal('value');
	});

	it('liefert undefined für eine Top-Level-Stelle, an der beides gültig ist', () => {
		const parsed = parse('a\n');
		const expression = parsed.checked!.expressions![0];
		expect(getExpectedPositionKind(expression)).to.equal(undefined);
	});

	it('erkennt die aufgerufene Funktion eines functionCall als Wert-Position', () => {
		const parsed = parse(`f = (a: Integer) :> Integer => a
f(1)
`);
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
		const parsed = parse(`a: Integer = 1
f = (b: Integer) :> Integer => b
g = (b: Text) :> Text => b
`);
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

describe('getExpectedArgumentType', () => {
	/** liefert den functionCall aus der Definition in der angegebenen Zeile */
	function getCallFromDefinition(code: string, rowIndex: number): ParseFunctionCall {
		const parsed = parse(code);
		const definition = parsed.checked!.expressions![rowIndex];
		if (definition?.type !== 'definition' || definition.value?.type !== 'functionCall') {
			throw new Error(`Erwartet definition mit functionCall value, bekommen ${definition?.type}`);
		}
		return definition.value;
	}

	// Realer Fall: in `Or([] )` wird an der Cursorposition ein Typ erwartet (Rest-Parameter
	// "...ChoiceTypes: List(Type)" in core-lib.jul), trotzdem standen Werte weiter oben.
	it('liefert bei einem Rest-Parameter den Elementtyp, nicht den Listentyp', () => {
		const functionCall = getCallFromDefinition('x = Or([] )\n', 0);
		// Cursor hinter dem Leerzeichen, also im zweiten Argument
		expect(getExpectedArgumentType(functionCall, 0, 10)?.julType).to.equal('type');
	});

	it('liefert den Typ des Parameters an der Cursorposition', () => {
		const code = `f = (a: Integer b: Text) :> Integer => a
x = f(1 )
`;
		const functionCall = getCallFromDefinition(code, 1);
		// Cursor im zweiten Argument, dort wird Text erwartet
		expect(getExpectedArgumentType(functionCall, 1, 8)?.julType).to.equal('text');
	});
});

describe('getPositionKindForExpectedType', () => {
	it('macht aus einem erwarteten Typ eine Typ-Position', () => {
		const typeType = getResolvedType(builtInSymbols['Type']?.typeInfo);
		expect(getPositionKindForExpectedType(typeType)).to.equal('type');
	});

	it('macht aus einem erwarteten Wert-Typ eine Wert-Position', () => {
		// ein Typ, dessen Wert selbst nicht ein Typ ist - z.B. Integer als erwarteter Wert
		expect(getPositionKindForExpectedType({ julType: 'integer', isUnresolvedPlaceholder: false })).to.equal('value');
	});

	it('liefert undefined, wenn kein Typ bekannt ist', () => {
		expect(getPositionKindForExpectedType(undefined)).to.equal(undefined);
	});
});

describe('getArgumentPositionKind', () => {
	it('erkennt Or([] <Cursor>) als Typ-Position (Realfall aus der Session)', () => {
		const parsed = parse('x = Or([] )\n');
		const definition = parsed.checked!.expressions![0];
		if (definition?.type !== 'definition' || definition.value?.type !== 'functionCall') {
			throw new Error('Erwartet definition mit functionCall value');
		}
		const list = definition.value.arguments;
		expect(getArgumentPositionKind(list, 0, 10)).to.equal('type');
	});

	it('erkennt einen Wert-Parameter als Wert-Position, auch wenn schon ein Argument getippt ist', () => {
		const parsed = parse(`f = (a: Integer b: Text) :> Integer => a
x = f(1 )
`);
		const definition = parsed.checked!.expressions![1];
		if (definition?.type !== 'definition' || definition.value?.type !== 'functionCall') {
			throw new Error('Erwartet definition mit functionCall value');
		}
		const list = definition.value.arguments;
		expect(getArgumentPositionKind(list, 1, 8)).to.equal('value');
	});

	it('liefert undefined außerhalb einer Argumentliste', () => {
		const parsed = parse('a: Integer = 5\n');
		const expression = parsed.checked!.expressions![0];
		expect(getArgumentPositionKind(expression, 0, 0)).to.equal(undefined);
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

describe('getDictionaryFieldCompletionItemsFromType', () => {
	// Realer Fall: `MyType = [f1: Integer]` gefolgt von `x: MyType = []` - beim Tippen im leeren
	// Dictionary-Literal sollte `f1` als Feld vorgeschlagen werden.
	it('schlägt die Felder eines per Namen referenzierten Dictionary-Typs vor', () => {
		const parsed = parse(`MyType = [f1: Integer]
x: MyType = []
`);
		const definition = parsed.checked!.expressions![1];
		if (definition?.type !== 'definition' || definition.value?.type !== 'empty') {
			throw new Error('Erwartet definition mit empty value');
		}
		const declaredType = getDeclaredResolvedType(definition.value);
		const completionItems = declaredType && getDictionaryFieldCompletionItemsFromType(declaredType);
		expect(completionItems?.map(item => item.label)).to.include('f1');
	});

	// Ein Dictionary-Literal (Wert) braucht für jedes Feld immer einen zugewiesenen Wert - anders
	// als der Dictionary-Typ, wo `f1: Integer` ohne `=` steht.
	it('hängt an den vorgeschlagenen Feldnamen " = " an, da ein Feld im Literal immer einen Wert braucht', () => {
		const parsed = parse(`MyType = [f1: Integer]
x: MyType = []
`);
		const definition = parsed.checked!.expressions![1];
		if (definition?.type !== 'definition' || definition.value?.type !== 'empty') {
			throw new Error('Erwartet definition mit empty value');
		}
		const declaredType = getDeclaredResolvedType(definition.value);
		const completionItems = declaredType && getDictionaryFieldCompletionItemsFromType(declaredType);
		const f1CompletionItem = completionItems?.find(item => item.label === 'f1');
		expect(f1CompletionItem?.insertText).to.equal('f1 = ');
	});
});

describe('getDeclaredResolvedType', () => {
	/** liefert den Wert der Definition in der angegebenen Zeile */
	function getDefinitionValue(code: string, rowIndex: number) {
		const parsed = parse(code);
		const definition = parsed.checked!.expressions![rowIndex];
		if (definition?.type !== 'definition' || !definition.value) {
			throw new Error(`Erwartet definition mit value, bekommen ${definition?.type}`);
		}
		return definition.value;
	}

	// Ein Spread bringt unbekannt viele Argumente mit, danach ist die Position des Parameters
	// nicht mehr bekannt.
	it('liefert nach einem Spread in der Argumentliste keinen Parametertyp', () => {
		const code = `f = (a: Integer b: Text c: Float) :> Integer => a
xs: List(Integer) = [1]
x = f(1 ...xs §a§)
`;
		const functionCall = getDefinitionValue(code, 2);
		if (functionCall.type !== 'functionCall' || functionCall.arguments?.type !== 'list') {
			throw new Error('Erwartet functionCall mit list-Argumenten');
		}
		const text = functionCall.arguments.values[2]!;
		expect(getDeclaredResolvedType(text)).to.equal(undefined);
	});

	it('liefert nach einem Spread in einem Listenliteral keinen Elementtyp', () => {
		const code = `xs: List(Integer) = [1]
l: [Integer Text Float] = [1 ...xs §a§]
`;
		const list = getDefinitionValue(code, 1);
		if (list.type !== 'list') {
			throw new Error(`Erwartet list, bekommen ${list.type}`);
		}
		const text = list.values[2]!;
		expect(getDeclaredResolvedType(text)).to.equal(undefined);
	});

	// Der Parametertyp des Callbacks steht in der core-lib als TypeOf(values)/ElementType und ist
	// erst mit den Argumenten dieses Aufrufs bekannt. Das Symbol values behält den Typ seines Werts,
	// das Element ist also das Literal 1.
	it('liefert für die Parameter eines Callbacks den mit dem Aufruf instanziierten Typ', () => {
		const code = `values: List(Integer) = [1]
x = values.map((v) => v)
`;
		const functionCall = getDefinitionValue(code, 1);
		if (functionCall.type !== 'functionCall' || functionCall.arguments?.type !== 'list') {
			throw new Error('Erwartet functionCall mit list-Argumenten');
		}
		const callback = functionCall.arguments.values[0];
		if (callback?.type !== 'functionLiteral') {
			throw new Error(`Erwartet functionLiteral, bekommen ${callback?.type}`);
		}
		const paramsType = getDeclaredResolvedType(callback.params);
		if (paramsType?.julType !== 'parameters') {
			throw new Error(`Erwartet parameters, bekommen ${paramsType?.julType}`);
		}
		expect(paramsType.singleNames[0]?.type?.julType).to.equal('integerLiteral');
	});
});

describe('getDictionaryLiteralFieldCompletionItems', () => {
	it('schlägt bei einem leeren Dictionary-Literal die Felder des erwarteten Typs vor', () => {
		const parsed = parse(`MyType = [f1: Integer]
x: MyType = []
`);
		const definition = parsed.checked!.expressions![1];
		if (definition?.type !== 'definition' || definition.value?.type !== 'empty') {
			throw new Error('Erwartet definition mit empty value');
		}
		const completionItems = getDictionaryLiteralFieldCompletionItems(definition.value);
		expect(completionItems?.map(item => item.label)).to.include('f1');
	});

	// Bei List(X) verlangt jedes Element dasselbe, auch hinter einem Spread.
	it('schlägt hinter einem Spread in einer List die Felder des Elementtyps vor', () => {
		const parsed = parse(`Button = [label: Text]
defaults: List(Button) = [[label = §a§]]
buttons: List(Button) = [...defaults []]
`);
		const definition = parsed.checked!.expressions![2];
		if (definition?.type !== 'definition' || definition.value?.type !== 'list') {
			throw new Error('Erwartet definition mit list value');
		}
		const completionItems = getDictionaryLiteralFieldCompletionItems(definition.value.values[1]);
		expect(completionItems?.map(item => item.label)).to.include('label');
	});

	// Realer Fall: sobald der erste Buchstabe eines Feldnamens getippt ist, parst `[f]` nicht mehr
	// als leeres/dictionary-Literal, sondern als list mit einer reference darin - die Vervollständigung
	// verschwindet dadurch komplett.
	it('schlägt beim Tippen eines Feldnamens weiterhin die Felder des erwarteten Typs vor', () => {
		const parsed = parse(`MyType = [f1: Integer]
x: MyType = [f]
`);
		const definition = parsed.checked!.expressions![1];
		if (definition?.type !== 'definition' || definition.value?.type !== 'list') {
			throw new Error('Erwartet definition mit list value');
		}
		const reference = definition.value.values[0];
		if (reference?.type !== 'reference') {
			throw new Error(`Erwartet reference, bekommen ${reference?.type}`);
		}
		const completionItems = getDictionaryLiteralFieldCompletionItems(reference);
		expect(completionItems?.map(item => item.label)).to.include('f1');
	});

	// Realer Fall (tic-tac-toe): hinter einem Feld mit `=` bleibt das Literal ein unaufgelöstes `data`.
	it('schlägt hinter einem schon gesetzten Feld die übrigen Felder des erwarteten Typs vor', () => {
		const parsed = parse(`MyType = [f1: Integer f2: Integer]
x: MyType = [
	f1 = 1
	f
]
`);
		const definition = parsed.checked!.expressions![1];
		if (definition?.type !== 'definition' || definition.value?.type !== 'data') {
			throw new Error(`Erwartet definition mit data value, bekommen ${definition?.type}`);
		}
		const field = definition.value.fields.at(-1);
		const completionItems = getDictionaryLiteralFieldCompletionItems(field?.name);
		expect(completionItems?.map(item => item.label)).to.deep.equal(['f2']);
	});
});

describe('getFieldNamePositionKind', () => {
	// Die zuletzt getippte reference im Aufruf in der letzten Zeile von code.
	function lastArgumentReference(code: string): PositionedExpression {
		const parsed = parse(code);
		const functionCall = parsed.checked!.expressions!.at(-1);
		if (functionCall?.type !== 'functionCall') {
			throw new Error(`Erwartet functionCall, bekommen ${functionCall?.type}`);
		}
		const argumentsExpression = functionCall.arguments;
		const lastArgument = argumentsExpression?.type === 'list'
			? argumentsExpression.values.at(-1)
			: argumentsExpression?.type === 'binding'
				? argumentsExpression.fields.at(-1)?.name
				: undefined;
		if (lastArgument?.type !== 'reference') {
			throw new Error(`Erwartet reference, bekommen ${lastArgument?.type}`);
		}
		return lastArgument;
	}

	// Realer Fall aus jul-examples/ui/tic-tac-toe: bei `subscribeEvent(res)` wurde `resetElement`
	// nicht vorgeschlagen, sondern nur die Parameternamen. Als einziges Argument kann `res` aber
	// ebenso gut der Anfang eines positionalen Arguments sein.
	it('erlaubt beim einzigen Argument eines Aufrufs Feldname und Wert', () => {
		const reference = lastArgumentReference(`f = (element: Integer event: Integer) => element
resetElement = 5
f(res)
`);
		expect(getFieldNamePositionKind(reference)).to.equal('mixed');
	});

	// Positionale und benannte Argumente lassen sich nicht mischen.
	it('erlaubt nach einem positionalen Argument keinen Feldnamen', () => {
		const reference = lastArgumentReference(`f = (element: Integer event: Integer) => element
resetElement = 5
f(1 res)
`);
		expect(getFieldNamePositionKind(reference)).to.equal('none');
	});

	it('erlaubt nach einem benannten Argument nur einen Feldnamen', () => {
		const reference = lastArgumentReference(`f = (element: Integer event: Integer) => element
f(element = 1 ev)
`);
		expect(getFieldNamePositionKind(reference)).to.equal('exclusive');
	});

	// Das Argument ist schon ein Dictionary-Literal, der Cursor steht in dessen Klammern.
	it('erlaubt im Dictionary-Literal als einzigem Argument nur einen Feldnamen', () => {
		const parsed = parse(`MyType = [f1: Integer]
f = (value: MyType) => value
f([])
`);
		const functionCall = parsed.checked!.expressions![2];
		if (functionCall?.type !== 'functionCall' || functionCall.arguments?.type !== 'list') {
			throw new Error('Erwartet functionCall mit list arguments');
		}
		expect(getFieldNamePositionKind(functionCall.arguments.values[0])).to.equal('exclusive');
	});

	// Der erwartete Typ ist ein Dictionary, eine List wäre dort ein Typfehler.
	it('erlaubt in einem Literal mit erwartetem Dictionary-Typ nur einen Feldnamen', () => {
		const parsed = parse(`MyType = [f1: Integer]
x: MyType = [f]
`);
		const definition = parsed.checked!.expressions![1];
		if (definition?.type !== 'definition' || definition.value?.type !== 'list') {
			throw new Error('Erwartet definition mit list value');
		}
		expect(getFieldNamePositionKind(definition.value.values[0])).to.equal('exclusive');
	});
});

describe('getInfixFunctionCall', () => {
	// Realer Fall aus C:\Projects\privat\yugioh\src\ui\main-menu.jul:23 - "decks." während des
	// Tippens: der functionCall hat nur prefixArgument, functionExpression fehlt noch komplett.
	it('erkennt einen unvollständigen Infix-Aufruf ohne Funktionsname (nur prefixArgument)', () => {
		const parsed = parse(`decks: Or([] List(Integer)) = []
decks.
`);
		const functionCall = parsed.checked!.expressions![1];
		if (functionCall?.type !== 'functionCall') {
			throw new Error(`Erwartet functionCall, bekommen ${functionCall?.type}`);
		}
		expect(functionCall.functionExpression).to.equal(undefined);
		expect(getInfixFunctionCall(functionCall)).to.equal(functionCall);
	});
});

describe('getFieldReferenceCompletionItems', () => {
	// Was nach x/ angeboten wird, für das Symbol name aus code.
	function fieldReferenceItems(code: string, name: string): { label: string; detail?: string; }[] {
		const parsed = parse(code);
		const type = getResolvedType(parsed.checked!.symbols[name]!.typeInfo);
		return (type ? getFieldReferenceCompletionItems(type) : [])
			.map(item => ({ label: item.label, detail: item.detail }));
	}

	// Typeigenschaften gibt es nur über einen Typwert, ein Stream-Wert hat nur getValue.
	it('bietet bei einem Stream-Wert nur getValue an', () => {
		expect(fieldReferenceItems('s$ = create$(Integer 1)', 's$').map(item => item.label))
			.to.deep.equal(['getValue']);
	});

	it('bietet bei einem Funktionswert nichts an', () => {
		expect(fieldReferenceItems('f = (q: Integer) => §a§', 'f')).to.deep.equal([]);
	});

	it('bietet bei einem Stream-Typ ValueType als Typwert an', () => {
		expect(fieldReferenceItems('S = Stream(Integer)', 'S'))
			.to.deep.equal([{ label: 'ValueType', detail: 'TypeOf(Integer)' }]);
	});

	it('bietet bei einem List-Typ ElementType als Typwert an', () => {
		expect(fieldReferenceItems('L = List(Integer)', 'L'))
			.to.deep.equal([{ label: 'ElementType', detail: 'TypeOf(Integer)' }]);
	});

	it('bietet bei einem Funktionstyp ParamsType und ReturnType als Typwerte an', () => {
		expect(fieldReferenceItems('F = (q: Integer) :> Text', 'F').map(item => item.label))
			.to.deep.equal(['ParamsType', 'ReturnType']);
	});
});

describe('getLambdaCompletionItem', () => {
	/** `|` markiert die Cursorposition und wird entfernt */
	function getLambdaItem(codeWithCursor: string) {
		const lines = codeWithCursor.split('\n');
		const rowIndex = lines.findIndex(line => line.includes('|'));
		const columnIndex = lines[rowIndex]!.indexOf('|');
		const parsed = parse(codeWithCursor.replace('|', ''));
		const { expression, scopes } = findExpressionInParsedFile(parsed, rowIndex, columnIndex);
		return getLambdaCompletionItem(expression, rowIndex, columnIndex, [...scopes, builtInSymbols]);
	}

	it('bietet an einer Funktionsposition das Lambda mit den festen Parameternamen an', () => {
		expect(getLambdaItem('x = [].map(|)\n')?.insertText).to.equal('(value index) => $0');
	});

	it('bietet es auch mit einem Argument im Aufruf ohne Infix an', () => {
		expect(getLambdaItem('x = map([1] |)\n')?.insertText).to.equal('(value index) => $0');
	});

	it('vergibt einen Alias, wenn der Name im Scope schon vergeben ist', () => {
		expect(getLambdaItem('value = 1\nx = [].map(|)\n')?.insertText).to.equal('(${1:value2} = value index) => $0');
	});

	it('zählt den Alias hoch, wenn auch der Aliasname vergeben ist', () => {
		expect(getLambdaItem('value = 1\nvalue2 = 2\nx = [].map(|)\n')?.insertText).to.equal('(${1:value3} = value index) => $0');
	});

	it('nummeriert die Alias-Tabstops in Parameterreihenfolge vor dem Funktionskörper', () => {
		expect(getLambdaItem('value = 1\nindex = 2\nx = [].map(|)\n')?.insertText).to.equal('(${1:value2} = value ${2:index2} = index) => $0');
	});

	it('bietet kein Lambda an, wenn keine Funktion erwartet wird', () => {
		expect(getLambdaItem('f = (a: Integer) :> Integer => a\nx = f(|)\n')).to.equal(undefined);
	});
});

// Realer Fall: in `[].map()` wurden `values` und `transform` als Feldnamen vorgeschlagen, obwohl
// `values` schon das Präfixargument ist und die leere Argumentliste auch ein positionales
// Argument (z.B. ein Lambda) erlaubt.
describe('leere Argumentliste eines Infix-Aufrufs', () => {
	function emptyArguments(code: string): PositionedExpression {
		const parsed = parse(code);
		const functionCall = parsed.checked!.expressions!.at(-1);
		if (functionCall?.type !== 'functionCall' || functionCall.arguments?.type !== 'empty') {
			throw new Error(`Erwartet functionCall mit empty Argumenten, bekommen ${functionCall?.type}`);
		}
		return functionCall.arguments;
	}

	it('schlägt das Präfixargument nicht als Feldname vor', () => {
		const completionItems = getDictionaryLiteralFieldCompletionItems(emptyArguments('[].map()\n'));
		expect(completionItems?.map(item => item.label)).to.deep.equal(['transform']);
	});

	it('lässt neben Feldnamen auch positionale Argumente zu', () => {
		expect(getFieldNamePositionKind(emptyArguments('[].map()\n'))).to.equal('mixed');
	});
});
