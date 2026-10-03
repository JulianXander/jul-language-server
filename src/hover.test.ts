import { expect } from 'chai';
import { join, resolve } from 'path';
import { ParsedDocuments } from 'jul-compiler/out/checker/checker.js';
import { createInMemoryHost, loadFile } from 'jul-compiler/out/compiler/project-loader.js';
import { getHover } from './hover.js';

const folder = resolve('/hover-test');
const filePath = join(folder, 'main.jul');
const cursor = '¦';

/** Hover-Text an der Stelle von ¦, das Zeichen selbst wird vor dem Parsen entfernt. */
function hoverAt(codeWithCursor: string): string | undefined {
	const rows = codeWithCursor.split('\n');
	const rowIndex = rows.findIndex(row => row.includes(cursor));
	if (rowIndex === -1) {
		throw new Error(`${cursor} fehlt im Code`);
	}
	const columnIndex = rows[rowIndex]!.indexOf(cursor);
	const code = codeWithCursor.replace(cursor, '');
	const documents: ParsedDocuments = {};
	const host = createInMemoryHost({ [filePath]: code }, { cloneUnchecked: true });
	const parsed = loadFile(filePath, documents, host);
	if (typeof parsed === 'string') {
		throw new Error(parsed);
	}
	return getHover(parsed, rowIndex, columnIndex, folder, documents)?.value;
}

/** So rendert getTypeMarkdown einen Typ ohne Beschreibung. */
function typeMarkdown(typeString: string): string {
	return `\`\`\`jul\n${typeString}\n\`\`\`\n`;
}

describe('hover', () => {
	it('eine Referenz zeigt den Typ ihrer Definition', () => {
		const code = `a = 1
b = ¦a`;
		expect(hoverAt(code)).to.equal(typeMarkdown('1'));
	});

	it('ein per Destructuring aus einer Liste gebundener Name zeigt seinen Typ', () => {
		const code = `list = [1 2]
(fi¦rst) = list`;
		expect(hoverAt(code)).to.equal(typeMarkdown('1'));
	});

	it('ein per Destructuring aus einem Dictionary gebundener Name zeigt seinen Typ', () => {
		const code = `dict = [fieldName = 1]
(field¦Name) = dict`;
		expect(hoverAt(code)).to.equal(typeMarkdown('1'));
	});

	it('ein per Destructuring im Funktionsrumpf gebundener Name zeigt seinen Typ', () => {
		const code = `f = (a: List(Integer)) =>
	(¦x y) = a
	[x y]`;
		expect(hoverAt(code)).to.equal(typeMarkdown('Integer'));
	});

	it('die aufgerufene Funktion zeigt Parametertypen, die auf ein Argument verweisen, am Aufruf aufgelöst', () => {
		const code = `s$ = create$(Or([] Integer) [])
s$.pu¦sh(1)`;
		expect(hoverAt(code)).to.equal(typeMarkdown(`(
  stream$: Stream(Or(Empty Integer))
  value: Or(Empty Integer)
) ~> Empty`));
	});

	it('ein Parameter, auf dessen ValueType verwiesen wird, zeigt am Aufruf den eingesetzten Stream-Typ', () => {
		const code = `s$ = create$(Integer 1)
s$.subscri¦be((value: Integer) => log(value))`;
		expect(hoverAt(code)).to.equal(typeMarkdown(`(
  stream$: Stream(Integer)
  listener: (value: Integer) :> Any
) ~> Empty`));
	});

	it('ein Parameter, auf dessen ElementType verwiesen wird, behält am Aufruf das Empty seiner Deklaration', () => {
		const code = `f = (values: Or([] List(Text))) =>
	values.fil¦ter((value: Text) => true)`;
		expect(hoverAt(code)).to.match(/values: Or\(Empty List\(Text\)\)/);
	});

	it('ein leeres Argument setzt in den Parameter, auf den verwiesen wird, nichts ein', () => {
		const code = `[].fil¦ter((value: Any) => true)`;
		expect(hoverAt(code)).to.match(/values: Or\(Empty List\(Any\)\)/);
	});

	it('ein Parameter, auf dessen ReturnType verwiesen wird, zeigt am Aufruf den Rückgabetyp des Callbacks', () => {
		const code = `s$ = create$(Integer 1)
s$.ma¦p$((value: Integer) => §text§)`;
		expect(hoverAt(code)).to.match(/transform\$: \(value: Integer\) :> §text§/);
	});

	it('die Selbstanwendung einer rekursiven Typfunktion zeigt sich mit ihrem Argument', () => {
		const code = `Tree = (T: Type) => [value: T children: Or([] List(Tree(T)))]
f = (tree: Tree(Integer)) =>
	tr¦ee`;
		expect(hoverAt(code)).to.match(/children: Or\(Empty List\(Tree\(Integer\)\)\)/);
	});

	it('bei einem falschen Argument bleibt der Parameter, auf den verwiesen wird, deklariert', () => {
		const code = `§text§.pu¦sh(1)`;
		expect(hoverAt(code)).to.match(/stream\$: Stream\(Any\)/);
	});

	it('ein Feld hinter einem womöglich leeren Zugriff zeigt den Typ des Zugriffs samt Empty', () => {
		const code = `Node = [value: Integer left: Or([] Node)]
f = (node: Node) =>
	node/left/val¦ue`;
		expect(hoverAt(code)).to.equal(typeMarkdown('Or(Empty Integer)'));
	});
});
