import { expect } from 'chai';
import { join, resolve } from 'path';
import { ParsedDocuments } from 'jul-compiler/out/checker/checker.js';
import { createInMemoryHost, loadFile } from 'jul-compiler/out/project-loader.js';
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
		const code = [
			'a = 1',
			'b = ¦a',
			'',
		].join('\n');
		expect(hoverAt(code)).to.equal(typeMarkdown('1'));
	});

	it('ein per Destructuring aus einer Liste gebundener Name zeigt seinen Typ', () => {
		const code = [
			'list = [1 2]',
			'(fi¦rst) = list',
			'',
		].join('\n');
		expect(hoverAt(code)).to.equal(typeMarkdown('1'));
	});

	it('ein per Destructuring aus einem Dictionary gebundener Name zeigt seinen Typ', () => {
		const code = [
			'dict = [fieldName = 1]',
			'(field¦Name) = dict',
			'',
		].join('\n');
		expect(hoverAt(code)).to.equal(typeMarkdown('1'));
	});

	it('ein per Destructuring im Funktionsrumpf gebundener Name zeigt seinen Typ', () => {
		const code = [
			'f = (a: List(Integer)) =>',
			'	(¦x y) = a',
			'	[x y]',
			'',
		].join('\n');
		expect(hoverAt(code)).to.equal(typeMarkdown('Integer'));
	});
});
