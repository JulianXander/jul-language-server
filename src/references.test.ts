import { expect } from 'chai';
import { join, resolve } from 'path';
import { ParsedDocuments } from 'jul-compiler/out/checker/checker.js';
import { ReferenceIndex, SymbolLocation } from 'jul-compiler/out/checker/reference-index.js';
import { createInMemoryHost, loadFile } from 'jul-compiler/out/project-loader.js';
import { PositionedExpression } from 'jul-compiler/out/syntax-tree.js';
import {
	createRenameWorkspaceEdit,
	getFieldAccessTypeFields,
	getReferenceLocations,
	getRenameEdits,
	RenameEdit,
	resolveRelatedTargets,
} from './references.js';

const folder = resolve('/references-test');
const filePath = join(folder, 'main.jul');

function check(code: string): { documents: ParsedDocuments; referenceIndex: ReferenceIndex; } {
	const documents: ParsedDocuments = {};
	const referenceIndex = new ReferenceIndex();
	const host = createInMemoryHost({ [filePath]: code }, { cloneUnchecked: true, referenceIndex: referenceIndex });
	const parsed = loadFile(filePath, documents, host);
	if (typeof parsed === 'string') {
		throw new Error(parsed);
	}
	return { documents, referenceIndex };
}

function getTypeField(documents: ParsedDocuments, typeName: string, fieldName: string): SymbolLocation {
	const type = documents[filePath]!.checked!.symbols[typeName]!.typeInfo!.type as any;
	const declaration = (type.julType === 'typeOf' ? type.value : type).declaration;
	return { symbol: declaration.expression.symbols[fieldName], filePath: filePath };
}

/** Der Wert der Top-Level-Definition name. */
function getDefinitionValue(documents: ParsedDocuments, name: string): PositionedExpression {
	const definition = documents[filePath]!.checked!.expressions!.find(expression =>
		expression.type === 'definition' && expression.name.name === name);
	if (definition?.type !== 'definition' || !definition.value) {
		throw new Error(`Definition ${name} not found`);
	}
	return definition.value;
}

/** Das Feld-Token eines Feldzugriffs wie a/name. */
function getFieldAccessKey(documents: ParsedDocuments, definitionName: string): PositionedExpression {
	const value = getDefinitionValue(documents, definitionName);
	if (value.type !== 'nestedReference' || !value.nestedKey) {
		throw new Error(`${definitionName} is not a field access`);
	}
	return value.nestedKey;
}

/** Zeile:Spalte (1-basiert) und neuer Text, sortiert - so lesen sich die Erwartungen wie der Code. */
function describeEdits(edits: RenameEdit[]): string[] {
	return edits
		.map(edit => `${edit.startRowIndex + 1}:${edit.startColumnIndex + 1} ${edit.newText}${edit.needsConfirmation ? ' (bestätigen)' : ''}`)
		.sort();
}

const fieldCode = [
	'MyType = [',
	'	name: Text',
	']',
	'a: MyType = [name = §a§]',
	'e = a/name',
	'(name) = a',
	'nameUsage = name',
	'',
].join('\n');

describe('references', () => {
	describe('resolveRelatedTargets', () => {
		it('ein Typfeld ist sein eigenes Ziel', () => {
			const { documents, referenceIndex } = check(fieldCode);
			const typeField = getTypeField(documents, 'MyType', 'name');
			expect(resolveRelatedTargets(typeField, referenceIndex)).to.deep.equal([typeField]);
		});

		it('ein Literalfeld an einer Stelle mit erwartetem Typ zielt auf das Typfeld', () => {
			const { documents, referenceIndex } = check(fieldCode);
			const literalField = getTypeField(documents, 'a', 'name');
			expect(resolveRelatedTargets(literalField, referenceIndex)).to.deep.equal([getTypeField(documents, 'MyType', 'name')]);
		});
	});

	describe('getFieldAccessTypeFields', () => {
		const code = [
			'MyType = [',
			'	name: Text',
			']',
			'Person = [name: Text age: Integer]',
			'Pet = [name: Text species: Text]',
			'a: MyType = [name = §a§]',
			'e = a/name',
			'(name) = a',
			'nameUsage = name',
			'x = [name = §x§]',
			'g = x/name',
			'y: Or(Person Pet) = [name = §Rex§ age = 3 species = §Hund§]',
			'f = y/name',
			'',
		].join('\n');

		it('ein Zugriff auf ein Literalfeld mit erwartetem Typ zielt auf das Typfeld', () => {
			const { documents, referenceIndex } = check(code);
			expect(getFieldAccessTypeFields(getFieldAccessKey(documents, 'e'), getTypeField(documents, 'a', 'name'), referenceIndex))
				.to.deep.equal([getTypeField(documents, 'MyType', 'name')]);
		});

		it('ein Zugriff auf eine mehrdeutige Stelle zielt auf alle Typfelder', () => {
			const { documents, referenceIndex } = check(code);
			expect(getFieldAccessTypeFields(getFieldAccessKey(documents, 'f'), getTypeField(documents, 'y', 'name'), referenceIndex))
				.to.deep.equal([getTypeField(documents, 'Person', 'name'), getTypeField(documents, 'Pet', 'name')]);
		});

		it('Gegenprobe: ein Zugriff auf ein Literalfeld ohne erwarteten Typ bleibt beim Literalfeld', () => {
			const { documents, referenceIndex } = check(code);
			expect(getFieldAccessTypeFields(getFieldAccessKey(documents, 'g'), getTypeField(documents, 'x', 'name'), referenceIndex))
				.to.equal(undefined);
		});

		it('Gegenprobe: eine Variable springt zu ihrem Binding, auch wenn sie mit einem Typfeld verknüpft ist', () => {
			const { documents, referenceIndex } = check(code);
			const nameUsage = getDefinitionValue(documents, 'nameUsage');
			const localSymbol = documents[filePath]!.checked!.symbols['name']!;
			expect(getFieldAccessTypeFields(nameUsage, { symbol: localSymbol, filePath: filePath }, referenceIndex))
				.to.equal(undefined);
		});
	});

	describe('getReferenceLocations', () => {
		it('liefert Literalfeld, Zugriff über die typisierte Variable, Destructuring und dessen Verwendung', () => {
			const { documents, referenceIndex } = check(fieldCode);
			const locations = getReferenceLocations([getTypeField(documents, 'MyType', 'name')], referenceIndex, true);
			expect(locations.map(location => `${location.startRowIndex + 1}:${location.startColumnIndex + 1}`).sort())
				.to.deep.equal(['2:2', '4:14', '5:7', '6:2', '7:13']);
		});
	});

	describe('getRenameEdits', () => {
		it('benennt Typfeld, Literalfeld, Zugriff, lokalen Namen und seine Verwendung um', () => {
			const { documents, referenceIndex } = check(fieldCode);
			const edits = getRenameEdits([getTypeField(documents, 'MyType', 'name')], 'label', referenceIndex, documents);
			expect(describeEdits(edits)).to.deep.equal([
				'2:2 label',
				'4:14 label',
				'5:7 label',
				'6:2 label',
				'7:13 label',
			]);
		});

		it('schreibt bei einem Namenskonflikt einen Alias statt den lokalen Namen umzubenennen', () => {
			const { documents, referenceIndex } = check(fieldCode + 'label = 5\n');
			const edits = getRenameEdits([getTypeField(documents, 'MyType', 'name')], 'label', referenceIndex, documents);
			expect(describeEdits(edits)).to.deep.equal([
				'2:2 label',
				'4:14 label',
				'5:7 label',
				'6:2 name = label',
			]);
		});

		it('ein Builtin als neuer Name ist ein Namenskonflikt', () => {
			const { documents, referenceIndex } = check(fieldCode);
			const edits = getRenameEdits([getTypeField(documents, 'MyType', 'name')], 'length', referenceIndex, documents);
			expect(describeEdits(edits)).to.include('6:2 name = length');
			expect(describeEdits(edits)).to.not.include('7:13 length');
		});

		it('von einer mehrdeutigen Stelle aus werden alle Typfelder umbenannt, die weiteren nur mit Bestätigung', () => {
			const { documents, referenceIndex } = check([
				'Person = [name: Text age: Integer]',
				'Pet = [name: Text species: Text]',
				'y: Or(Person Pet) = [name = §Rex§ age = 3 species = §Hund§]',
				'',
			].join('\n'));
			const targets = resolveRelatedTargets(getTypeField(documents, 'y', 'name'), referenceIndex);
			const edits = getRenameEdits(targets, 'label', referenceIndex, documents);
			expect(describeEdits(edits)).to.deep.equal([
				'1:11 label',
				'2:8 label (bestätigen)',
				'3:22 label',
			]);
		});
	});

	describe('createRenameWorkspaceEdit', () => {
		const edit = (row: number, needsConfirmation: boolean): RenameEdit => ({
			filePath: filePath,
			startRowIndex: row,
			startColumnIndex: 0,
			endRowIndex: row,
			endColumnIndex: 4,
			newText: 'label',
			needsConfirmation: needsConfirmation,
		});

		it('legt Änderungen, die bestätigt werden müssen, mit changeAnnotation vor', () => {
			const workspaceEdit = createRenameWorkspaceEdit([edit(0, false), edit(1, true)], true);
			expect(workspaceEdit.changes).to.equal(undefined);
			const documentEdits = (workspaceEdit.documentChanges![0] as any).edits;
			expect(documentEdits.map((textEdit: any) => textEdit.annotationId)).to.deep.equal([undefined, 'furtherTypeFields']);
			expect(workspaceEdit.changeAnnotations!['furtherTypeFields']!.needsConfirmation).to.equal(true);
		});

		it('ohne Unterstützung des Clients werden alle Änderungen ohne Rückfrage angewendet', () => {
			const workspaceEdit = createRenameWorkspaceEdit([edit(0, false), edit(1, true)], false);
			expect(workspaceEdit.documentChanges).to.equal(undefined);
			expect(Object.values(workspaceEdit.changes!)[0]).to.have.lengthOf(2);
		});

		it('ohne Änderung, die bestätigt werden muss, gibt es keine changeAnnotation', () => {
			const workspaceEdit = createRenameWorkspaceEdit([edit(0, false)], true);
			expect(workspaceEdit.changeAnnotations).to.equal(undefined);
			expect(Object.values(workspaceEdit.changes!)[0]).to.have.lengthOf(1);
		});
	});
});
