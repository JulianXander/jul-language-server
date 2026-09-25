import { builtInSymbols, ParsedDocuments } from 'jul-compiler/out/checker/checker.js';
import { ReferenceIndex, ReferenceLocation, SymbolLocation } from 'jul-compiler/out/checker/reference-index.js';
import { Positioned } from 'jul-compiler/out/compiler-errors.js';
import { forEachChild, PositionedExpression, SymbolTable } from 'jul-compiler/out/syntax-tree.js';
import { AnnotatedTextEdit, TextEdit, WorkspaceEdit } from 'vscode-languageserver';
import { pathToUri, positionedToRange } from './util.js';

//#region Ziele

/**
 * Die Symbole, auf die Rename und Find-All-References eigentlich zielen: Ein Literalfeld an einer
 * Stelle mit erwartetem Typ und der lokale Name eines Destructurings ohne Alias gehören zu den
 * Feldern ihres Typs. Bei einer Union können das mehrere sein. Alles andere ist sein eigenes Ziel.
 */
export function resolveRelatedTargets(target: SymbolLocation, referenceIndex: ReferenceIndex): SymbolLocation[] {
	const typeFields = referenceIndex.getRelatedTypeFields(target.symbol, target.filePath);
	return typeFields.length
		? typeFields
		: [target];
}

/**
 * Go-to-Definition auf einem Feldzugriff: Ist das gefundene Feld ein Literalfeld mit erwartetem
 * Typ (a/name mit a: MyType = [...]), sind die Felder des Typs das Ziel, wie bei einem Zugriff auf
 * einen Parameter vom Typ MyType. undefined, wenn es dabei bleibt, etwa weil der Ausdruck kein
 * Feldzugriff ist: eine Variable springt zu ihrem eigenen Binding, auch wenn sie mit einem Typfeld
 * verknüpft ist.
 */
export function getFieldAccessTypeFields(
	expression: PositionedExpression,
	found: SymbolLocation,
	referenceIndex: ReferenceIndex,
): SymbolLocation[] | undefined {
	if (expression.type !== 'name'
		|| expression.parent?.type !== 'nestedReference') {
		return undefined;
	}
	const typeFields = referenceIndex.getRelatedTypeFields(found.symbol, found.filePath);
	return typeFields.length
		? typeFields
		: undefined;
}

//#endregion Ziele

//#region Find-All-References

export function getReferenceLocations(
	targets: SymbolLocation[],
	referenceIndex: ReferenceIndex,
	includeDeclaration: boolean,
): ReferenceLocation[] {
	const locations = new Map<string, ReferenceLocation>();
	const add = (location: ReferenceLocation) => {
		locations.set(getLocationKey(location.filePath, location), location);
	};
	targets.forEach(target => {
		referenceIndex.getReferences(target.symbol, target.filePath).forEach(add);
		if (includeDeclaration) {
			add(toLocation(target.filePath, target.symbol));
		}
	});
	return [...locations.values()];
}

//#endregion Find-All-References

//#region Rename

export interface RenameEdit extends ReferenceLocation {
	newText: string;
	/**
	 * Die Änderung gehört zu einem weiteren Typfeld einer mehrdeutigen Stelle. Der Nutzer soll sie
	 * in der Vorschau sehen, bevor sie angewendet wird.
	 */
	needsConfirmation: boolean;
}

/**
 * Die Änderungen für einen Rename der Ziele. Mit umbenannt werden die verknüpften Literalfelder,
 * die Zugriffe darauf und der lokale Name eines Destructurings ohne Alias samt seinen Verwendungen.
 * Kollidiert der neue Name dort mit einem vorhandenen, wird stattdessen ein Alias geschrieben
 * ((name) = a wird zu (name = label) = a), der lokale Name und seine Verwendungen bleiben.
 * Änderungen, die erst über ein weiteres Ziel hinzukommen, müssen bestätigt werden.
 */
export function getRenameEdits(
	targets: SymbolLocation[],
	newName: string,
	referenceIndex: ReferenceIndex,
	documents: ParsedDocuments,
): RenameEdit[] {
	const edits = new Map<string, RenameEdit>();
	const add = (location: ReferenceLocation, targetIndex: number) => {
		const key = getLocationKey(location.filePath, location);
		if (!edits.has(key)) {
			edits.set(key, { ...location, newText: newName, needsConfirmation: targetIndex > 0 });
		}
	};
	const conflictingLocals: SymbolLocation[] = [];
	targets.forEach((target, targetIndex) => {
		add(toLocation(target.filePath, target.symbol), targetIndex);
		referenceIndex.getDirectReferences(target.symbol, target.filePath).forEach(location => {
			add(location, targetIndex);
		});
		referenceIndex.getRelatedSymbols(target.symbol, target.filePath).forEach(related => {
			if (getDestructuredLocalName(related)
				&& hasNameConflict(newName, related.filePath, documents)) {
				conflictingLocals.push(related);
				return;
			}
			add(toLocation(related.filePath, related.symbol), targetIndex);
			referenceIndex.getDirectReferences(related.symbol, related.filePath).forEach(location => {
				add(location, targetIndex);
			});
		});
	});
	conflictingLocals.forEach(local => {
		const declarationKey = getLocationKey(local.filePath, local.symbol);
		referenceIndex.getDirectReferences(local.symbol, local.filePath).forEach(location => {
			const key = getLocationKey(location.filePath, location);
			if (key !== declarationKey) {
				edits.delete(key);
			}
		});
		edits.set(declarationKey, {
			...toLocation(local.filePath, local.symbol),
			newText: `${getDestructuredLocalName(local)} = ${newName}`,
			needsConfirmation: edits.get(declarationKey)?.needsConfirmation ?? false,
		});
	});
	return [...edits.values()];
}

const furtherTypeFieldsAnnotationId = 'furtherTypeFields';

/**
 * Baut aus den Rename-Änderungen die WorkspaceEdit. Änderungen, die bestätigt werden müssen,
 * tragen eine changeAnnotation, sofern der Client das kann (nur in documentChanges erlaubt), sonst
 * werden sie ohne Rückfrage angewendet.
 */
export function createRenameWorkspaceEdit(edits: RenameEdit[], supportsChangeAnnotations: boolean): WorkspaceEdit {
	const toTextEdit = (edit: RenameEdit): TextEdit => ({
		range: positionedToRange(edit),
		newText: edit.newText,
	});
	const editsByUri = new Map<string, (TextEdit | AnnotatedTextEdit)[]>();
	const addToUri = (edit: RenameEdit, textEdit: TextEdit | AnnotatedTextEdit) => {
		const uri = pathToUri(edit.filePath);
		let uriEdits = editsByUri.get(uri);
		if (!uriEdits) {
			uriEdits = [];
			editsByUri.set(uri, uriEdits);
		}
		uriEdits.push(textEdit);
	};
	const needsConfirmation = supportsChangeAnnotations
		&& edits.some(edit => edit.needsConfirmation);
	if (!needsConfirmation) {
		edits.forEach(edit => {
			addToUri(edit, toTextEdit(edit));
		});
		return { changes: Object.fromEntries(editsByUri) };
	}
	edits.forEach(edit => {
		addToUri(edit, edit.needsConfirmation
			? { ...toTextEdit(edit), annotationId: furtherTypeFieldsAnnotationId }
			: toTextEdit(edit));
	});
	return {
		documentChanges: [...editsByUri].map(([uri, uriEdits]) => ({
			textDocument: { uri: uri, version: null },
			edits: uriEdits,
		})),
		changeAnnotations: {
			[furtherTypeFieldsAnnotationId]: {
				label: 'Feld auch in weiteren Typen',
				description: 'Die Stelle gehört zu mehreren Typen, das Feld wird in allen umbenannt.',
				needsConfirmation: true,
			},
		},
	};
}

/**
 * Der lokale Name, wenn das Symbol aus einem Destructuring ohne Alias stammt.
 */
function getDestructuredLocalName(symbolLocation: SymbolLocation): string | undefined {
	const definition = symbolLocation.symbol.definition;
	return definition?.type === 'destructuringField' && !definition.source
		? definition.name.name
		: undefined;
}

/**
 * Ob name als lokaler Name Ärger machen würde. Bewusst grob: JUL verbietet doppelte Namen im
 * selben Scope und das Verdecken, jede Definition desselben Namens irgendwo in der Datei und jedes
 * Builtin zählt deshalb als Konflikt. Im Zweifel entsteht ein Alias zu viel, nie falscher Code.
 */
function hasNameConflict(name: string, filePath: string, documents: ParsedDocuments): boolean {
	if (builtInSymbols[name]) {
		return true;
	}
	const file = documents[filePath]?.checked;
	if (!file) {
		return true;
	}
	if (file.symbols[name]) {
		return true;
	}
	return !!file.expressions?.some(expression => isNameDefinedIn(expression, name));
}

function isNameDefinedIn(expression: PositionedExpression, name: string): boolean {
	// Die Symbole eines Dictionary-Literals oder -Typs sind Feldnamen, keine Variablen.
	if (expression.type !== 'dictionary'
		&& expression.type !== 'dictionaryType'
		&& 'symbols' in expression
		&& (expression.symbols as SymbolTable)[name]) {
		return true;
	}
	return !!forEachChild(expression, child =>
		isNameDefinedIn(child, name) || undefined);
}

//#endregion Rename

function getLocationKey(filePath: string, position: Positioned): string {
	return `${filePath}#${position.startRowIndex}:${position.startColumnIndex}`;
}

function toLocation(filePath: string, position: Positioned): ReferenceLocation {
	return {
		filePath: filePath,
		startRowIndex: position.startRowIndex,
		startColumnIndex: position.startColumnIndex,
		endRowIndex: position.endRowIndex,
		endColumnIndex: position.endColumnIndex,
	};
}
