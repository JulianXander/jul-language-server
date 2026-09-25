import { builtInSymbols, ParsedDocuments } from 'jul-compiler/out/checker/checker.js';
import { ReferenceIndex, ReferenceLocation, SymbolLocation } from 'jul-compiler/out/checker/reference-index.js';
import { Positioned } from 'jul-compiler/out/compiler-errors.js';
import { forEachChild, PositionedExpression, SymbolTable } from 'jul-compiler/out/syntax-tree.js';

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
