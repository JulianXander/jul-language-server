import { dirname, relative, sep } from 'path';
import { TextEdit } from 'vscode-languageserver';
import { ParsedDocuments } from 'jul-compiler/out/checker/checker.js';
import { getPathFromImport, isCoreLibPath, isImportFunctionCall } from 'jul-compiler/out/parser/parser.js';
import { isExportedSymbol } from 'jul-compiler/out/parser/parser-utils.js';
import {
	ParseDestructuringDefinition,
	ParsedFile,
	ParseSingleDefinition,
} from 'jul-compiler/out/syntax-tree.js';
import { Extension } from 'jul-compiler/out/util.js';

export interface ImportCandidate {
	/** absoluter Pfad der Datei, die das Symbol definiert */
	filePath: string;
	/** relativer Pfad, wie er im Quelltext stehen wird, z.B. ./card-effects.jul */
	importPath: string;
}

interface ExistingImport {
	definition: ParseDestructuringDefinition | ParseSingleDefinition;
	/** relativer Pfad, wie er im Quelltext steht */
	path: string;
	fullPath: string;
}

function compareText(a: string, b: string): number {
	return a < b
		? -1
		: a > b
			? 1
			: 0;
}

function getImportPath(sourceFolder: string, filePath: string): string {
	const relativePath = relative(sourceFolder, filePath).split(sep).join('/');
	// relative liefert für den Elternordner bereits ../, nur der gleiche Ordner braucht ./
	return relativePath.startsWith('.')
		? relativePath
		: './' + relativePath;
}

/**
 * Alle Dateien des Projekts, die name als Top-Level-Definition enthalten.
 * Ein eigener Index lohnt nicht: der Name steht fest, pro Datei ist es ein Hash-Lookup,
 * und parsedDocuments ist nach dem Startup-Scan ohnehin vollständig.
 */
export function findImportCandidates(
	name: string,
	documentPath: string,
	parsedDocuments: ParsedDocuments,
): ImportCandidate[] {
	const sourceFolder = dirname(documentPath);
	const candidates: ImportCandidate[] = [];
	for (const filePath in parsedDocuments) {
		if (filePath === documentPath
			|| isCoreLibPath(filePath)) {
			continue;
		}
		const parsedFile = parsedDocuments[filePath]!;
		// json/yaml enthalten Daten, keine Deklarationen - ihre Schlüssel als Import anzubieten wäre Lärm
		if (parsedFile.extension === Extension.json
			|| parsedFile.extension === Extension.yaml) {
			continue;
		}
		const symbol = parsedFile.checked?.symbols[name];
		// Dateien, die den Namen bloß importieren, bieten ihn nicht an
		if (!symbol
			|| !isExportedSymbol(symbol)) {
			continue;
		}
		candidates.push({
			filePath: filePath,
			importPath: getImportPath(sourceFolder, filePath),
		});
	}
	return candidates.sort((a, b) => compareText(a.importPath, b.importPath));
}

function findTopLevelImports(parsedFile: ParsedFile): ExistingImport[] {
	const imports: ExistingImport[] = [];
	parsedFile.checked?.expressions?.forEach(expression => {
		if (expression.type !== 'definition'
			&& expression.type !== 'destructuring') {
			return;
		}
		const value = expression.value;
		if (!value
			|| !isImportFunctionCall(value)) {
			return;
		}
		const { path, fullPath } = getPathFromImport(value, parsedFile.sourceFolder);
		if (path === undefined
			|| fullPath === undefined) {
			return;
		}
		imports.push({
			definition: expression,
			path: path,
			fullPath: fullPath,
		});
	});
	return imports;
}

/**
 * Fügt name alphabetisch in ein bestehendes Destructuring ein - mehrzeilig als eigene Zeile,
 * einzeilig inline.
 */
function createFieldEdit(
	destructuring: ParseDestructuringDefinition,
	name: string,
): TextEdit | undefined {
	const fields = destructuring.fields;
	const singleFields = fields.fields;
	const firstField = singleFields[0];
	if (!firstField) {
		return undefined;
	}
	const successor = singleFields.find(field => compareText(field.name.name, name) > 0);
	const isMultiline = fields.startRowIndex !== fields.endRowIndex;
	if (successor) {
		const position = {
			line: successor.startRowIndex,
			character: isMultiline
				? 0
				: successor.startColumnIndex,
		};
		return {
			range: { start: position, end: position },
			newText: isMultiline
				// Einrückung ist immer Tab, die Spalte ist damit die Anzahl der Tabs
				? '\t'.repeat(firstField.startColumnIndex) + name + '\n'
				: name + ' ',
		};
	}
	const lastField = singleFields[singleFields.length - 1]!;
	const position = {
		line: lastField.endRowIndex,
		character: lastField.endColumnIndex,
	};
	return {
		range: { start: position, end: position },
		newText: (isMultiline
			? '\n' + '\t'.repeat(firstField.startColumnIndex)
			: ' ') + name,
	};
}

/**
 * Zeile, in der ein neuer Import entsteht: alphabetisch nach Pfad zwischen den bestehenden
 * Importen, aber nie unterhalb der Verwendung - sonst greift usedBeforeDefined.
 */
function findInsertRow(
	parsedFile: ParsedFile,
	existingImports: ExistingImport[],
	importPath: string,
	maxRowIndex: number,
): number {
	const importsAbove = existingImports.filter(existing =>
		existing.definition.startRowIndex <= maxRowIndex);
	const firstImport = importsAbove[0];
	if (firstImport) {
		const successor = importsAbove.find(existing => compareText(existing.path, importPath) > 0);
		if (successor) {
			return successor.definition.startRowIndex;
		}
		const rowAfterLast = importsAbove[importsAbove.length - 1]!.definition.endRowIndex + 1;
		return rowAfterLast <= maxRowIndex
			? rowAfterLast
			: firstImport.definition.startRowIndex;
	}
	const firstExpression = parsedFile.checked?.expressions?.[0];
	// vor den ersten Ausdruck, damit ein führender Kommentarblock oben bleibt
	return firstExpression
		? Math.min(firstExpression.startRowIndex, maxRowIndex)
		: 0;
}

/**
 * Der Edit, der name aus candidate verfügbar macht: entweder als neues Feld in einem bereits
 * bestehenden Destructuring derselben Datei oder als neue Definition in kanonischer Form.
 * maxRowIndex ist die Zeile der Verwendung - der Import muss darüber landen.
 */
export function createImportEdit(
	parsedFile: ParsedFile,
	candidate: ImportCandidate,
	name: string,
	maxRowIndex: number,
): TextEdit | undefined {
	const existingImports = findTopLevelImports(parsedFile);
	const existingDestructuring = existingImports.find(existing =>
		existing.fullPath === candidate.filePath
		&& existing.definition.type === 'destructuring'
		&& existing.definition.startRowIndex <= maxRowIndex);
	if (existingDestructuring) {
		return createFieldEdit(existingDestructuring.definition as ParseDestructuringDefinition, name);
	}
	const row = findInsertRow(parsedFile, existingImports, candidate.importPath, maxRowIndex);
	const position = { line: row, character: 0 };
	return {
		range: { start: position, end: position },
		newText: `(${name}) = import(§${candidate.importPath}§)\n`,
	};
}
