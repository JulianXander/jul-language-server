import { getFileCheckResult } from 'jul-compiler/out/checker/check-result.js';
import { ParsedDocuments } from 'jul-compiler/out/checker/checker.js';
import { ReferenceIndex, ReferenceLocation } from 'jul-compiler/out/checker/reference-index.js';
import { Positioned } from 'jul-compiler/out/compiler-errors.js';
import { dirname, relative } from 'jul-compiler/out/path-util.js';
import { Extension, isTestFilePath } from 'jul-compiler/out/util.js';
import {
	CodeAction,
	CompletionItem,
	CompletionItemKind,
	Diagnostic,
	DocumentSymbol,
	FoldingRange,
	MarkupContent,
	SemanticTokens,
	SignatureHelp,
	TextEdit,
} from 'vscode-languageserver-types';
import { getCompletionItems } from './completion.js';
import { DefinitionLocation, getDefinitionLocations } from './definition.js';
import { getDocumentSymbols } from './document-symbols.js';
import { getFoldingRanges } from './folding.js';
import { getDocumentFormattingEdits } from './formatting.js';
import { getHover } from './hover.js';
import { getIgnoreCommentCodeActions } from './ignore-comment.js';
import {
	getDocumentHighlightLocations,
	getReferenceLocations,
	getRenameEdits,
	getRenameRange,
	getTargetsAt,
	RenameEdit,
} from './references.js';
import { getSemanticTokens } from './semantic-tokens.js';
import { getSignatureHelp } from './signature-help.js';
import { findExpressionInParsedFile, isImportPath } from './symbol-lookup.js';
import { DiscoveredTest, findTests } from './test-discovery.js';

export type { DefinitionLocation } from './definition.js';
export type { RenameEdit } from './references.js';
export type { DiscoveredTest } from './test-discovery.js';
export { semanticTokenLegend } from './semantic-tokens.js';

/**
 * Einstieg für die Sprachfeatures im Browser (Playground der Homepage), ohne LSP-Verbindung und
 * ohne Dateisystem. Grundlage ist ein geprüfter Stand aller Dateien, wie ihn browser.ts des
 * Compilers liefert, mit einem ReferenceIndex aus demselben Lauf. Positionen sind 0-basiert,
 * Dateien stehen als Pfad, wie im Compiler.
 */

export interface Workspace {
	documents: ParsedDocuments;
	referenceIndex: ReferenceIndex;
}

export function hover(workspace: Workspace, filePath: string, rowIndex: number, columnIndex: number): MarkupContent | undefined {
	const parsedFile = workspace.documents[filePath];
	return parsedFile && getHover(parsedFile, rowIndex, columnIndex, dirname(filePath), workspace.documents);
}

/**
 * In einem Importpfad die anderen Dateien des Workspace, sonst wie im Language Server.
 */
export function completion(workspace: Workspace, filePath: string, rowIndex: number, columnIndex: number): CompletionItem[] {
	const parsedFile = workspace.documents[filePath];
	if (!parsedFile) {
		return [];
	}
	const { expression, scopes } = findExpressionInParsedFile(parsedFile, rowIndex, columnIndex);
	if (isImportPath(expression)) {
		const folderPath = dirname(filePath);
		return Object.keys(workspace.documents)
			.filter(otherPath => otherPath !== filePath && !isTestFilePath(otherPath))
			.map(otherPath => {
				const relativePath = relative(folderPath, otherPath);
				const importPath = relativePath.startsWith('.')
					? relativePath
					: `./${relativePath}`;
				return { label: importPath, kind: CompletionItemKind.File };
			});
	}
	return getCompletionItems(getFileCheckResult(parsedFile), expression, scopes, rowIndex, columnIndex, false) ?? [];
}

export function signatureHelp(workspace: Workspace, filePath: string, rowIndex: number, columnIndex: number): SignatureHelp | undefined {
	const parsedFile = workspace.documents[filePath];
	return parsedFile && getSignatureHelp(parsedFile, rowIndex, columnIndex);
}

export function definition(workspace: Workspace, filePath: string, rowIndex: number, columnIndex: number): DefinitionLocation[] {
	const parsedFile = workspace.documents[filePath];
	return parsedFile
		? getDefinitionLocations(parsedFile, rowIndex, columnIndex, workspace.documents, workspace.referenceIndex)
		: [];
}

export function references(
	workspace: Workspace,
	filePath: string,
	rowIndex: number,
	columnIndex: number,
	includeDeclaration: boolean,
): ReferenceLocation[] {
	const parsedFile = workspace.documents[filePath];
	const targets = parsedFile && getTargetsAt(parsedFile, rowIndex, columnIndex, workspace.documents, workspace.referenceIndex);
	return targets
		? getReferenceLocations(targets, workspace.referenceIndex, includeDeclaration)
		: [];
}

export function documentHighlights(workspace: Workspace, filePath: string, rowIndex: number, columnIndex: number): ReferenceLocation[] {
	const parsedFile = workspace.documents[filePath];
	return parsedFile
		? getDocumentHighlightLocations(parsedFile, rowIndex, columnIndex, workspace.documents, workspace.referenceIndex)
		: [];
}

export function prepareRename(workspace: Workspace, filePath: string, rowIndex: number, columnIndex: number): Positioned | undefined {
	const parsedFile = workspace.documents[filePath];
	const range = parsedFile && getRenameRange(parsedFile, rowIndex, columnIndex, workspace.documents);
	// Nur die Position, nicht der Knoten: Das Ergebnis geht per postMessage aus dem Worker.
	return range && {
		startRowIndex: range.startRowIndex,
		startColumnIndex: range.startColumnIndex,
		endRowIndex: range.endRowIndex,
		endColumnIndex: range.endColumnIndex,
	};
}

export function rename(
	workspace: Workspace,
	filePath: string,
	rowIndex: number,
	columnIndex: number,
	newName: string,
): RenameEdit[] {
	const parsedFile = workspace.documents[filePath];
	const targets = parsedFile && getTargetsAt(parsedFile, rowIndex, columnIndex, workspace.documents, workspace.referenceIndex);
	return targets
		? getRenameEdits(targets, newName, workspace.referenceIndex, workspace.documents)
		: [];
}

export function documentSymbols(workspace: Workspace, filePath: string): DocumentSymbol[] {
	const parsedFile = workspace.documents[filePath];
	return (parsedFile && getDocumentSymbols(parsedFile)) ?? [];
}

/**
 * Die Tests einer *.test.jul-Datei, in anderen Dateien keine.
 */
export function tests(workspace: Workspace, filePath: string): DiscoveredTest[] {
	const parsedFile = workspace.documents[filePath];
	return parsedFile
		? findTests(parsedFile)
		: [];
}

export function foldingRanges(workspace: Workspace, filePath: string, code: string): FoldingRange[] {
	return getFoldingRanges(workspace.documents[filePath], code.split('\n'));
}

export function semanticTokens(workspace: Workspace, filePath: string): SemanticTokens {
	return getSemanticTokens(workspace.documents[filePath]);
}

/**
 * Der Formatter kennt nur die Syntax von .jul, andere Dateien bleiben unberührt.
 */
export function formatting(code: string, filePath: string): TextEdit[] {
	return filePath.endsWith(Extension.jul)
		? getDocumentFormattingEdits(code, filePath)
		: [];
}

/**
 * Quick Fixes für die Meldungen an der Stelle. Die Änderungen stehen unter dem Dateipfad.
 */
export function codeActions(code: string, filePath: string, diagnostics: Diagnostic[]): CodeAction[] {
	return getIgnoreCommentCodeActions(filePath, code.split('\n'), diagnostics);
}
