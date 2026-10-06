import { readdirSync, readFileSync, statSync } from 'fs';
import { dirname, extname, join } from 'path';
import {
	LanguageService,
	getLanguageService as getHtmlLanguageService,
} from 'vscode-html-languageservice';
import {
	CodeAction,
	CodeActionKind,
	CodeActionParams,
	CompletionItem,
	CompletionItemKind,
	Diagnostic,
	DiagnosticSeverity,
	DiagnosticTag,
	DocumentHighlight,
	DocumentSymbol,
	InitializeParams,
	InitializeResult,
	Location,
	ParameterInformation,
	ProposedFeatures,
	Range,
	SignatureHelp,
	SymbolKind,
	TextDocuments,
	TextDocumentSyncKind,
	TextEdit,
} from 'vscode-languageserver';
import { createConnection } from 'vscode-languageserver/node';
import {
	TextDocument
} from 'vscode-languageserver-textdocument';
import { URI } from 'vscode-uri';
import {
	coreLibPath,
	getPathFromImport,
	isCoreLibPath,
	isImportFunctionCall,
} from 'jul-compiler/out/parser/parser.js';
import { readWarnUnknown } from 'jul-compiler/out/compiler/config.js';
import { loadFile, ProjectHost } from 'jul-compiler/out/compiler/project-loader.js';
import { getCheckedEscapableName } from 'jul-compiler/out/parser/parser-utils.js';
import { CompilerErrorSeverity, ErrorCode, errorInfos, Positioned } from 'jul-compiler/out/compiler-errors.js';
import {
	CompileTimeType,
	ImportedDependency,
	PositionedExpression,
	Parameter,
	ParseDestructuringFields,
	ParsedFile,
	ParseFunctionCall,
	ParseValueExpression,
	SymbolDefinition,
	SymbolTable,
	TextLiteralType,
} from 'jul-compiler/out/syntax-tree.js';
import { builtInSymbols, checkTypes, findSymbolInScopesWithBuiltIns, ParsedDocuments } from 'jul-compiler/out/checker/checker.js';
import { isDictionaryLiteralType, isFunctionType, isParameterReference, isParametersType, isTextLiteralType, typeToString } from 'jul-compiler/out/checker/type-algebra.js';
import { ReferenceIndex, resolveCanonicalSymbol, resolveImportBinding } from 'jul-compiler/out/checker/reference-index.js';
import { isDefined, isTestFilePath, isValidExtension, map } from 'jul-compiler/out/util.js';
import { createImportEdit, findImportCandidates } from './auto-import.js';
import { createChangeDebouncer } from './change-debouncer.js';
import { emptyLiteralsNotification, EmptyLiteralsParams, findEmptyLiterals } from './empty-literals.js';
import { getIgnoreCommentCodeActions } from './ignore-comment.js';
import {
	createRenameWorkspaceEdit,
	getFieldAccessTypeFields,
	getReferenceLocations,
	getRenameEdits,
	resolveRelatedTargets,
} from './references.js';
import {
	dictionaryTypeToCompletionItems,
	getFieldReferenceCompletionItems,
	getArgumentPositionKind,
	getCompletionSortText,
	getDictionaryLiteralFieldCompletionItems,
	getExpectedPositionKind,
	getFieldNamePositionKind,
	getFirstArgumentSymbolFilter,
	getInfixFunctionCall,
	getLambdaCompletionItem,
	isTypeSymbol,
} from './completion.js';
import { getHover, getTypeMarkdown } from './hover.js';
import { getSemanticTokens, semanticTokenLegend } from './semantic-tokens.js';
import { DiscoveredTest, findTests } from './test-discovery.js';
import {
	findExpressionInParsedFile,
	getRawSymbolDefinition,
	getSymbolDefinition,
} from './symbol-lookup.js';
import {
	getDeclaredResolvedType,
	getParameterIndex,
	getResolvedType,
	pathToUri,
	positionedToRange,
} from './util.js';

// For performance do not process large files
const maxFileSize = 100000;

// Create a connection for the server, using Node's IPC as a transport.
// Also include all preview / proposed LSP features.
const connection = createConnection(ProposedFeatures.all);

// Create a simple text document manager.
const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);
const parsedDocuments: ParsedDocuments = {};
// Projektweiter Referenz-Index für Rename/Find-All-References, siehe
// jul-compiler/docs/cross-file-reference-index.md. Lebt so lange wie der Serverprozess, wird pro
// Datei bei jedem Recheck geleert und neu befüllt (siehe checkTypes-Aufrufe unten).
const referenceIndex = new ReferenceIndex();
// Reverse-Dependency-Map (wer importiert diese Datei, auch transitiv) für die Invalidierung - Typen
// fließen über Definitionen weiter (b = d), ParsedFile.dependencies kennt nur die Vorwärtsrichtung.
const dependents = new Map<string, Set<string>>();

function registerDependencies(filePath: string, dependencies: ImportedDependency[] | undefined): void {
	dependencies?.forEach(({ fullPath }) => {
		let dependentSet = dependents.get(fullPath);
		if (!dependentSet) {
			dependentSet = new Set();
			dependents.set(fullPath, dependentSet);
		}
		dependentSet.add(filePath);
	});
}

function unregisterDependencies(filePath: string, dependencies: ImportedDependency[] | undefined): void {
	dependencies?.forEach(({ fullPath }) => {
		dependents.get(fullPath)?.delete(filePath);
	});
}

/**
 * Alle Dateien, die filePath direkt oder transitiv importieren - müssen mit invalidiert
 * werden, wenn sich filePath ändert (Visited-Set schützt vor zyklischen Importen).
 */
function getTransitiveDependents(filePath: string): Set<string> {
	const result = new Set<string>();
	const queue = [filePath];
	while (queue.length) {
		const current = queue.shift()!;
		dependents.get(current)?.forEach(dependentPath => {
			if (!result.has(dependentPath)) {
				result.add(dependentPath);
				queue.push(dependentPath);
			}
		});
	}
	return result;
}

let hasDiagnosticRelatedInformationCapability = false;
/**
 * Der Client kann Änderungen eines Renames zur Bestätigung vorlegen (changeAnnotations mit
 * needsConfirmation, nur in documentChanges erlaubt).
 */
let hasChangeAnnotationCapability = false;
let htmlLanguageService: LanguageService;

/**
 * Wurzelordner des Workspace, aus params.workspaceFolders (bzw. dem älteren rootUri als Fallback).
 * Wird in onInitialized für den initialen Workspace-weiten Scan gebraucht - Rename/Find-All-
 * References über den ReferenceIndex kennen nur Dateien, die je geparst wurden, und ohne diesen
 * Scan wären das ausschließlich geöffnete Dateien plus deren Importe.
 */
let workspaceFolderPaths: string[] = [];

connection.onInitialize((params: InitializeParams) => {
	htmlLanguageService = getHtmlLanguageService();

	const capabilities = params.capabilities;

	hasDiagnosticRelatedInformationCapability = !!capabilities.textDocument?.publishDiagnostics?.relatedInformation;
	hasChangeAnnotationCapability = !!capabilities.workspace?.workspaceEdit?.changeAnnotationSupport
		&& !!capabilities.workspace.workspaceEdit.documentChanges;

	workspaceFolderPaths = params.workspaceFolders
		? params.workspaceFolders.map(folder => uriToPath(folder.uri))
		: params.rootUri
			? [uriToPath(params.rootUri)]
			: [];

	const result: InitializeResult = {
		capabilities: {
			codeActionProvider: {
				codeActionKinds: [CodeActionKind.QuickFix],
			},
			// Tell the client that this server supports code completion.
			completionProvider: {
				resolveProvider: true,
				triggerCharacters: ['.', '/'],
			},
			definitionProvider: true,
			documentHighlightProvider: true,
			documentSymbolProvider: true,
			hoverProvider: true,
			referencesProvider: true,
			renameProvider: {
				prepareProvider: true,
			},
			semanticTokensProvider: {
				legend: semanticTokenLegend,
				full: true,
			},
			signatureHelpProvider: {
				triggerCharacters: ['(', ' ', '\n'],
			},
			textDocumentSync: TextDocumentSyncKind.Incremental,
		},
	};
	return result;
});

/**
 * Alle Dateien mit importierbarer Extension unterhalb folder, rekursiv - node_modules/out
 * ausgenommen, analog zu findJulFiles in den Test-/Bench-Skripten.
 */
function findIndexableFiles(folder: string): string[] {
	let entries;
	try {
		entries = readdirSync(folder, { withFileTypes: true });
	} catch {
		return [];
	}
	return entries.flatMap(entry => {
		if (entry.isDirectory()) {
			return entry.name === 'node_modules' || entry.name === 'out'
				? []
				: findIndexableFiles(join(folder, entry.name));
		}
		return isValidExtension(extname(entry.name))
			? [join(folder, entry.name)]
			: [];
	});
}

/**
 * Parst+checkt den gesamten Workspace einmal beim Start. Ohne das kennt der ReferenceIndex nur
 * geöffnete Dateien plus deren Importe - Rename/Find-All-References würden Importeure übersehen,
 * die nie geöffnet wurden und auch nicht transitiv von einer geöffneten Datei importiert werden.
 */
connection.onInitialized(() => {
	workspaceFolderPaths.forEach(folderPath => {
		findIndexableFiles(folderPath).forEach(filePath => {
			parseDocumentByPath(filePath);
		});
	});
});

//#region diagnostics

/**
 * uri wird bewusst vom Aufrufer übergeben statt aus dem Dateipfad neu gebaut: URI.file(path)
 * normalisiert unter Windows die Laufwerksbuchstaben-Schreibweise anders als die vom Client
 * gesendete URI (z.B. "C:" -> "c:") - ein Roundtrip über den Pfad würde die URI-Strings
 * auseinanderlaufen lassen, unter denen der Client seine offenen Dokumente führt.
 */
function sendDiagnosticsForFile(uri: string, parsed: ParsedFile, version?: number): void {
	const { errors } = parsed.checked!;
	const diagnostics: Diagnostic[] = errors.map(error => {
		const diagnostic: Diagnostic = {
			severity: diagnosticSeverities[errorInfos[error.code].severity],
			range: positionedToRange(error),
			code: error.code,
			message: error.message,
			source: 'jul'
		};
		// Der Editor graut den Namen aus, statt ihn zu unterstreichen.
		if (error.code === ErrorCode.unusedDefinition) {
			diagnostic.tags = [DiagnosticTag.Unnecessary];
		}
		if (error.expectedIndent !== undefined) {
			diagnostic.data = { expectedIndent: error.expectedIndent };
		}
		if (hasDiagnosticRelatedInformationCapability && error.relatedInformation) {
			diagnostic.relatedInformation = [
				{
					location: {
						uri,
						range: positionedToRange(error.relatedInformation),
					},
					message: error.relatedInformation.message,
				},
			];
		}
		return diagnostic;
	});
	connection.sendDiagnostics({ uri, diagnostics, version });
}

/**
 * Rechecked alle (transitiven) Dateien, die filePath importieren, und sendet für jede aktualisierte
 * Diagnostics - nötig, weil sich ihre inferierten Importtypen bzw. der Referenz-Index geändert
 * haben können.
 */
function recheckDependents(filePath: string): void {
	getTransitiveDependents(filePath).forEach(dependentPath => {
		const dependentParsed = parsedDocuments[dependentPath];
		if (!dependentParsed) {
			return;
		}
		checkTypes(dependentParsed, parsedDocuments, projectHost);
		sendDiagnosticsForFile(pathToUri(dependentPath), dependentParsed);
	});
}

// This event is emitted when the text document first opened or when its content has changed.
// parse document, fill parsedDocuments and sendDiagnostics
function processDocumentChange(textDocument: TextDocument): void {
	const text = textDocument.getText();
	if (text.length > maxFileSize) {
		return;
	}
	const path = uriToPath(textDocument.uri);
	const parsed = parseDocumentByCode(text, path);
	sendDiagnosticsForFile(textDocument.uri, parsed, textDocument.version);
	const emptyLiterals: EmptyLiteralsParams = {
		uri: textDocument.uri,
		version: textDocument.version,
		ranges: findEmptyLiterals(parsed),
	};
	connection.sendNotification(emptyLiteralsNotification, emptyLiterals);
	// Andere Dateien importieren evtl. diese - ihre Typen/Referenzen müssen neu berechnet werden.
	recheckDependents(path);
}

/**
 * Änderungen derselben Datei werden zusammengefasst, siehe change-debouncer.ts. Anfragen, die den
 * Syntaxbaum lesen, rufen vorher flushPendingChanges auf und sehen so nie einen veralteten Stand.
 */
const changeDebouncer = createChangeDebouncer<TextDocument>(
	(_uri, textDocument) => processDocumentChange(textDocument),
	100,
	500,
);

function flushPendingChanges(): void {
	changeDebouncer.flushAll();
}

documents.onDidChangeContent(change => {
	changeDebouncer.schedule(change.document.uri, change.document);
});

documents.onDidClose(close => {
	changeDebouncer.forget(close.document.uri);
});

/**
 * Derselbe Ladeweg wie in der CLI (project-loader.ts). Obendrauf pflegt der Server nur seinen
 * Abhängigkeitsgraphen in umgekehrter Richtung.
 */
const projectHost: ProjectHost = {
	readSource: path => {
		const code = tryReadTextFile(path);
		if (code === undefined) {
			return { type: 'notFound' };
		}
		if (code.length > maxFileSize) {
			return { type: 'skipped' };
		}
		return { type: 'code', code: code };
	},
	// recheckDependents checkt Dateien ohne neues Parsen erneut.
	cloneUnchecked: true,
	// Die jul-config.yaml der Datei entscheidet, der Server hält mehrere Projekte.
	warnUnknown: readWarnUnknown,
	referenceIndex: referenceIndex,
	onParsed: (parsed, previous) => {
		unregisterDependencies(parsed.filePath, previous?.dependencies);
		registerDependencies(parsed.filePath, parsed.dependencies);
		if (isTestFilePath(parsed.filePath)) {
			notifyTestsChanged(parsed.filePath, findTests(parsed));
		}
	},
};

/**
 * Parst text (Editor-Inhalt) neu, lädt fehlende Importe von der Platte und checkt.
 */
function parseDocumentByCode(text: string, path: string): ParsedFile {
	return loadFile(path, parsedDocuments, projectHost, text);
}

/**
 * Lädt path von der Platte, sofern noch nicht geladen, und checkt.
 */
function parseDocumentByPath(path: string): void {
	loadFile(path, parsedDocuments, projectHost);
}
//#endregion diagnostics

connection.onDidChangeWatchedFiles(changeParams => {
	flushPendingChanges();
	// Monitored files have change in VSCode
	const changedFilePaths = changeParams.changes
		.map(fileChange => uriToPath(fileChange.uri))
		.filter(path => {
			return !!parsedDocuments[path];
		});
	// Eine neue Testdatei importiert niemand, ohne das Laden hier fehlte sie im Test Explorer, bis
	// sie jemand öffnet.
	changeParams.changes
		.map(fileChange => uriToPath(fileChange.uri))
		.filter(path => isTestFilePath(path) && !parsedDocuments[path])
		.forEach(parseDocumentByPath);

	// Transitiv betroffene Dateien (über dependents) VOR dem
	// Neu-Parsen ermitteln - danach kennen wir die alten Import-Kanten nicht mehr.
	const transitivelyAffected = new Set<string>();
	changedFilePaths.forEach(path => {
		getTransitiveDependents(path).forEach(dependentPath => transitivelyAffected.add(dependentPath));
	});
	changedFilePaths.forEach(path => transitivelyAffected.delete(path));

	// clean invalidated data
	changedFilePaths.forEach((path) => {
		unregisterDependencies(path, parsedDocuments[path]?.dependencies);
		delete parsedDocuments[path];
	});
	// recalculate data
	changedFilePaths.forEach((path) => {
		parseDocumentByPath(path);
		// Gelöscht: loadFile findet sie nicht mehr, onParsed kommt nicht.
		if (isTestFilePath(path) && !parsedDocuments[path]) {
			notifyTestsChanged(path, []);
		}
	});
	transitivelyAffected.forEach(dependentPath => {
		const dependentParsed = parsedDocuments[dependentPath];
		if (!dependentParsed) {
			return;
		}
		checkTypes(dependentParsed, parsedDocuments, projectHost);
		sendDiagnosticsForFile(pathToUri(dependentPath), dependentParsed);
	});
});

//#region autocomplete
// This handler provides the initial list of the completion items.
connection.onCompletion(completionParams => {
	flushPendingChanges();
	const documentUri = completionParams.textDocument.uri;
	const documentPath = uriToPath(documentUri);
	const parsedFile = parsedDocuments[documentPath];
	if (!parsedFile) {
		return;
	}
	const folderPath = dirname(documentPath);
	const position = completionParams.position;
	const rowIndex = position.line;
	const columnIndex = position.character;
	// TODO sortierung type/nicht-type, bei normaler stelle erst nicht-types, bei type erst types/nur types?
	// todo / (nested field)
	// const foundSymbol = getSymbolDefinition(parsed, rowIndex, columnIndex);
	// if (!foundSymbol) {
	// 	return;
	// }

	// Get symbols from containing scopes
	const { expression, scopes } = findExpressionInParsedFile(parsedFile, rowIndex, columnIndex);
	const positionKind = getExpectedPositionKind(expression)
		?? getArgumentPositionKind(expression, rowIndex, columnIndex);

	//#region embbeded language
	const embeddedLanguage = expression?.type === 'text' && expression.language;
	switch (embeddedLanguage) {
		case 'html':
			const textDocument = documents.get(documentUri);
			if (!textDocument) {
				break;
			}
			// TODO
			// Get virtual html document, with all non-html code replaced with whitespace
			// const embedded = documentRegions.get(document).getEmbeddedDocument('html');
			const embedded = textDocument;
			// Compute a response with vscode-html-languageservice
			const parsedHtml = htmlLanguageService.parseHTMLDocument(textDocument);
			return htmlLanguageService.doComplete(embedded, position, parsedHtml);
		case 'js':
			// TODO
			break;
		default:
			break;
	}
	//#endregion embbeded language

	if (expression?.type === 'text') {
		//#region import path
		if (isImportPath(expression)) {
			const rawImportedPath = expression.values[0]?.type === 'textToken'
				? expression.values[0].value
				: undefined;
			let entryFolderPath = folderPath;
			let entries;
			if (rawImportedPath) {
				const importedFullPath = join(folderPath, rawImportedPath);
				// ein vollständig getippter Dateipfad ist der Normalfall, kein Fehler
				if (statSync(importedFullPath, { throwIfNoEntry: false })?.isDirectory()) {
					entryFolderPath = importedFullPath;
					entries = readdirSync(entryFolderPath, { withFileTypes: true });
				}
			}
			if (!entries) {
				entryFolderPath = folderPath;
				entries = readdirSync(entryFolderPath, { withFileTypes: true });
			}
			return entries.map(entry => {
				const entryName = entry.name;
				const isDirectory = entry.isDirectory();
				// selbst import nicht vorschlagen
				if (!isDirectory) {
					const entryFilePath = join(entryFolderPath, entryName);
					if (entryFilePath === documentPath) {
						return undefined;
					}
				}
				// nur Dateien mit importierbarer extension vorschlagen
				// TODO nur Ordner, die importierbare Dateien enthalten vorschlagen
				const extension = extname(entryName);
				if (!isDirectory) {
					if (!isValidExtension(extension)) {
						return undefined;
					}
				}
				const completionItem: CompletionItem = {
					label: entryName,
					insertText: (rawImportedPath
						? ''
						: './') + entryName,
					kind: isDirectory
						? CompletionItemKind.Folder
						: CompletionItemKind.File,
					detail: undefined,
					documentation: undefined,
				};
				return completionItem;
			}).filter(isDefined);
		}
		//#endregion import path

		//#region Text literal with declared type
		const declaredType = getDeclaredResolvedType(expression);
		switch (declaredType?.julType) {
			case 'textLiteral':
				return [textLiteralTypeToCompletionItem(declaredType)];
			case 'or':
				return declaredType.ChoiceTypes
					.filter(isTextLiteralType)
					.map(textLiteralTypeToCompletionItem);
			default:
				break;
		}
		//#endregion Text literal with declared type
	}

	// In der core-lib sind die builtInSymbols bereits der unterste Scope,
	// sonst stünde jeder builtIn Name doppelt in der completion.
	const allScopes = isCoreLibPath(documentPath)
		? scopes
		: [...scopes, builtInSymbols];
	let symbolFilter: ((symbol: SymbolDefinition, name: string) => boolean) | undefined = undefined;
	const lambdaCompletionItems = [getLambdaCompletionItem(expression, rowIndex, columnIndex, allScopes)].filter(isDefined);
	//#region infix function call (bei infix function reference)
	const infixFunctionCall = getInfixFunctionCall(expression);
	if (infixFunctionCall) {
		const prefixArgumentTypeRaw = infixFunctionCall.prefixArgument!.typeInfo!.type;
		let prefixArgumentType: CompileTimeType | undefined;
		if (isParameterReference(prefixArgumentTypeRaw)) {
			const dereferenced = findSymbolInScopesWithBuiltIns(prefixArgumentTypeRaw.name, scopes);
			prefixArgumentType = dereferenced?.symbol.typeInfo?.type;
		}
		else {
			prefixArgumentType = prefixArgumentTypeRaw;
		}

		symbolFilter = getFirstArgumentSymbolFilter(prefixArgumentType);

		// hier wird immer eine Funktion ausgewählt (nie ein Typ) - unabhängig davon, ob
		// `expression` die Referenz selbst oder der ganze Aufruf ist (siehe getInfixFunctionCall)
		return symbolsToCompletionItems(allScopes, symbolFilter, 'value');
	}
	//#endregion infix function call (bei infix function reference)

	//#region / field reference
	if (expression?.type === 'nestedReference') {
		const dereferencedType = getResolvedType(expression.source?.typeInfo);
		return dereferencedType && getFieldReferenceCompletionItems(dereferencedType);
	}
	//#endregion / field reference

	//#region dictionary literal field
	const dictionaryLiteralFieldCompletionItems = getDictionaryLiteralFieldCompletionItems(expression);
	if (dictionaryLiteralFieldCompletionItems) {
		switch (getFieldNamePositionKind(expression)) {
			case 'exclusive':
				return dictionaryLiteralFieldCompletionItems;
			case 'mixed':
				// positionale Argumente sind der Normalfall, die Feldnamen kommen dahinter
				return [
					...lambdaCompletionItems,
					...symbolsToCompletionItems(allScopes, undefined, positionKind),
					...dictionaryLiteralFieldCompletionItems.map(completionItem => ({
						...completionItem,
						sortText: '2' + (completionItem.sortText ?? completionItem.label),
					})),
				];
			case 'none':
				break;
		}
	}
	//#endregion dictionary literal field

	//#region destructuring definition field
	let destructuringFields: ParseDestructuringFields | undefined;
	if (expression?.type === 'destructuringFields') {
		destructuringFields = expression;
	}
	if (expression?.parent?.type === 'destructuringFields') {
		destructuringFields = expression.parent;
	}
	if (expression?.parent?.parent?.type === 'destructuringFields'
		&& expression.parent.type === 'destructuringField'
		&& expression === expression.parent.name
	) {
		destructuringFields = expression.parent.parent;
	}
	if (destructuringFields) {
		const destructuring = destructuringFields.parent;
		if (destructuring?.type === 'destructuring') {
			const destructuredValue = destructuring.value;
			// schon definierte Felder ausschließen
			symbolFilter = (symbol, name) => {
				return !destructuring.fields.symbols[name];
			};
			switch (destructuredValue?.type) {
				case 'dictionary':
				case 'dictionaryType':
					return symbolsToCompletionItems([destructuredValue.symbols], symbolFilter, positionKind);
				default: {
					const dereferencedType = getResolvedType(destructuredValue?.typeInfo);
					if (isDictionaryLiteralType(dereferencedType)) {
						const allCompletionItems = dictionaryTypeToCompletionItems(dereferencedType.Fields);
						// schon definierte Felder ausschließen
						const filtered = allCompletionItems.filter(completionItem => {
							return !destructuring.fields.symbols[completionItem.label];
						});
						return filtered;
					}
					return [];
				}
			}
		}
	}
	//#endregion destructuring definition field

	//#region function literal parameter name
	if (expression?.type === 'parameters') {
		const innerParamsType = getDeclaredResolvedType(expression);
		if (isParametersType(innerParamsType)) {
			const completionItems: CompletionItem[] = [];
			innerParamsType.singleNames.forEach((singleName, index) => {
				const isAlreadyDeclared = expression.singleFields.some(declaredParameter =>
					declaredParameter.source?.name === singleName.name
					|| (!declaredParameter.source && declaredParameter.name.name === singleName.name));
				if (!isAlreadyDeclared) {
					completionItems.push(parameterToCompletionItem(singleName, index, false));
				}
			});
			if (innerParamsType.rest && !expression.rest) {
				completionItems.push(parameterToCompletionItem(innerParamsType.rest, innerParamsType.singleNames.length, true));
			}
			return completionItems;
		}
	}
	//#endregion function literal parameter name

	return [
		...lambdaCompletionItems,
		...symbolsToCompletionItems(allScopes, undefined, positionKind),
	];
});

//#region create CompletionItems

function parameterToCompletionItem(parameter: Parameter, index: number, isRest: boolean): CompletionItem {
	const completionItem: CompletionItem = {
		label: (isRest ? '...' : '') + parameter.name,
		kind: CompletionItemKind.Constant,
		detail: parameter.type
			? typeToString(parameter.type, 0, 0)
			: undefined,
		// documentation: parameter.description,
		sortText: '' + index,
	};
	return completionItem;
}

function textLiteralTypeToCompletionItem(value: TextLiteralType): CompletionItem {
	const completionItem: CompletionItem = {
		label: value.value,
		kind: CompletionItemKind.Constant,
		// kind: CompletionItemKind.Text,
		// detail: typeToString(value, 0),
		// documentation: symbol.description,
	};
	return completionItem;
}

function symbolsToCompletionItems(
	scopes: SymbolTable[],
	symbolFilter?: (symbol: SymbolDefinition, name: string) => boolean,
	positionKind?: 'type' | 'value',
): CompletionItem[] {
	return scopes.flatMap(symbols => {
		return map(
			symbols,
			(symbol, name) => {
				const showSymbol = symbolFilter?.(symbol, name) ?? true;
				if (!showSymbol) {
					return undefined;
				}
				const symbolType = getResolvedType(symbol.typeInfo);
				const isFunction = isFunctionType(symbolType);
				const sortText = getCompletionSortText(name, isTypeSymbol(symbolType), positionKind);
				const completionItem: CompletionItem = {
					label: name,
					kind: isFunction
						? CompletionItemKind.Function
						: CompletionItemKind.Constant,
					detail: symbolType && typeToString(symbolType, 0, 0),
					documentation: symbol.description,
					sortText: sortText,
				};
				return completionItem;
			}).filter(isDefined);
	});
}

//#endregion create CompletionItems

// This handler resolves additional information for the item selected in
// the completion list.
connection.onCompletionResolve((item: CompletionItem): CompletionItem => {
	if (item.data === 1) {
		item.detail = 'TypeScript details';
		item.documentation = 'TypeScript documentation';
	} else if (item.data === 2) {
		item.detail = 'JavaScript details';
		item.documentation = 'JavaScript documentation';
	}
	return item;
});
//#endregion autocomplete

//#region function signature help
connection.onSignatureHelp(signatureParams => {
	const parsed = getParsedFileByUri(signatureParams.textDocument.uri);
	if (!parsed) {
		return;
	}
	// TODO find functiontLiteral, show param + return type
	const rowIndex = signatureParams.position.line;
	const columnIndex = signatureParams.position.character;
	const { expression, scopes } = findExpressionInParsedFile(parsed, rowIndex, columnIndex);
	if (expression?.parent?.type === 'functionCall') {
		const functionCall = expression.parent;
		const functionSymbol = getFunctionSymbolFromFunctionCall(functionCall, scopes);
		if (functionSymbol) {
			const functionType = functionSymbol.symbol.typeExpression;
			const parameterResults: ParameterInformation[] = [];
			if (functionType?.type === 'functionLiteral') {
				const paramsType = functionType.params;
				if (paramsType.type === 'parameters') {
					paramsType.singleFields.forEach(singleField => {
						parameterResults.push({
							label: singleField.name.name,
							documentation: getTypeMarkdown(singleField.typeInfo, singleField.description),
						});
					});
					const rest = paramsType.rest;
					if (rest) {
						parameterResults.push({
							label: rest.name.name,
							documentation: getTypeMarkdown(rest.typeInfo, rest.description),
						});
					}
				}
			}
			const parameterIndex = getParameterIndex(functionCall, rowIndex, columnIndex, parameterResults.length);
			const normalizedFunctionType = getResolvedType(functionSymbol.symbol.typeInfo);
			const signatureResult: SignatureHelp = {
				signatures: [{
					label: normalizedFunctionType
						? typeToString(normalizedFunctionType, 0, 0)
						: functionSymbol.name,
					documentation: functionSymbol.symbol.description,
					parameters: parameterResults,
				}],
				activeParameter: parameterIndex,
				activeSignature: 0,
			};
			return signatureResult;
		}
	}
	return undefined;
});

function getFunctionSymbolFromFunctionCall(functionCall: ParseFunctionCall, scopes: SymbolTable[]): {
	name: string;
	isBuiltIn: boolean;
	symbol: SymbolDefinition;
} | undefined {
	const functionExpression = functionCall.functionExpression;
	if (functionExpression?.type === 'reference') {
		const functionName = functionExpression.name.name;
		const functionSymbol = findSymbolInScopesWithBuiltIns(functionName, scopes);
		return functionSymbol && {
			...functionSymbol,
			name: functionName,
		};
	}
}
//#endregion function signature help

connection.languages.semanticTokens.on(params =>
	// Dateien über maxFileSize stehen gar nicht erst in parsedDocuments.
	getSemanticTokens(getParsedFileByUri(params.textDocument.uri)));


//#region tests
/**
 * Zuletzt an den Client gemeldete Tests je Testdatei, als JSON. Beim Tippen wird jede Änderung neu
 * geparst, gemeldet wird nur, was sich an den Tests geändert hat.
 */
const sentTests = new Map<string, string>();

function notifyTestsChanged(filePath: string, tests: DiscoveredTest[]): void {
	const testsJson = JSON.stringify(tests);
	if (sentTests.get(filePath) === testsJson) {
		return;
	}
	sentTests.set(filePath, testsJson);
	connection.sendNotification('jul/testsChanged', { uri: pathToUri(filePath), tests: tests });
}

/**
 * Alle Tests des Workspace für den Test Explorer (siehe test-explorer.ts der Extension). Danach
 * kommen Änderungen über jul/testsChanged.
 */
connection.onRequest('jul/tests', () =>
	Object.values(parsedDocuments)
		.filter(parsed => isTestFilePath(parsed.filePath))
		.map(parsed => {
			const tests = findTests(parsed);
			sentTests.set(parsed.filePath, JSON.stringify(tests));
			return { uri: pathToUri(parsed.filePath), tests: tests };
		}));
//#endregion tests

//#region go to definition
// Go to definition auf builtIns führt in die core-lib. Statt die kompilierte Kopie in out/
// als editierbare Datei zu öffnen, liefert der Server ihren Inhalt an ein read only
// virtual document des Clients. Siehe extension.ts, coreLibScheme.
const coreLibUri = 'jul-core-lib:/core-lib.jul';
connection.onRequest('jul/coreLibContent', () =>
	tryReadTextFile(coreLibPath) ?? '');
connection.onDefinition((definitionParams) => {
	flushPendingChanges();
	const documentUri = definitionParams.textDocument.uri;
	const documentPath = uriToPath(documentUri);
	const parsedFile = parsedDocuments[documentPath];
	if (!parsedFile) {
		return;
	}
	const folderPath = dirname(documentPath);
	const rowIndex = definitionParams.position.line;
	const columnIndex = definitionParams.position.character;
	const { expression, scopes } = findExpressionInParsedFile(parsedFile, rowIndex, columnIndex);
	if (!expression) {
		return;
	}
	//#region go to imported file
	if (isImportPath(expression)) {
		const { fullPath, error } = getPathFromImport(expression!.parent!.parent as ParseFunctionCall, folderPath);
		if (error) {
			connection.console.log(error.message);
			return;
		}
		if (!fullPath) {
			return;
		}
		const location: Location = {
			uri: pathToUri(fullPath),
			range: {
				start: { character: 0, line: 0 },
				end: { character: 0, line: 0 },
			},
		};
		return location;
	}
	//#endregion go to imported file

	const foundSymbol = getSymbolDefinition(expression, scopes, folderPath, parsedDocuments);
	const typeFields = foundSymbol
		&& !foundSymbol.isBuiltIn
		&& getFieldAccessTypeFields(
			expression,
			{ symbol: foundSymbol.symbol, filePath: foundSymbol.filePath || documentPath },
			referenceIndex);
	if (typeFields) {
		return typeFields.map((typeField): Location => ({
			uri: pathToUri(typeField.filePath),
			range: positionedToRange(typeField.symbol),
		}));
	}
	if (foundSymbol) {
		const location: Location = {
			uri: foundSymbol.isBuiltIn
				? coreLibUri
				: foundSymbol.filePath
					? pathToUri(foundSymbol.filePath)
					: documentUri,
			range: positionedToRange(foundSymbol.symbol)
		};
		return location;
	}
});
//#endregion go to definition

//#region hover
connection.onHover((hoverParams) => {
	const documentUri = hoverParams.textDocument.uri;
	const parsed = getParsedFileByUri(documentUri);
	if (!parsed) {
		return;
	}
	const folderPath = dirname(uriToPath(documentUri));
	const contents = getHover(parsed, hoverParams.position.line, hoverParams.position.character, folderPath, parsedDocuments);
	return contents && { contents: contents };
});
//#endregion hover

//#region rename
connection.onPrepareRename(prepareRenameParams => {
	const documentUri = prepareRenameParams.textDocument.uri;
	const parsedFile = getParsedFileByUri(documentUri);
	if (!parsedFile) {
		return;
	}

	const { expression, scopes } = findExpressionInParsedFile(parsedFile, prepareRenameParams.position.line, prepareRenameParams.position.character);
	if (!expression) {
		return expression;
	}
	const documentPath = uriToPath(documentUri);
	const folderPath = dirname(documentPath);
	const foundSymbol = getSymbolDefinition(expression, scopes, folderPath, parsedDocuments);
	if (!foundSymbol || foundSymbol.isBuiltIn) {
		return;
	}

	return positionedToRange(expression);
});
/**
 * Löst ein Rename-/Find-All-References-Ziel auf die kanonische Identität auf: ein Alias-Binding
 * (`local = source` in einer destructuring-Zeile) ist dabei bewusst eine eigene Identität,
 * unabhängig vom Ursprung - Cursor auf dem lokalen Alias-Namen darf den Ursprung nicht mitziehen,
 * Cursor auf dem source-Token (bzw. dem Namen ohne Alias) zeigt dagegen auf den Ursprung.
 * Ein Literalfeld an einer Stelle mit erwartetem Typ und der lokale Name eines Destructurings ohne
 * Alias zielen auf die Felder ihres Typs, bei einer Union auf mehrere.
 */
function resolveRenameTargets(
	expression: PositionedExpression,
	scopes: SymbolTable[],
	documentPath: string,
	folderPath: string,
): { symbol: SymbolDefinition; filePath: string; }[] | undefined {
	const canonical = resolveCanonicalRenameTarget(expression, scopes, documentPath, folderPath);
	return canonical && resolveRelatedTargets(canonical, referenceIndex);
}

function resolveCanonicalRenameTarget(
	expression: PositionedExpression,
	scopes: SymbolTable[],
	documentPath: string,
	folderPath: string,
): { symbol: SymbolDefinition; filePath: string; } | undefined {
	if (expression.type === 'name' && expression.parent?.type === 'destructuringField') {
		const field = expression.parent;
		const localSymbol = field.parent?.type === 'destructuringFields'
			? field.parent.symbols[field.name.name]
			: undefined;
		const local = localSymbol && {
			symbol: localSymbol,
			filePath: documentPath,
		};
		if (field.source && expression === field.name) {
			return local;
		}
		// Kein Import: der lokale Name eines Dictionary-Destructurings, über die Verknüpfung
		// landet er beim Typfeld.
		return resolveImportBinding(field, documentPath, parsedDocuments) ?? local;
	}
	const raw = getRawSymbolDefinition(expression, scopes, folderPath, parsedDocuments);
	if (!raw || raw.isBuiltIn) {
		return undefined;
	}
	return resolveCanonicalSymbol(raw.symbol, raw.filePath ?? documentPath, parsedDocuments);
}


connection.onRenameRequest(renameParams => {
	const documentUri = renameParams.textDocument.uri;
	const parsedFile = getParsedFileByUri(documentUri);
	if (!parsedFile) {
		return;
	}
	const { expression, scopes } = findExpressionInParsedFile(parsedFile, renameParams.position.line, renameParams.position.character);
	if (!expression) {
		return;
	}
	const documentPath = uriToPath(documentUri);
	const folderPath = dirname(documentPath);
	const targets = resolveRenameTargets(expression, scopes, documentPath, folderPath);
	if (!targets) {
		return;
	}
	return createRenameWorkspaceEdit(
		getRenameEdits(targets, renameParams.newName, referenceIndex, parsedDocuments),
		hasChangeAnnotationCapability);
});
//#endregion rename

//#region codeAction
function spaceIndentationTextEdit(range: Range, expectedIndent: number): TextEdit {
	return {
		range,
		newText: '\t'.repeat(expectedIndent),
	};
}

function getSpaceIndentationCodeActions(
	documentUri: string,
	parsedFile: ParsedFile,
	params: CodeActionParams,
): CodeAction[] {
	const hasSpaceIndentationHere = params.context.diagnostics
		.some(diagnostic => diagnostic.code === ErrorCode.spaceIndentation);
	if (!hasSpaceIndentationHere) {
		return [];
	}
	// Eine Zeile einzeln zu fixen bringt wenig - Space-Einrückung tritt praktisch immer gebündelt
	// auf (ein ganzer eingefügter Block oder eine ganze Datei). Deshalb nur ein Fix für alle
	// Stellen der Datei, aus dem zuletzt geprüften Stand geholt statt aus params.context.diagnostics
	// (das ist auf die angefragte Range beschränkt).
	const spaceIndentationErrors = parsedFile.checked?.errors
		.filter((error): error is typeof error & { expectedIndent: number; } =>
			error.code === ErrorCode.spaceIndentation && error.expectedIndent !== undefined)
		?? [];
	if (!spaceIndentationErrors.length) {
		return [];
	}
	return [{
		title: `Convert indentation to tabs (${spaceIndentationErrors.length} Stellen in dieser Datei)`,
		kind: CodeActionKind.QuickFix,
		isPreferred: true,
		edit: {
			changes: {
				[documentUri]: spaceIndentationErrors.map(error =>
					spaceIndentationTextEdit(positionedToRange(error), error.expectedIndent)),
			},
		},
	}];
}

/**
 * Bietet für jedes unbekannte Symbol die Dateien des Projekts an, die es definieren.
 * Anders als beim Einrückungsfix ist die Beschränkung auf params.context.diagnostics hier richtig:
 * es geht um genau die Stelle, an der der Nutzer steht.
 */
function getAutoImportCodeActions(
	documentUri: string,
	parsedFile: ParsedFile,
	params: CodeActionParams,
): CodeAction[] {
	const documentPath = uriToPath(documentUri);
	return params.context.diagnostics.flatMap(diagnostic => {
		if (diagnostic.code !== ErrorCode.notDefined) {
			return [];
		}
		const start = diagnostic.range.start;
		// Name aus dem Baum holen, nicht aus der Fehlermeldung parsen
		const { expression } = findExpressionInParsedFile(parsedFile, start.line, start.character);
		if (expression?.type !== 'reference') {
			return [];
		}
		const name = expression.name.name;
		const candidates = findImportCandidates(name, documentPath, parsedDocuments);
		return candidates.map(candidate => {
			const textEdit = createImportEdit(parsedFile, candidate, name, start.line);
			if (!textEdit) {
				return undefined;
			}
			const codeAction: CodeAction = {
				title: `Import '${name}' from '${candidate.importPath}'`,
				kind: CodeActionKind.QuickFix,
				isPreferred: candidates.length === 1,
				diagnostics: [diagnostic],
				edit: {
					changes: {
						[documentUri]: [textEdit],
					},
				},
			};
			return codeAction;
		}).filter(isDefined);
	});
}

connection.onCodeAction((params: CodeActionParams): CodeAction[] => {
	const documentUri = params.textDocument.uri;
	const parsedFile = getParsedFileByUri(documentUri);
	if (!parsedFile) {
		return [];
	}
	const lines = documents.get(documentUri)?.getText().split('\n') ?? [];
	return [
		...getSpaceIndentationCodeActions(documentUri, parsedFile, params),
		...getAutoImportCodeActions(documentUri, parsedFile, params),
		...getIgnoreCommentCodeActions(documentUri, lines, params.context.diagnostics),
	];
});
//#endregion codeAction

//#region references
connection.onReferences(referenceParams => {
	const documentUri = referenceParams.textDocument.uri;
	const parsedFile = getParsedFileByUri(documentUri);
	if (!parsedFile) {
		return;
	}
	const { expression, scopes } = findExpressionInParsedFile(parsedFile, referenceParams.position.line, referenceParams.position.character);
	if (!expression) {
		return;
	}
	const documentPath = uriToPath(documentUri);
	const folderPath = dirname(documentPath);
	const targets = resolveRenameTargets(expression, scopes, documentPath, folderPath);
	if (!targets) {
		return;
	}
	return getReferenceLocations(targets, referenceIndex, referenceParams.context.includeDeclaration)
		.map((location): Location => ({
			uri: pathToUri(location.filePath),
			range: positionedToRange(location),
		}));
});
//#endregion references

//#region document highlight
connection.onDocumentHighlight(highlightParams => {
	const documentUri = highlightParams.textDocument.uri;
	const parsedFile = getParsedFileByUri(documentUri);
	if (!parsedFile) {
		return;
	}
	const { expression, scopes } = findExpressionInParsedFile(parsedFile, highlightParams.position.line, highlightParams.position.character);
	if (!expression) {
		return;
	}
	const documentPath = uriToPath(documentUri);
	const folderPath = dirname(documentPath);
	const targets = resolveRenameTargets(expression, scopes, documentPath, folderPath);
	if (!targets) {
		return;
	}
	// Anders als bei Find-All-References/Rename: nur Vorkommen in genau diesem Dokument, kein
	// Cross-File-Ergebnis - Document Highlight ist die stille Markierung im aktuell offenen Editor.
	return getReferenceLocations(targets, referenceIndex, true)
		.filter(location => location.filePath === documentPath)
		.map((location): DocumentHighlight => ({ range: positionedToRange(location) }));
});
//#endregion document highlight

//#region document symbols
connection.onDocumentSymbol(documentSymbolParams => {
	flushPendingChanges();
	const documentUri = documentSymbolParams.textDocument.uri;
	const documentPath = uriToPath(documentUri);
	const parsedFile = parsedDocuments[documentPath];
	if (!parsedFile) {
		return;
	}
	const parsed2 = parsedFile.checked;
	if (!parsed2) {
		return;
	}
	return parsed2.expressions && getDocumentSymbolsFromExpressions(parsed2.expressions);
});

function getDocumentSymbolsFromExpressions(expressions: PositionedExpression[]): DocumentSymbol[] {
	return expressions.flatMap(expression =>
		getDocumentSymbolsFromExpression(expression));
}

function getDocumentSymbolsFromExpression(expression: PositionedExpression): DocumentSymbol[] {
	switch (expression.type) {
		case 'branching':
		case 'typeBranching':
			return [
				...(expression.args
					? getDocumentSymbolsFromExpression(expression.args)
					: []),
				...getDocumentSymbolsFromExpressions(expression.branches),
			];
		case 'definition': {
			const children = [
				...(expression.typeGuard
					? getDocumentSymbolsFromExpression(expression.typeGuard)
					: []),
				...(expression.value
					? getDocumentSymbolsFromExpression(expression.value)
					: []),
			];
			return createDocumentSymbol(expression, expression.name, getSymbolKindFromExpression(expression.value), children);
		}
		case 'destructuring':
			return [
				...getDocumentSymbolsFromExpression(expression.fields),
				...(expression.value
					? getDocumentSymbolsFromExpression(expression.value)
					: []),
			];
		case 'destructuringField': {
			const children = [
				...(expression.typeGuard
					? getDocumentSymbolsFromExpression(expression.typeGuard)
					: []),
			];
			return createDocumentSymbol(expression, expression.name, SymbolKind.Field, children);
		}
		case 'destructuringFields':
			return getDocumentSymbolsFromExpressions(expression.fields);
		case 'dictionary':
			return getDocumentSymbolsFromExpressions(expression.fields);
		case 'dictionaryType':
			return getDocumentSymbolsFromExpressions(expression.fields);
		case 'functionCall':
			return [
				...(expression.prefixArgument
					? getDocumentSymbolsFromExpression(expression.prefixArgument)
					: []),
				...(expression.arguments
					? getDocumentSymbolsFromExpression(expression.arguments)
					: []),
			];
		case 'functionLiteral':
			return [
				...getDocumentSymbolsFromExpression(expression.params),
				...(expression.returnType
					? getDocumentSymbolsFromExpression(expression.returnType)
					: []),
				...getDocumentSymbolsFromExpressions(expression.body),
			];
		case 'functionTypeLiteral':
			return [
				...getDocumentSymbolsFromExpression(expression.params),
				...getDocumentSymbolsFromExpression(expression.returnType),
			];
		case 'list':
			return getDocumentSymbolsFromExpressions(expression.values);
		case 'parameter':
			return createDocumentSymbol(expression, expression.name, SymbolKind.Variable);
		case 'parameters':
			return getDocumentSymbolsFromExpressions(expression.singleFields);
		case 'singleDictionaryField': {
			const children = expression.value && getDocumentSymbolsFromExpression(expression.value);
			return createDocumentSymbol(expression, expression.name, SymbolKind.Field, children);
		}
		case 'singleDictionaryTypeField': {
			const children = expression.typeGuard && getDocumentSymbolsFromExpression(expression.typeGuard);
			return createDocumentSymbol(expression, expression.name, SymbolKind.Field, children);
		}
		case 'binding':
		case 'data':
		case 'empty':
		case 'field':
		case 'float':
		case 'fraction':
		case 'index':
		case 'integer':
		case 'name':
		case 'nestedReference':
		case 'object':
		case 'reference':
		case 'spread':
		case 'text':
			return [];
		default: {
			const assertNever: never = expression;
			throw new Error(`Unexpected expression.type for getDocumentSymbolsFromExpression: ${(assertNever as PositionedExpression).type}`);
		}
	}
}

function getSymbolKindFromExpression(expression: ParseValueExpression | undefined): SymbolKind {
	return expression?.type === 'functionLiteral'
		? SymbolKind.Function
		: SymbolKind.Constant;
}

function createDocumentSymbol(
	definitionPosition: Positioned,
	nameExpression: PositionedExpression,
	kind: SymbolKind,
	children?: DocumentSymbol[],
): DocumentSymbol[] {
	const nameString = getCheckedEscapableName(nameExpression);
	if (nameString === undefined) {
		return [];
	}
	const documentSymbol: DocumentSymbol = {
		kind: kind,
		name: nameString,
		range: positionedToRange(definitionPosition),
		selectionRange: positionedToRange(nameExpression),
		children: children,
	};
	return [documentSymbol];
}
//#endregion document symbols

// Make the text document manager listen on the connection
// for open, change and close text document events
documents.listen(connection);

// Listen on the connection
connection.listen();

//#region helper

// Rename/Find-All-References laufen über den ReferenceIndex (siehe oben, Region "rename"/
// "references") statt über eine Textsuche pro Datei - der Index kennt die tatsächlich aufgelösten
// Bindungen (inkl. Scope/Shadowing/Cross-File), eine Textsuche wäre hier nur eine Annäherung.

/**
 * Ermittelt den Index des Parameters, für den das Argument ist, das an der Position liegt.
 * Position muss in arguments liegen.
 */
function isImportPath(expression: PositionedExpression | undefined): boolean {
	if (expression
		&& expression.type === 'text'
		&& expression.parent?.type === 'list'
		&& expression.parent.parent
		&& isImportFunctionCall(expression.parent.parent)) {
		return true;
	}
	return false;
}

/**
 * Der Compiler kennt die LSP-Zahlen nicht - er läuft auch als CLI.
 * Hier werden seine Werte auf DiagnosticSeverity übersetzt.
 */
const diagnosticSeverities: { [Severity in CompilerErrorSeverity]: DiagnosticSeverity; } = {
	error: DiagnosticSeverity.Error,
	warning: DiagnosticSeverity.Warning,
	hint: DiagnosticSeverity.Hint,
};

//#region uri

function uriToPath(uri: string): string {
	return URI.parse(uri).fsPath;
}

function getParsedFileByUri(uri: string): ParsedFile | undefined {
	flushPendingChanges();
	const path = uriToPath(uri);
	const parsed = parsedDocuments[path];
	return parsed;
}

//#endregion uri

function tryReadTextFile(path: string): string | undefined {
	try {
		return readFileSync(path).toString();
	}
	catch (error) {
		console.error(error);
		return undefined;
	}
}

//#endregion helper
