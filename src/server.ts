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
	MarkupContent,
	ParameterInformation,
	ProposedFeatures,
	Range,
	SemanticTokensBuilder,
	SignatureHelp,
	SymbolKind,
	TextDocumentIdentifier,
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
import { loadFile, ProjectHost } from 'jul-compiler/out/project-loader.js';
import { getCheckedEscapableName, isExportedSymbol } from 'jul-compiler/out/parser/parser-utils.js';
import { CompilerErrorSeverity, ErrorCode, errorInfos, Positioned } from 'jul-compiler/out/compiler-errors.js';
import {
	CompileTimeType,
	DefinitionExpression,
	forEachChild,
	ImportedDependency,
	PositionedExpression,
	Parameter,
	ParseDestructuringField,
	ParseDestructuringFields,
	ParsedFile,
	ParseFunctionCall,
	ParseValueExpression,
	SymbolDefinition,
	SymbolTable,
	TextLiteralType,
	TypeInfo,
} from 'jul-compiler/out/syntax-tree.js';
import {
	resolvePlaceholders,
	builtInSymbols,
	checkTypes,
	findSymbolInScopesWithBuiltIns,
	getStreamGetValueType,
	isDictionaryLiteralType,
	isFunctionType,
	isParameterReference,
	isParametersType,
	isTextLiteralType,
	isTypeOfType,
	ParsedDocuments,
	typeToString,
} from 'jul-compiler/out/checker/checker.js';
import { ReferenceIndex, getFieldSymbolsFromDictionaryType, resolveCanonicalSymbol, resolveImportBinding } from 'jul-compiler/out/checker/reference-index.js';
import { isDefined, isValidExtension, map } from 'jul-compiler/out/util.js';
import { createImportEdit, findImportCandidates } from './auto-import.js';
import {
	createRenameWorkspaceEdit,
	getFieldAccessTypeFields,
	getReferenceLocations,
	getRenameEdits,
	resolveRelatedTargets,
} from './references.js';
import {
	dictionaryTypeToCompletionItems,
	getArgumentPositionKind,
	getCompletionSortText,
	getDictionaryLiteralFieldCompletionItems,
	getExpectedPositionKind,
	getFirstArgumentSymbolFilter,
	getInfixFunctionCall,
	isTypeSymbol,
} from './completion.js';
import {
	getDeclaredResolvedType,
	getDeclaredType,
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
				legend: {
					tokenTypes: [...semanticTokenTypes],
					tokenModifiers: [...semanticTokenModifiers],
				},
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
function sendDiagnosticsForFile(uri: string, parsed: ParsedFile): void {
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
	connection.sendDiagnostics({ uri, diagnostics });
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
documents.onDidChangeContent(change => {
	const textDocument = change.document;
	const text = textDocument.getText();
	if (text.length > maxFileSize) {
		return;
	}
	const path = uriToPath(textDocument.uri);
	const parsed = parseDocumentByCode(text, path);
	sendDiagnosticsForFile(textDocument.uri, parsed);
	// Andere Dateien importieren evtl. diese - ihre Typen/Referenzen müssen neu berechnet werden.
	recheckDependents(path);
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
	referenceIndex: referenceIndex,
	onParsed: (parsed, previous) => {
		unregisterDependencies(parsed.filePath, previous?.dependencies);
		registerDependencies(parsed.filePath, parsed.dependencies);
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
	// Monitored files have change in VSCode
	const changedFilePaths = changeParams.changes
		.map(fileChange => uriToPath(fileChange.uri))
		.filter(path => {
			return !!parsedDocuments[path];
		});

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
		return dereferencedType && getNestedReferenceCompletionItems(dereferencedType);
	}
	//#endregion / field reference

	//#region dictionary literal field
	const dictionaryLiteralFieldCompletionItems = getDictionaryLiteralFieldCompletionItems(expression);
	if (dictionaryLiteralFieldCompletionItems) {
		return dictionaryLiteralFieldCompletionItems;
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
					declaredParameter.source === singleName.name
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

	return symbolsToCompletionItems(allScopes, undefined, positionKind);
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

function getNestedReferenceCompletionItems(dereferencedType: CompileTimeType): CompletionItem[] {
	switch (dereferencedType.julType) {
		case 'dictionaryLiteral':
			return dictionaryTypeToCompletionItems(dereferencedType.Fields);
		case 'function':
			return functionTypeToCompletionItems(dereferencedType);
		case 'or': {
			const completionItems = dereferencedType.ChoiceTypes.flatMap(choiceType => getNestedReferenceCompletionItems(choiceType));
			return completionItems;
		}
		case 'stream': {
			const getValueType = getStreamGetValueType(dereferencedType);
			return [
				{
					label: 'getValue',
					kind: CompletionItemKind.Function,
					detail: typeToString(getValueType, 0, 0),
				},
				// TODO? nur bei TypeOf(Stream)
				{
					label: 'ValueType',
					kind: CompletionItemKind.Constant,
					detail: typeToString(dereferencedType.ValueType, 0, 0),
				},
			];
		}
		default:
			return [];
	}
}

function functionTypeToCompletionItems(functionType: CompileTimeType | undefined): CompletionItem[] {
	// TODO ParamsType, ReturnType stattdessen als symbols?
	let paramsType: CompileTimeType | undefined;
	let returnType: CompileTimeType | undefined;
	if (isFunctionType(functionType)) {
		returnType = functionType.ReturnType;
		paramsType = functionType.ParamsType;
	}
	return [
		{
			label: 'ParamsType',
			kind: CompletionItemKind.Constant,
			detail: paramsType
				? typeToString(paramsType, 0, 0)
				: undefined,
		},
		{
			label: 'ReturnType',
			kind: CompletionItemKind.Constant,
			detail: returnType
				? typeToString(returnType, 0, 0)
				: undefined,
		},
	];
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

//#region empty literals
// Das Empty-Literal [] ist ein eigener Wert, wird im Editor aber wie ein leeres Klammernpaar
// gefärbt: die bracket pair colorization übermalt jede Farbe aus Grammatik und Semantic Tokens.
// Nur eine Decoration liegt darüber, und die braucht diese Positionen. Siehe extension.ts.
connection.onRequest('jul/emptyLiterals', (params: TextDocumentIdentifier) => {
	const parsedFile = getParsedFileByUri(params.uri);
	const expressions = parsedFile?.checked?.expressions;
	if (!expressions) {
		return [];
	}
	const ranges: Range[] = [];
	expressions.forEach(expression => collectEmptyLiterals(expression, ranges));
	return ranges;
});

function collectEmptyLiterals(expression: PositionedExpression, ranges: Range[]): void {
	if (expression.type === 'empty') {
		ranges.push(positionedToRange(expression));
	}
	forEachChild(expression, child => {
		collectEmptyLiterals(child, ranges);
		return undefined;
	});
}
//#endregion empty literals

//#region semantic tokens
// Die Grammatik rät den Bezeichnertyp an der Schreibweise. Der checker weiß ihn - Werte und Typen
// teilen in JUL denselben Namensraum, dort rät die Grammatik zwangsläufig falsch.
const semanticTokenTypes = ['namespace', 'type', 'function', 'parameter', 'variable', 'property'] as const;
const semanticTokenModifiers = ['declaration', 'defaultLibrary', 'readonly', 'stream'] as const;
type SemanticTokenType = typeof semanticTokenTypes[number];
type SemanticTokenModifier = typeof semanticTokenModifiers[number];

interface SemanticTokenInfo {
	line: number;
	character: number;
	length: number;
	tokenType: number;
	tokenModifiers: number;
}

connection.languages.semanticTokens.on(params => {
	// Dateien über maxFileSize stehen gar nicht erst in parsedDocuments.
	const parsedFile = getParsedFileByUri(params.textDocument.uri);
	const checked = parsedFile?.checked;
	const expressions = checked?.expressions;
	if (!checked || !expressions) {
		return { data: [] };
	}
	const tokens: SemanticTokenInfo[] = [];
	const scopes: SymbolTable[] = [checked.symbols];
	expressions.forEach(expression => collectSemanticTokens(expression, scopes, tokens));
	// Der builder verlangt aufsteigende Positionen. forEachChild liefert Quelltextreihenfolge,
	// aber ein infix call stellt das Aufrufziel hinter sein erstes Argument.
	tokens.sort((a, b) => a.line - b.line || a.character - b.character);
	const builder = new SemanticTokensBuilder();
	tokens.forEach(token => builder.push(
		token.line,
		token.character,
		token.length,
		token.tokenType,
		token.tokenModifiers));
	return builder.build();
});

function collectSemanticTokens(
	expression: PositionedExpression,
	scopes: SymbolTable[],
	tokens: SemanticTokenInfo[],
): void {
	addSemanticToken(expression, scopes, tokens);
	const pushedScope = pushScope(expression, scopes);
	forEachChild(expression, child => {
		collectSemanticTokens(child, scopes, tokens);
		return undefined;
	});
	if (pushedScope) {
		scopes.pop();
	}
}

function addSemanticToken(
	expression: PositionedExpression,
	scopes: SymbolTable[],
	tokens: SemanticTokenInfo[],
): void {
	switch (expression.type) {
		case 'reference': {
			// true/false bekommen keinen Semantic Token: sie sollen wie Literale gefärbt werden
			// (Grammatik-Scope constant.language.boolean.jul), nicht wie eine eingebaute Variable
			// oder ein Typ-Pattern (z.B. [true] => ...  löst als TypeOf(booleanLiteral) auf).
			const referencedType = expression.typeInfo?.type;
			if (referencedType?.julType === 'booleanLiteral'
				|| (isTypeOfType(referencedType) && referencedType.value.julType === 'booleanLiteral')) {
				return;
			}
			// import bekommt ebenfalls keinen Semantic Token: es ist nur als direkter Aufruf
			// unterstützt (JUL3040), soll also wie ein Keyword gefärbt werden (Grammatik-Scope
			// keyword.control.import.jul), nicht wie eine eingebaute Funktion/Variable.
			if (expression.name.name === 'import') {
				return;
			}
			const found = findSymbolInScopesWithBuiltIns(expression.name.name, scopes);
			pushSemanticToken(
				tokens,
				expression.name,
				getSemanticTokenType(expression.typeInfo, found?.symbol),
				getSemanticTokenModifiers(expression.typeInfo, found?.isBuiltIn));
			return;
		}
		case 'definition': {
			// Die Deklarationen von import/true/false in core-lib.jul bekommen ebenfalls keinen
			// Semantic Token, aus demselben Grund wie an der jeweiligen Referenzstelle oben.
			if (expression.name.name === 'import') {
				return;
			}
			const definedType = expression.typeInfo?.type;
			if (definedType?.julType === 'booleanLiteral'
				|| (isTypeOfType(definedType) && definedType.value.julType === 'booleanLiteral')) {
				return;
			}
			pushSemanticToken(
				tokens,
				expression.name,
				getSemanticTokenType(expression.typeInfo, findSymbolInScopes(expression.name.name, scopes)),
				['declaration', ...getSemanticTokenModifiers(expression.typeInfo, false)]);
			return;
		}
		case 'parameter':
			pushSemanticToken(tokens, expression.name, 'parameter', ['declaration']);
			return;
		case 'destructuringField':
			pushSemanticToken(tokens, expression.name, 'variable', ['declaration']);
			if (expression.source) {
				pushSemanticToken(tokens, expression.source, 'property', []);
			}
			return;
		case 'singleDictionaryField':
		case 'singleDictionaryTypeField':
			if (expression.name.type === 'name') {
				pushSemanticToken(tokens, expression.name, 'property', ['declaration']);
			}
			return;
		case 'nestedReference':
			if (expression.nestedKey?.type === 'name') {
				pushSemanticToken(tokens, expression.nestedKey, 'property', []);
			}
			return;
		default:
			return;
	}
}

function findSymbolInScopes(name: string, scopes: SymbolTable[]): SymbolDefinition | undefined {
	for (let index = scopes.length - 1; index >= 0; index--) {
		const symbol = scopes[index]![name];
		if (symbol) {
			return symbol;
		}
	}
	return undefined;
}

function getSemanticTokenType(
	typeInfo: TypeInfo | undefined,
	symbol: SymbolDefinition | undefined,
): SemanticTokenType {
	// Vor allen Typprüfungen: ein importiertes Modul kann selbst eine Funktion oder ein Typ sein.
	if (symbol?.definition && isImportDefinition(symbol.definition)) {
		return 'namespace';
	}
	if (symbol?.definition?.type === 'parameter') {
		return 'parameter';
	}
	// Der unaufgelöste Typ genügt: gefragt ist die Art des Bezeichners, nicht sein Inhalt.
	// resolvePlaceholders pro Referenz kostet mehr als der ganze restliche Durchlauf.
	const type = typeInfo?.type;
	if (isTypeOfType(type) || type?.julType === 'type') {
		return 'type';
	}
	if (isFunctionType(type)) {
		return 'function';
	}
	return 'variable';
}

function getSemanticTokenModifiers(
	typeInfo: TypeInfo | undefined,
	isBuiltIn: boolean | undefined,
): SemanticTokenModifier[] {
	const modifiers: SemanticTokenModifier[] = [];
	if (isBuiltIn) {
		modifiers.push('defaultLibrary');
	}
	// Streams haben keinen eigenen LSP-Tokentyp. Der Modifier heißt wie die Sache; die Farbe
	// liefert die semanticTokenScopes-Contribution der Extension, kein Theme kennt ihn von selbst.
	if (typeInfo?.type.julType === 'stream') {
		modifiers.push('stream');
	}
	return modifiers;
}

function isImportDefinition(definition: DefinitionExpression): boolean {
	const value = definition.type === 'definition'
		? definition.value
		: undefined;
	return value?.type === 'functionCall' && isImportFunctionCall(value);
}

function pushSemanticToken(
	tokens: SemanticTokenInfo[],
	positioned: Positioned,
	tokenType: SemanticTokenType,
	modifiers: SemanticTokenModifier[],
): void {
	// LSP kennt keine mehrzeiligen Tokens.
	if (positioned.startRowIndex !== positioned.endRowIndex) {
		return;
	}
	const length = positioned.endColumnIndex - positioned.startColumnIndex;
	if (length < 1) {
		return;
	}
	// LSP hat keinen Tokentyp für Konstanten. JUL kennt keine Variablen - jede Bindung ist
	// konstant, also trägt jeder Bezeichner readonly.
	const allModifiers: SemanticTokenModifier[] = isBinding(tokenType)
		? [...modifiers, 'readonly']
		: modifiers;
	tokens.push({
		line: positioned.startRowIndex,
		character: positioned.startColumnIndex,
		length: length,
		tokenType: semanticTokenTypes.indexOf(tokenType),
		tokenModifiers: allModifiers.reduce(
			(combined, modifier) => combined | (1 << semanticTokenModifiers.indexOf(modifier)),
			0),
	});
}

function isBinding(tokenType: SemanticTokenType): boolean {
	switch (tokenType) {
		case 'variable':
		case 'parameter':
		case 'property':
			return true;
		default:
			return false;
	}
}
//#endregion semantic tokens

//#region go to definition
// Go to definition auf builtIns führt in die core-lib. Statt die kompilierte Kopie in out/
// als editierbare Datei zu öffnen, liefert der Server ihren Inhalt an ein read only
// virtual document des Clients. Siehe extension.ts, coreLibScheme.
const coreLibUri = 'jul-core-lib:/core-lib.jul';
connection.onRequest('jul/coreLibContent', () =>
	tryReadTextFile(coreLibPath) ?? '');
connection.onDefinition((definitionParams) => {
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

	const foundSymbol = getSymbolDefinition(expression, scopes, folderPath);
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
	const { expression, scopes } = findExpressionInParsedFile(parsed, hoverParams.position.line, hoverParams.position.character);
	if (!expression) {
		return;
	}

	const documentPath = uriToPath(documentUri);
	const folderPath = dirname(documentPath);
	const foundSymbol = getSymbolDefinition(expression, scopes, folderPath);
	if (foundSymbol) {
		const symbol = foundSymbol.symbol;
		// Der Typ gehört der Stelle, nicht dem Namen: in einem branch ist er hier verengt.
		// Die Beschreibung steht dagegen nur an der Definition.
		return {
			contents: getTypeMarkdown(('typeInfo' in expression && expression.typeInfo) || symbol.typeInfo, symbol.description),
		};
	}

	const declaredType = getDeclaredType(expression);
	if (declaredType) {
		return {
			contents: getTypeMarkdown(declaredType, undefined),
		};
	}
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
	const foundSymbol = getSymbolDefinition(expression, scopes, folderPath);
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
	const raw = getRawSymbolDefinition(expression, scopes, folderPath);
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
	return [
		...getSpaceIndentationCodeActions(documentUri, parsedFile, params),
		...getAutoImportCodeActions(documentUri, parsedFile, params),
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

//#region findExpression

/**
 * Liefert auch scopes
 */
function findExpressionInParsedFile(
	parsedFile: ParsedFile,
	rowIndex: number,
	columnIndex: number,
): {
	expression: PositionedExpression | undefined;
	scopes: SymbolTable[];
} {
	const parsed2 = parsedFile.checked!;
	const scopes: SymbolTable[] = [
		parsed2.symbols,
	];
	const expressions = parsed2.expressions;
	const expression = expressions && findExpressionInExpressions(
		expressions,
		rowIndex,
		columnIndex,
		scopes);
	return {
		expression: expression,
		scopes: scopes,
	};
}

/**
 * Füllt scopes
 */
function findExpressionInExpressions(
	expressions: PositionedExpression[],
	rowIndex: number,
	columnIndex: number,
	scopes: SymbolTable[],
): PositionedExpression | undefined {
	const foundOuter = expressions.find(expression => {
		return isPositionInRange(rowIndex, columnIndex, expression);
	});
	if (!foundOuter) {
		return undefined;
	}
	const foundInner = findExpressionInExpression(foundOuter, rowIndex, columnIndex, scopes);
	return foundInner;
}

/**
 * Füllt scopes
 * Gibt die gegebene expression zurück, falls keine passende innere expression gefunden wurde.
 */
function findExpressionInExpression(
	expression: PositionedExpression,
	rowIndex: number,
	columnIndex: number,
	scopes: SymbolTable[],
): PositionedExpression {
	pushScope(expression, scopes);
	const found = forEachChild(expression, child =>
		isPositionInRange(rowIndex, columnIndex, child)
			? findExpressionInExpression(child, rowIndex, columnIndex, scopes)
			: undefined);
	return found ?? expression;
}

/** Nur diese beiden bringen einen eigenen Scope mit. */
function pushScope(expression: PositionedExpression, scopes: SymbolTable[]): boolean {
	switch (expression.type) {
		case 'functionLiteral':
		case 'functionTypeLiteral':
			scopes.push(expression.symbols);
			return true;
		default:
			return false;
	}
}

function isPositionInRange(
	rowIndex: number,
	columnIndex: number,
	range: Positioned,
): boolean {
	return (range.startRowIndex < rowIndex
		|| (range.startRowIndex === rowIndex && range.startColumnIndex <= columnIndex))
		&& (range.endRowIndex > rowIndex
			|| (range.endRowIndex === rowIndex && range.endColumnIndex >= columnIndex));
}

//#endregion findExpression

// Rename/Find-All-References laufen über den ReferenceIndex (siehe oben, Region "rename"/
// "references") statt über eine Textsuche pro Datei - der Index kennt die tatsächlich aufgelösten
// Bindungen (inkl. Scope/Shadowing/Cross-File), eine Textsuche wäre hier nur eine Annäherung.

//#region get Symbol

interface SymbolInfo {
	isBuiltIn: boolean;
	symbol: SymbolDefinition;
	name: string;
	/**
	 * undefined, wenn Symbol in gleicher Datei gefunden
	 * Leerstring, wenn builtin.
	 */
	filePath?: string;
}

/**
 * Löst den Ausdruck auf das lokal gebundene Symbol auf, ohne durch Importe hindurchzufolgen.
 * Für Go-to-Definition/Hover wird das Ergebnis über `resolveThroughImports` weitergereicht
 * (siehe `getSymbolDefinition`); Rename/Find-All-References brauchen dagegen genau diese
 * ungefolgte, lokale Bindung, um sie alias-bewusst über `resolveCanonicalSymbol` (jul-compiler)
 * aufzulösen - ein Alias darf dort nicht wie beim Go-to-Definition blind mitgezogen werden.
 */
function getRawSymbolDefinition(
	expression: PositionedExpression,
	scopes: SymbolTable[],
	folderPath: string,
): SymbolInfo | undefined {
	switch (expression.type) {
		case 'reference': {
			const name = expression.name.name;
			const definition = findSymbolInScopesWithBuiltIns(name, scopes);
			return definition && {
				...definition,
				name: name,
			};
		}
		case 'definition': {
			// TODO GoToDefinition: bei import: go to source file symbol?
			// create dictionary type mit allen definitions?
			return undefined;
		}
		case 'destructuring': {
			// TODO stattdessen bei name case, destrucuring als parent expression?
			// TODO GoToDefinition: bei import: go to source file symbol
			// if (isImport(expression.value)) {
			// 	const importedPath = getPathFromImport(expression.value);
			// 	const importedFile = parsedDocuments[importedPath];
			// 	const importedSymbol = importedFile?.symbols[expression.fields];
			// }
			return undefined;
		}
		case 'name': {
			const parent = expression.parent;
			const name = expression.name;
			switch (parent?.type) {
				case 'destructuringField': {
					const importedSymbol = getImportedSymbol(parent, folderPath);
					if (importedSymbol) {
						return importedSymbol.symbol && {
							name: name,
							isBuiltIn: false,
							symbol: importedSymbol.symbol,
							filePath: importedSymbol.filePath,
						};
					}
					// Kein Import: der lokale Name bindet selbst, wie bei einer normalen Definition.
					// Den Typ trägt das Symbol im Scope, nicht das in destructuringFields.symbols.
					if (expression !== parent.name) {
						return undefined;
					}
					const definition = findSymbolInScopesWithBuiltIns(name, scopes);
					return definition && {
						...definition,
						name: name,
					};
				}
				case 'nestedReference': {
					const declaredSourceType = getDeclaredType(parent.source);
					const sourceType = getResolvedType(declaredSourceType ?? parent.source.typeInfo);
					const foundSymbol = sourceType && getSymbolFromDictionaryType(sourceType, name);
					return foundSymbol;
				}
				case 'singleDictionaryField':
				case 'singleDictionaryTypeField': {
					const declaredParentType = getDeclaredResolvedType(parent.parent!);
					const foundSymbol = declaredParentType && getSymbolFromDictionaryType(declaredParentType, name);
					return foundSymbol;
				}
				default: {
					const definition = findSymbolInScopesWithBuiltIns(name, scopes);
					return definition && {
						...definition,
						name: name,
					};
				}
			}
		}
		case 'binding':
		case 'data':
		case 'branching':
		case 'typeBranching':
		case 'destructuringField':
		case 'destructuringFields':
		case 'dictionary':
		case 'dictionaryType':
		case 'empty':
		case 'field':
		case 'float':
		case 'fraction':
		case 'functionCall':
		case 'functionLiteral':
		case 'functionTypeLiteral':
		case 'index':
		case 'integer':
		case 'list':
		case 'nestedReference':
		case 'object':
		case 'parameter':
		case 'parameters':
		case 'singleDictionaryField':
		case 'singleDictionaryTypeField':
		case 'spread':
		case 'text':
			return undefined;
		default: {
			const assertNever: never = expression;
			throw new Error(`Unexpected expression.type: ${(assertNever as PositionedExpression).type}`);
		}
	}
}

/**
 * Für Go-to-Definition/Hover: wie `getRawSymbolDefinition`, folgt aber zusätzlich durch Importe
 * (auch Aliase) bis zur tatsächlichen Deklaration durch, siehe `resolveThroughImports`.
 */
function getSymbolDefinition(
	expression: PositionedExpression,
	scopes: SymbolTable[],
	folderPath: string,
): SymbolInfo | undefined {
	const raw = getRawSymbolDefinition(expression, scopes, folderPath);
	if (!raw) {
		return undefined;
	}
	const rawFolderPath = raw.filePath
		? dirname(raw.filePath)
		: folderPath;
	return resolveThroughImports(raw, rawFolderPath);
}

function getSymbolFromDictionaryType(
	dictionaryType: CompileTimeType,
	name: string,
): SymbolInfo | undefined {
	// TODO bei Union: Liste aller Treffer liefern statt nur des ersten?
	const found = getFieldSymbolsFromDictionaryType(dictionaryType, name)[0];
	return found && {
		name: name,
		isBuiltIn: found.filePath === '',
		symbol: found.symbol,
		filePath: found.filePath,
	};
}

function getImportedSymbol(
	destructuringField: ParseDestructuringField,
	folderPath: string,
): {
	symbol: SymbolDefinition | undefined;
	filePath: string;
} | undefined {
	if (destructuringField.parent?.type === 'destructuringFields') {
		const destructuring = destructuringField.parent.parent;
		if (destructuring?.type === 'destructuring'
			&& destructuring.value
			&& isImportFunctionCall(destructuring.value)) {
			const { fullPath, error } = getPathFromImport(destructuring.value, folderPath);
			if (error) {
				connection.console.log(error.message);
				return;
			}
			if (!fullPath) {
				return;
			}
			const importedDocument = parsedDocuments[fullPath];
			if (importedDocument) {
				const symbolName = destructuringField.source ?? destructuringField.name;
				const impordedExpressions = importedDocument.checked ?? importedDocument.unchecked;
				const importedSymbol = impordedExpressions.symbols[symbolName.name];
				return {
					symbol: importedSymbol && isExportedSymbol(importedSymbol)
						? importedSymbol
						: undefined,
					filePath: fullPath,
				};
			}
		}
	}
}

// Löst Verweise auf importierte Symbole direkt bis zur tatsächlichen Deklaration auf, statt an der
// lokalen Import-Zeile stehen zu bleiben. Ein Hop genügt, exportiert werden nur Definitionen.
function resolveThroughImports(symbolInfo: SymbolInfo, folderPath: string): SymbolInfo {
	const definition = symbolInfo.symbol.definition;
	if (definition?.type !== 'destructuringField') {
		return symbolInfo;
	}
	const imported = getImportedSymbol(definition, folderPath);
	if (!imported?.symbol) {
		return symbolInfo;
	}
	return {
		name: symbolInfo.name,
		isBuiltIn: false,
		symbol: imported.symbol,
		filePath: imported.filePath,
	};
}

//#endregion get Symbol

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

function getTypeMarkdown(
	type: TypeInfo | undefined,
	description: string | undefined,
): MarkupContent {
	const typeString = type
		? `\`\`\`jul
${typeToString(resolvePlaceholders(type.type), 0, 0)}
\`\`\`
`
		: '';
	return {
		kind: 'markdown',
		value: typeString + (description ?? ''),
	};
}

//#endregion helper
