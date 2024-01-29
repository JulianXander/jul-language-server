import {
	CompletionItem,
	CompletionItemKind,
	createConnection,
	Diagnostic,
	DiagnosticSeverity,
	Hover,
	InitializeParams,
	InitializeResult,
	Location,
	ProposedFeatures,
	Range,
	TextDocuments,
	TextDocumentSyncKind,
} from 'vscode-languageserver';
import {
	LanguageService,
	getLanguageService as getHtmlLanguageService,
} from 'vscode-html-languageservice';
import { dirname, extname, join } from 'path';
import {
	TextDocument
} from 'vscode-languageserver-textdocument';
import { URI } from 'vscode-uri';
import { coreLibPath, getPathFromImport, isImportFunctionCall, parseCode } from 'jul-compiler/out/parser/parser.js';
import { Positioned } from 'jul-compiler/out/parser/parser-combinator.js';
import {
	PositionedExpression,
	ParseValueExpression,
	SymbolTable,
	ParsedFile,
	SymbolDefinition,
	Reference,
	Name,
	ParseFunctionCall,
	ParseTextLiteral,
} from 'jul-compiler/out/syntax-tree.js';
import {
	builtInSymbols,
	checkTypes,
	dereferenceInferredType,
	dereferenceNameFromObject,
	findSymbolInScopesWithBuiltIns,
	getTypeError,
	ParsedDocuments,
	typeToString,
} from 'jul-compiler/out/checker.js';
import { isDefined, isValidExtension, map, tryReadTextFile } from 'jul-compiler/out/util.js';
import { BuiltInTypeBase, FunctionType, ListType, ParameterReference, ParametersType, RuntimeType, TupleType } from 'jul-compiler/out/runtime.js';
import { readdirSync } from 'fs';

// Create a connection for the server, using Node's IPC as a transport.
// Also include all preview / proposed LSP features.
const connection = createConnection(ProposedFeatures.all as any, undefined as any);

// Create a simple text document manager.
const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);
const parsedDocuments: ParsedDocuments = {};

let hasDiagnosticRelatedInformationCapability = false;
let htmlLanguageService: LanguageService;

connection.onInitialize((params: InitializeParams) => {
	htmlLanguageService = getHtmlLanguageService();

	const capabilities = params.capabilities;

	hasDiagnosticRelatedInformationCapability = !!capabilities.textDocument?.publishDiagnostics?.relatedInformation;

	const result: InitializeResult = {
		capabilities: {
			textDocumentSync: TextDocumentSyncKind.Incremental,
			// Tell the client that this server supports code completion.
			completionProvider: {
				resolveProvider: true,
				triggerCharacters: ['.', '/'],
			},
			definitionProvider: true,
			hoverProvider: true,
			renameProvider: {
				prepareProvider: true,
			},
		},
	};
	return result;
});

//#region validate
// This event is emitted when the text document first opened or when its content has changed.
documents.onDidChangeContent(change => {
	validateTextDocument(change.document);
});

async function validateTextDocument(textDocument: TextDocument): Promise<void> {
	const uri = textDocument.uri;
	const text = textDocument.getText();
	const path = uriToPath(uri);
	const parsed = parseDocumentByCode(text, path);
	if (coreLibUri === uri) {
		// errors in core-lib ignorieren
		return;
	}
	const { errors } = parsed.checked!;
	const diagnostics: Diagnostic[] = errors.map(error => {
		const diagnostic: Diagnostic = {
			severity: DiagnosticSeverity.Error,
			range: positionedToRange(error),
			message: error.message,
			source: 'jul'
		};
		// if (hasDiagnosticRelatedInformationCapability) {
		// 	diagnostic.relatedInformation = [
		// 		{
		// 			location: {
		// 				uri: textDocument.uri,
		// 				range: Object.assign({}, diagnostic.range)
		// 			},
		// 			message: 'Spelling matters'
		// 		},
		// 		{
		// 			location: {
		// 				uri: textDocument.uri,
		// 				range: Object.assign({}, diagnostic.range)
		// 			},
		// 			message: 'Particularly for names'
		// 		}
		// 	];
		// }
		return diagnostic;
	});
	// Send the computed diagnostics to VSCode.
	connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });
}

/**
 * TODO check cyclic imports
 * füllt parsedDocuments
 * verarbeitet auch importe
 * checks types
 */
function parseDocumentByCode(text: string, path: string): ParsedFile {
	const parsed = parseCode(text, path);
	parsedDocuments[path] = parsed;
	// recursively parse imported files
	parsed.dependencies?.forEach(importedPath => {
		parseDocumentByPath(importedPath);
	});
	// TODO invalidate imported inferred types of this file in other files (that reference this file)?
	// oder nur in onDidChangeWatchedFiles bei file save?
	// infertypes, typecheck
	checkTypes(parsed, parsedDocuments);
	return parsed;
}

function parseDocumentByPath(path: string): void {
	const oldParsed = parsedDocuments[path];
	if (oldParsed) {
		if (oldParsed.checked) {
			return;
		}
		checkTypes(oldParsed, parsedDocuments);
		return;
	}
	const code = tryReadTextFile(path);
	if (code === undefined) {
		return;
	}
	parseDocumentByCode(code, path);
}
//#endregion validate

connection.onDidChangeWatchedFiles(changeParams => {
	// Monitored files have change in VSCode
	// connection.console.log('We received a file change event');

	// reparse changedFiles
	const changedFilePaths = changeParams.changes
		.map(fileChange => uriToPath(fileChange.uri))
		.filter(path => {
			return !!parsedDocuments[path];
		});
	// update dependendent files
	// for each file in parsedDocuments:
	// if depenency has changed (ie: file.dependencies contains a file in changeParams)
	// then recalculate types (checkTypes)
	const invalidatedFiles = map(
		parsedDocuments,
		(parsedDocument, path) => {
			const fileChanged = changedFilePaths.includes(path);
			if (fileChanged) {
				return undefined;
			}
			const dependencyChanged = parsedDocument.dependencies?.some(dependency => changedFilePaths.includes(dependency));
			if (dependencyChanged) {
				return parsedDocument;
			}
		}).filter(isDefined);
	// clean invalidated data
	invalidatedFiles.forEach((parsedDocument) => {
		parsedDocument.checked = undefined;
	});
	changedFilePaths.forEach((path) => {
		delete parsedDocuments[path];
	});
	// recalculate data
	changedFilePaths.forEach((path) => {
		parseDocumentByPath(path);
	});
	invalidatedFiles.forEach((parsedDocument) => {
		checkTypes(parsedDocument, parsedDocuments);
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

	//#region embbeded language
	const embeddedLanguage = expression?.type === 'text' && expression.language;
	switch (embeddedLanguage) {
		case 'html':
			const textDocument = documents.get(documentUri)
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

	//#region import path
	if (isImportPath(expression)) {
		const folderPath = dirname(documentPath);
		const rawImportedPath = expression.values[0]?.type === 'textToken'
			? expression.values[0].value
			: undefined;
		let entryFolderPath = folderPath;
		let entries;
		if (rawImportedPath) {
			entryFolderPath = join(folderPath, rawImportedPath);
			try {
				entries = readdirSync(entryFolderPath, { withFileTypes: true });
			} catch (error) {
				console.error(error);
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
				const entryFilePath = join(entryFolderPath, entryName)
				if (entryFilePath === documentPath) {
					return undefined;
				}
			}
			// nur Dateien mit importierbarer extension vorschlagen
			// TODO nur Ordner, die importierbare Dateien enthalten vorschlagen
			const extension = extname(entryName);
			if (!isDirectory) {
				if (!isValidExtension(extension)) {
					return undefined
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

	const allScopes = [...scopes, builtInSymbols];
	let symbolFilter: (symbol: SymbolDefinition) => boolean | undefined;
	//#region infix function call (bei infix function reference)
	function getInfixFunctionCall(expression: PositionedExpression | undefined): ParseFunctionCall | undefined {
		if (!expression) {
			return undefined;
		}
		if (expression.type === 'functionCall'
			&& expression.prefixArgument) {
			return expression;
		}
		if (
			expression.type === 'reference'
			&& expression.parent?.type === 'functionCall'
			&& expression.parent.prefixArgument
			&& expression === expression.parent.functionExpression) {
			return expression.parent;
		}
		return undefined;
	}
	const infixFunctionCall = getInfixFunctionCall(expression);
	if (infixFunctionCall) {
		const prefixArgumentTypeRaw = infixFunctionCall.prefixArgument!.inferredType;
		let prefixArgumentType: RuntimeType | undefined;
		if (prefixArgumentTypeRaw instanceof ParameterReference) {
			const dereferenced = findSymbolInScopesWithBuiltIns(prefixArgumentTypeRaw.name, scopes);
			prefixArgumentType = dereferenced?.symbol.normalizedType;
		}
		else {
			prefixArgumentType = prefixArgumentTypeRaw;
		}

		symbolFilter = symbol => {
			if (prefixArgumentType === undefined) {
				return false;
			}
			const symbolType = symbol.normalizedType
			if (symbolType instanceof FunctionType) {
				const paramsType = symbolType.paramsType;
				if (paramsType instanceof ParametersType) {
					let firstParameterType: RuntimeType | undefined;
					if (paramsType.singleNames.length) {
						firstParameterType = paramsType.singleNames[0]?.type;
					}
					else if (paramsType.rest) {
						const restType = paramsType.rest?.type;
						if (restType instanceof ListType) {
							firstParameterType = restType.elementType;
						}
						else if (restType instanceof TupleType) {
							firstParameterType = restType.elementTypes[0];
						}
					}
					if (firstParameterType === undefined) {
						return false;
					}
					const typeError = getTypeError(undefined, prefixArgumentType, firstParameterType);
					return !typeError;
				}
			}
			return false;
		};

		return symbolsToCompletionItems(allScopes, symbolFilter);
	}
	//#endregion infix function call (bei infix function reference)

	//#region / field reference
	if (expression?.type === 'nestedReference') {
		const sourceType = expression.source.inferredType;
		const dereferenced = dereferenceInferredType(sourceType, scopes);
		if (!dereferenced || !(dereferenced instanceof BuiltInTypeBase)) {
			return [];
		}
		switch (dereferenced.type) {
			case 'dictionaryLiteral':
				return map(dereferenced.fields, (fieldValue, fieldName) => {
					const completionItem: CompletionItem = {
						label: fieldName,
						kind: CompletionItemKind.Constant,
						detail: typeToString(fieldValue, 0),
					};
					return completionItem;
				});
			default:
				return [];
		}
	}
	//#endregion / field reference

	return symbolsToCompletionItems(allScopes);
});

function symbolsToCompletionItems(
	allScopes: SymbolTable[],
	symbolFilter?: (symbol: SymbolDefinition) => boolean | undefined,
): CompletionItem[] {
	return allScopes.flatMap(symbols => {
		return map(
			symbols,
			(symbol, name) => {
				const showSymbol = symbolFilter?.(symbol) ?? true;
				if (!showSymbol) {
					return undefined;
				}
				const completionItem: CompletionItem = {
					label: name,
					kind: CompletionItemKind.Constant,
					detail: symbol.normalizedType === undefined
						? undefined
						: typeToString(symbol.normalizedType, 0),
					documentation: symbol.description,
				};
				return completionItem;
			}).filter(isDefined);
	});
}

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

//#region go to definition
const coreLibUri = pathToUri(coreLibPath);
connection.onDefinition((definitionParams) => {
	const documentUri = definitionParams.textDocument.uri;
	const documentPath = uriToPath(documentUri);
	const parsedFile = parsedDocuments[documentPath];
	if (!parsedFile) {
		return;
	}
	const rowIndex = definitionParams.position.line;
	const columnIndex = definitionParams.position.character;
	const { expression, scopes } = findExpressionInParsedFile(parsedFile, rowIndex, columnIndex);
	//#region go to imported file
	if (isImportPath(expression)) {
		const folderPath = dirname(documentPath);
		const { fullPath, error } = getPathFromImport(expression.parent!.parent as ParseFunctionCall, folderPath);
		if (error) {
			console.log(error);
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
	//#endregion

	const foundSymbol = getSymbolDefinition(expression, scopes);
	if (foundSymbol) {
		const location: Location = {
			uri: foundSymbol.isBuiltIn
				? coreLibUri
				: documentUri,
			range: positionedToRange(foundSymbol.symbol)
		};
		return location;
	}
});
//#endregion go to definition

//#region hover
connection.onHover((hoverParams) => {
	const parsed = getParsedFileByUri(hoverParams.textDocument.uri);
	if (!parsed) {
		return;
	}
	// TODO find functiontLiteral, show param + return type
	const { expression, scopes } = findExpressionInParsedFile(parsed, hoverParams.position.line, hoverParams.position.character);
	// // TODO functionCall, definition berücksichtigen?
	// if (expression?.type === 'reference') {
	// 	// hoverParams.position.
	// 	// TODO find expression by position row/column
	// 	// parsed.parsed
	// 	// TODO check for ref, provide Type information, doc comments
	// 	return {
	// 		contents: 'test: ' + expression.names[0],
	// 	};
	// }
	// return {
	// 	contents: 'expr type: ' + expression?.type,
	// };

	const foundSymbol = getSymbolDefinition(expression, scopes);
	if (foundSymbol) {
		const symbol = foundSymbol.symbol;
		return getHoverInfo(symbol.normalizedType, symbol.description);
	}

	if (expression?.type === 'name'
		&& expression.parent?.type === 'nestedReference') {
		// TODO stattdessen in getSymbolDefinition symbol aus dictionary field erzeugen?
		const dereferencedSource = dereferenceInferredType(expression.parent.source.inferredType, scopes);
		const dereferencedName = dereferenceNameFromObject(expression.name, dereferencedSource);
		return getHoverInfo(dereferencedName, undefined);
	}
});

function getHoverInfo(
	type: RuntimeType | undefined,
	description: string | undefined,
): Hover {
	console.log(type);
	const typeString = type === undefined
		? ''
		: `\`\`\`jul
${typeToString(type, 0)}
\`\`\`
`;
	return {
		contents: {
			kind: 'markdown',
			value: typeString + (description ?? ''),
		}
	};
}
//#endregion hover

//#region rename
connection.onPrepareRename(prepareRenameParams => {
	const documentUri = prepareRenameParams.textDocument.uri;
	const parsedFile = getParsedFileByUri(documentUri);
	if (!parsedFile) {
		return;
	}

	const { expression, scopes } = findExpressionInParsedFile(parsedFile, prepareRenameParams.position.line, prepareRenameParams.position.character);
	const foundSymbol = getSymbolDefinition(expression, scopes);
	if (!foundSymbol || foundSymbol.isBuiltIn) {
		return;
	}

	return positionedToRange(foundSymbol.expression);
});
connection.onRenameRequest(renameParams => {
	const documentUri = renameParams.textDocument.uri;
	const parsedFile = getParsedFileByUri(documentUri);
	if (!parsedFile) {
		return;
	}

	// TODO rename across multiple files
	const { expression, scopes } = findExpressionInParsedFile(parsedFile, renameParams.position.line, renameParams.position.character);
	const foundSymbol = getSymbolDefinition(expression, scopes);
	if (!foundSymbol || foundSymbol.isBuiltIn) {
		return;
	}
	const occurences = findAllOccurrencesInParsedFile(parsedFile, foundSymbol.expression);
	return {
		changes: {
			[documentUri]: occurences.map(expression => {
				return {
					range: positionedToRange(expression),
					newText: renameParams.newName,
				};
			})
		}
	};
});
//#endregion rename

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
 * TODO wenn nicht found in inner expression, dann return outer expression?
 */
function findExpressionInExpression(
	expression: PositionedExpression,
	rowIndex: number,
	columnIndex: number,
	scopes: SymbolTable[],
): PositionedExpression | undefined {
	switch (expression.type) {
		case 'bracketed': {
			const foundField = findExpressionInExpressions(expression.fields, rowIndex, columnIndex, scopes);
			return foundField;
		}
		case 'branching': {
			if (isPositionInRange(rowIndex, columnIndex, expression.value)) {
				const foundValue = findExpressionInExpression(expression.value, rowIndex, columnIndex, scopes);
				return foundValue;
			}
			const foundBranch = findExpressionInExpressions(expression.branches, rowIndex, columnIndex, scopes);
			return foundBranch;
		}
		case 'definition': {
			if (isPositionInRange(rowIndex, columnIndex, expression.name)) {
				return expression.name;
			}
			const typeGuard = expression.typeGuard;
			if (typeGuard && isPositionInRange(rowIndex, columnIndex, typeGuard)) {
				const foundType = findExpressionInExpression(typeGuard, rowIndex, columnIndex, scopes);
				return foundType;
			}
			const fallback = expression.fallback;
			if (fallback && isPositionInRange(rowIndex, columnIndex, fallback)) {
				const foundFallBack = findExpressionInExpression(fallback, rowIndex, columnIndex, scopes);
				return foundFallBack;
			}
			const foundValue = findExpressionInExpression(expression.value, rowIndex, columnIndex, scopes);
			return foundValue;
		}
		case 'destructuring': {
			if (isPositionInRange(rowIndex, columnIndex, expression.fields)) {
				const foundName = findExpressionInExpression(expression.fields, rowIndex, columnIndex, scopes);
				return foundName;
			}
			const foundValue = findExpressionInExpression(expression.value, rowIndex, columnIndex, scopes);
			return foundValue;
		}
		case 'dictionary': {
			const foundField = findExpressionInExpressions(expression.fields, rowIndex, columnIndex, scopes);
			return foundField;
		}
		case 'dictionaryType': {
			const foundField = findExpressionInExpressions(expression.fields, rowIndex, columnIndex, scopes);
			return foundField;
		}
		case 'empty':
			return undefined;
		case 'field': {
			if (isPositionInRange(rowIndex, columnIndex, expression.name)) {
				return expression.name;
			}
			const typeGuard = expression.typeGuard;
			if (typeGuard && isPositionInRange(rowIndex, columnIndex, typeGuard)) {
				const foundType = findExpressionInExpression(typeGuard, rowIndex, columnIndex, scopes);
				return foundType;
			}
			const assignedValue = expression.assignedValue;
			if (assignedValue && isPositionInRange(rowIndex, columnIndex, assignedValue)) {
				const foundAssignedValue = findExpressionInExpression(assignedValue, rowIndex, columnIndex, scopes);
				return foundAssignedValue;
			}
			const fallback = expression.fallback;
			if (fallback && isPositionInRange(rowIndex, columnIndex, fallback)) {
				const foundFallBack = findExpressionInExpression(fallback, rowIndex, columnIndex, scopes);
				return foundFallBack;
			}
			return expression;
		}
		case 'float':
			return undefined;
		case 'fraction':
			return undefined;
		case 'functionCall': {
			if (expression.prefixArgument && isPositionInRange(rowIndex, columnIndex, expression.prefixArgument)) {
				const foundPrefix = findExpressionInExpression(expression.prefixArgument, rowIndex, columnIndex, scopes);
				return foundPrefix;
			}
			if (expression.functionExpression && isPositionInRange(rowIndex, columnIndex, expression.functionExpression)) {
				const foundFunction = findExpressionInExpression(expression.functionExpression, rowIndex, columnIndex, scopes);
				return foundFunction;
			}
			if (expression.arguments) {
				const foundArguments = findExpressionInExpression(expression.arguments, rowIndex, columnIndex, scopes);
				return foundArguments;
			}
			return expression;
		}
		case 'functionLiteral': {
			scopes.push(expression.symbols);
			if (isPositionInRange(rowIndex, columnIndex, expression.params)) {
				const foundParams = findExpressionInExpression(expression.params, rowIndex, columnIndex, scopes);
				return foundParams;
			}
			const returnType = expression.returnType;
			if (returnType && isPositionInRange(rowIndex, columnIndex, returnType)) {
				const foundReturnType = findExpressionInExpression(returnType, rowIndex, columnIndex, scopes);
				return foundReturnType;
			}
			const foundBody = findExpressionInExpressions(expression.body, rowIndex, columnIndex, scopes);
			return foundBody;
		}
		case 'functionTypeLiteral': {
			scopes.push(expression.symbols);
			if (isPositionInRange(rowIndex, columnIndex, expression.params)) {
				const foundParams = findExpressionInExpression(expression.params, rowIndex, columnIndex, scopes);
				return foundParams;
			}
			const returnType = expression.returnType;
			if (isPositionInRange(rowIndex, columnIndex, returnType)) {
				const foundParams = findExpressionInExpression(returnType, rowIndex, columnIndex, scopes);
				return foundParams;
			}
			return undefined;
		}
		case 'index':
			return expression;
		case 'integer':
			return undefined;
		case 'list': {
			const foundValue = findExpressionInExpressions(expression.values, rowIndex, columnIndex, scopes);
			return foundValue;
		}
		case 'name':
			return expression;
		case 'nestedReference': {
			const nestedKey = expression.nestedKey;
			if (nestedKey && isPositionInRange(rowIndex, columnIndex, nestedKey)) {
				return nestedKey;
			}
			const source = expression.source;
			if (isPositionInRange(rowIndex, columnIndex, source)) {
				const foundSource = findExpressionInExpression(source, rowIndex, columnIndex, scopes);
				return foundSource;
			}
			return expression;
		}
		case 'object': {
			const foundValue = findExpressionInExpressions(expression.values, rowIndex, columnIndex, scopes);
			return foundValue;
		}
		case 'parameter': {
			const name = expression.name;
			if (isPositionInRange(rowIndex, columnIndex, name)) {
				return name;
			}
			const typeGuard = expression.typeGuard;
			if (typeGuard && isPositionInRange(rowIndex, columnIndex, typeGuard)) {
				const foundType = findExpressionInExpression(typeGuard, rowIndex, columnIndex, scopes);
				return foundType;
			}
			const fallback = expression.fallback;
			const foundValue = fallback && findExpressionInExpression(fallback, rowIndex, columnIndex, scopes);
			return foundValue;
		}
		case 'parameters': {
			const foundField = findExpressionInExpressions(expression.singleFields, rowIndex, columnIndex, scopes);
			if (foundField) {
				return foundField;
			}
			const rest = expression.rest;
			if (rest && isPositionInRange(rowIndex, columnIndex, rest)) {
				const foundRest = findExpressionInExpression(rest, rowIndex, columnIndex, scopes);
				return foundRest;
			}
			return undefined;
		}
		case 'reference':
			return expression;
		case 'singleDictionaryField': {
			const name = expression.name;
			if (isPositionInRange(rowIndex, columnIndex, name)) {
				return name;
			}
			const typeGuard = expression.typeGuard;
			if (typeGuard && isPositionInRange(rowIndex, columnIndex, typeGuard)) {
				const foundType = findExpressionInExpression(typeGuard, rowIndex, columnIndex, scopes);
				return foundType;
			}
			const fallback = expression.fallback;
			if (fallback && isPositionInRange(rowIndex, columnIndex, fallback)) {
				const foundFallBack = findExpressionInExpression(fallback, rowIndex, columnIndex, scopes);
				return foundFallBack;
			}
			const foundValue = findExpressionInExpression(expression.value, rowIndex, columnIndex, scopes);
			return foundValue;
		}
		case 'singleDictionaryTypeField': {
			const name = expression.name;
			if (isPositionInRange(rowIndex, columnIndex, name)) {
				return name;
			}
			const typeGuard = expression.typeGuard;
			if (typeGuard && isPositionInRange(rowIndex, columnIndex, typeGuard)) {
				const foundType = findExpressionInExpression(typeGuard, rowIndex, columnIndex, scopes);
				return foundType;
			}
			return expression;
		}
		case 'spread': {
			if (isPositionInRange(rowIndex, columnIndex, expression.value)) {
				const foundValue = findExpressionInExpression(expression.value, rowIndex, columnIndex, scopes);
				return foundValue;
			}
			return expression;
		}
		case 'text': {
			const values = expression.values.filter((value): value is ParseValueExpression =>
				value.type !== 'textToken');
			const foundValue = findExpressionInExpressions(values, rowIndex, columnIndex, scopes);
			return foundValue ?? expression;
		}
		default: {
			const assertNever: never = expression;
			throw new Error(`Unexpected expression.type: ${(assertNever as PositionedExpression).type}`);
		}
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

//#region findAllOccurrences

// TODO scope berücksichtigen
function findAllOccurrencesInParsedFile(
	parsedFile: ParsedFile,
	searchTerm: Reference | Name,
): PositionedExpression[] {
	const expressions = parsedFile.checked?.expressions;
	return expressions
		? findAllOccurrencesInExpressions(expressions, searchTerm)
		: [];
}

function findAllOccurrencesInExpressions(
	expressions: PositionedExpression[],
	searchTerm: Reference | Name,
): PositionedExpression[] {
	return expressions.flatMap(expression => {
		return findAllOccurrencesInExpression(expression, searchTerm);
	});
}

function findAllOccurrencesInExpression(
	expression: PositionedExpression | undefined,
	searchTerm: Reference | Name,
): PositionedExpression[] {
	if (!expression) {
		return [];
	}
	switch (expression.type) {
		case 'bracketed':
			// return findAllOccurrencesInExpression(expression.fields, name);
			return [];
		case 'branching': {
			const occurences = [
				...findAllOccurrencesInExpression(expression.value, searchTerm),
				...findAllOccurrencesInExpressions(expression.branches, searchTerm),
			];
			return occurences;
		}
		case 'definition': {
			const occurences = [
				...findAllOccurrencesInExpression(expression.name, searchTerm),
				...findAllOccurrencesInExpression(expression.typeGuard, searchTerm),
				...findAllOccurrencesInExpression(expression.value, searchTerm),
				...findAllOccurrencesInExpression(expression.fallback, searchTerm),
			];
			return occurences;
		}
		case 'destructuring': {
			const occurences = [
				// TODO
				// ...findAllOccurrencesInExpressions(expression.fields, searchTerm),
				...findAllOccurrencesInExpression(expression.value, searchTerm),
			];
			return occurences;
		}
		case 'dictionary': {
			const occurences = findAllOccurrencesInExpressions(expression.fields, searchTerm);
			return occurences;
		}
		case 'dictionaryType': {
			const occurences = findAllOccurrencesInExpressions(expression.fields, searchTerm);
			return occurences;
		}
		case 'empty':
			return [];
		case 'field':
			// TODO check name range, source, typeGuard, fallback
			// return expression;
			return [];
		case 'float':
			return [];
		case 'fraction':
			return [];
		case 'functionCall': {
			const occurences = [
				...findAllOccurrencesInExpression(expression.functionExpression, searchTerm),
				...findAllOccurrencesInExpression(expression.arguments, searchTerm),
			];
			return occurences;
		}
		case 'functionLiteral': {
			const occurences = [
				...findAllOccurrencesInExpression(expression.params, searchTerm),
				...findAllOccurrencesInExpression(expression.returnType, searchTerm),
				...findAllOccurrencesInExpressions(expression.body, searchTerm),
			];
			return occurences;
		}
		case 'functionTypeLiteral': {
			const occurences = [
				...findAllOccurrencesInExpression(expression.params, searchTerm),
				...findAllOccurrencesInExpression(expression.returnType, searchTerm),
			];
			return occurences;
		}
		case 'index':
			return [];
		case 'integer':
			return [];
		case 'list':
			return findAllOccurrencesInExpressions(expression.values, searchTerm);
		case 'name':
			return expression.name === getSearchName(searchTerm)
				? [expression]
				: [];
		case 'nestedReference': {
			const occurences = [
				...findAllOccurrencesInExpression(expression.source, searchTerm),
			];
			const nestedKey = expression.nestedKey;
			if (nestedKey) {
				occurences.push(...findAllOccurrencesInExpression(nestedKey, searchTerm))
			}
			return occurences;
		}
		case 'object':
			return findAllOccurrencesInExpressions(expression.values, searchTerm);
		case 'parameter': {
			const occurences = [
				...findAllOccurrencesInExpression(expression.name, searchTerm),
				...findAllOccurrencesInExpression(expression.typeGuard, searchTerm),
				...findAllOccurrencesInExpression(expression.fallback, searchTerm),
			];
			return occurences;
		}
		case 'parameters': {
			const occurences = [
				...findAllOccurrencesInExpressions(expression.singleFields, searchTerm),
				...findAllOccurrencesInExpression(expression.rest, searchTerm),
			];
			return occurences;
		}
		case 'reference': {
			const referenceName = expression.name;
			return referenceName.name === getSearchName(searchTerm)
				? [referenceName]
				: [];
		}
		case 'singleDictionaryField': {
			const occurences = [
				...findAllOccurrencesInExpression(expression.name, searchTerm),
				...findAllOccurrencesInExpression(expression.typeGuard, searchTerm),
				...findAllOccurrencesInExpression(expression.value, searchTerm),
				...findAllOccurrencesInExpression(expression.fallback, searchTerm),
			];
			return occurences;
		}
		case 'singleDictionaryTypeField': {
			const occurences = [
				...findAllOccurrencesInExpression(expression.name, searchTerm),
				...findAllOccurrencesInExpression(expression.typeGuard, searchTerm),
			];
			return occurences;
		}
		case 'spread': {
			const occurences = findAllOccurrencesInExpression(expression.value, searchTerm);
			return occurences;
		}
		case 'text': {
			const values = expression.values.filter((value): value is ParseValueExpression =>
				value.type !== 'textToken');
			return findAllOccurrencesInExpressions(values, searchTerm);
		}
		default: {
			const assertNever: never = expression;
			throw new Error(`Unexpected expression.type: ${(assertNever as PositionedExpression).type}`);
		}
	}
}

function getSearchName(searchTerm: Reference | Name): string {
	switch (searchTerm.type) {
		case 'name':
			return searchTerm.name;
		case 'reference':
			return searchTerm.name.name;
		default: {
			const assertNever: never = searchTerm;
			throw new Error(`Unexpected searchTerm.type: ${(assertNever as Reference | Name).type}`);
		}
	}
}

//#endregion findAllOccurrences

// TODO go to source file bei import?
function getSymbolDefinition(
	expression: PositionedExpression | undefined,
	scopes: SymbolTable[],
): {
	isBuiltIn: boolean;
	symbol: SymbolDefinition;
	/**
	 * Die expression, die an der Position (rowIndex, columnIndex) gefunden wurde.
	 */
	expression: Reference | Name;
} | undefined {
	if (!expression) {
		return undefined;
	}
	switch (expression.type) {
		case 'reference': {
			const definition = findSymbolInScopesWithBuiltIns(expression.name.name, scopes);
			return definition && {
				...definition,
				expression: expression,
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
			switch (parent?.type) {
				case 'nestedReference': {
					// TODO find type definition, get symbol from dictionaryTypeLiteral
					// const dereferencedSource = dereferenceInferredType(parent.source.inferredType, scopes);
					// const dereferencedName = dereferenceNameFromObject(expression.name, dereferencedSource);
					return undefined;
				}
				case 'singleDictionaryField':
					// TODO
					return undefined;
				case 'singleDictionaryTypeField':
					// TODO
					return undefined;
				default: {
					const definition = findSymbolInScopesWithBuiltIns(expression.name, scopes);
					return definition && {
						...definition,
						expression: expression,
					};
				}
			}
		}
		case 'bracketed':
		case 'branching':
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

function isImportPath(expression: PositionedExpression | undefined): expression is ParseTextLiteral {
	if (expression
		&& expression.type === 'text'
		&& expression.parent?.type === 'list'
		&& expression.parent.parent
		&& isImportFunctionCall(expression.parent.parent)) {
		return true;
	}
	return false;
}

function positionedToRange(positioned: Positioned): Range {
	return {
		start: {
			line: positioned.startRowIndex,
			character: positioned.startColumnIndex,
		},
		end: {
			line: positioned.endRowIndex,
			character: positioned.endColumnIndex,
		},
	};
}

//#region uri

function pathToUri(path: string): string {
	return URI.file(path).toString()
}

function uriToPath(uri: string): string {
	return URI.parse(uri).fsPath;
}

function getParsedFileByUri(uri: string): ParsedFile | undefined {
	const path = uriToPath(uri);
	const parsed = parsedDocuments[path];
	return parsed;
}

//#endregion uri

//#endregion helper