import { readdirSync } from 'fs';
import { dirname, extname, join } from 'path';
import {
	LanguageService,
	getLanguageService as getHtmlLanguageService,
} from 'vscode-html-languageservice';
import {
	CompletionItem,
	CompletionItemKind,
	createConnection,
	Diagnostic,
	DiagnosticSeverity,
	DocumentSymbol,
	InitializeParams,
	InitializeResult,
	Location,
	MarkupContent,
	ParameterInformation,
	ProposedFeatures,
	Range,
	SignatureHelp,
	SymbolKind,
	TextDocuments,
	TextDocumentSyncKind,
} from 'vscode-languageserver';
import {
	TextDocument
} from 'vscode-languageserver-textdocument';
import { URI } from 'vscode-uri';
import {
	coreLibPath,
	getPathFromImport,
	isImportFunctionCall,
	parseCode
} from 'jul-compiler/out/parser/parser.js';
import { getCheckedEscapableName } from 'jul-compiler/out/parser/parser-utils.js';
import { Positioned } from 'jul-compiler/out/parser/parser-combinator.js';
import {
	CompileTimeDictionary,
	CompileTimeType,
	DefinitionExpression,
	PositionedExpression,
	Parameter,
	ParseDestructuringField,
	ParseDestructuringFields,
	ParsedFile,
	ParseFunctionCall,
	ParseValueExpression,
	SymbolDefinition,
	SymbolTable,
	TypeInfo,
} from 'jul-compiler/out/syntax-tree.js';
import {
	builtInSymbols,
	checkTypes,
	dereferenceIndexFromObject,
	dereferenceNameFromObject,
	findSymbolInScopesWithBuiltIns,
	getStreamGetValueType,
	getTypeError,
	isBuiltInType,
	isDictionaryLiteralType,
	isFunctionType,
	isListType,
	isParameterReference,
	isParametersType,
	isReferenceType,
	isTupleType,
	isTypeOfType,
	isUnionType,
	ParsedDocuments,
	typeToString,
} from 'jul-compiler/out/checker.js';
import { isDefined, isValidExtension, map, tryReadTextFile } from 'jul-compiler/out/util.js';
import { _julTypeSymbol } from 'jul-compiler/out/runtime.js';

// For performance do not process large files
const maxFileSize = 100000;

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
			// Tell the client that this server supports code completion.
			completionProvider: {
				resolveProvider: true,
				triggerCharacters: ['.', '/'],
			},
			definitionProvider: true,
			documentSymbolProvider: true,
			hoverProvider: true,
			renameProvider: {
				prepareProvider: true,
			},
			signatureHelpProvider: {
				triggerCharacters: ['(', ' ', '\n'],
			},
			textDocumentSync: TextDocumentSyncKind.Incremental,
		},
	};
	return result;
});

//#region diagnostics
// This event is emitted when the text document first opened or when its content has changed.
// parse document, fill parsedDocuments and sendDiagnostics
documents.onDidChangeContent(change => {
	const textDocument = change.document;
	const uri = textDocument.uri;
	const text = textDocument.getText();
	if (text.length > maxFileSize) {
		return;
	}
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
});

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
	if (code.length > maxFileSize) {
		return;
	}
	parseDocumentByCode(code, path);
}
//#endregion diagnostics

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
		const declaredType = getDeclaredType(expression)?.dereferencedType;
		switch (typeof declaredType) {
			case 'string':
				return [stringToCompletionItem(declaredType)];
			case 'object':
				if (isUnionType(declaredType)) {
					return declaredType.ChoiceTypes
						.filter((choiceType): choiceType is string =>
							typeof choiceType === 'string')
						.map(stringToCompletionItem);
				}
			default:
				break;
		}
		//#endregion Text literal with declared type
	}

	const allScopes = [...scopes, builtInSymbols];
	let symbolFilter: ((symbol: SymbolDefinition, name: string) => boolean) | undefined = undefined;
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
		const prefixArgumentTypeRaw = infixFunctionCall.prefixArgument!.typeInfo!.rawType;
		let prefixArgumentType: CompileTimeType | null;
		if (isParameterReference(prefixArgumentTypeRaw)) {
			const dereferenced = findSymbolInScopesWithBuiltIns(prefixArgumentTypeRaw.name, scopes);
			prefixArgumentType = dereferenced?.symbol.typeInfo?.rawType;
		}
		else {
			prefixArgumentType = prefixArgumentTypeRaw;
		}

		symbolFilter = symbol => {
			if (prefixArgumentType === null) {
				return false;
			}
			const symbolType = symbol.typeInfo?.dereferencedType;
			if (isFunctionType(symbolType)) {
				const paramsType = symbolType.ParamsType;
				if (isParametersType(paramsType)) {
					let firstParameterType: CompileTimeType | null;
					if (paramsType.singleNames.length) {
						firstParameterType = paramsType.singleNames[0]?.type;
					}
					else if (paramsType.rest) {
						const restType = paramsType.rest?.type;
						if (isListType(restType)) {
							firstParameterType = restType.ElementType;
						}
						else if (isTupleType(restType)) {
							firstParameterType = restType.ElementTypes[0];
						}
					}
					if (firstParameterType === null) {
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
		const dereferencedType = expression.source?.typeInfo?.dereferencedType;
		return getNestedReferenceCompletionItems(dereferencedType);
	}
	//#endregion / field reference

	//#region dictionary literal field
	if (expression?.type === 'empty'
		|| expression?.type === 'dictionary'
		|| expression?.type === 'object') {
		const declaredType = getDeclaredType(expression)?.dereferencedType;
		let allCompletionItems: CompletionItem[] | undefined;
		if (isDictionaryLiteralType(declaredType)) {
			allCompletionItems = dictionaryTypeToCompletionItems(declaredType.Fields);
		}
		if (isUnionType(declaredType)) {
			allCompletionItems = [];
			declaredType.ChoiceTypes.forEach(choiceType => {
				if (isDictionaryLiteralType(choiceType)) {
					const completionItems = dictionaryTypeToCompletionItems(choiceType.Fields);
					completionItems.forEach(newCompletionItem => {
						// Duplikate vermeiden
						if (!allCompletionItems?.some(existingCompletionItem => existingCompletionItem.label === newCompletionItem.label)) {
							allCompletionItems?.push(newCompletionItem);
						}
					});
				}
			});
		}
		//#region function call arg
		if (isParametersType(declaredType)) {
			allCompletionItems = declaredType.singleNames.map((singleName, index) => {
				return parameterToCompletionItem(singleName, index, false);
			});
		}
		//#endregion function call arg
		if (allCompletionItems) {
			// schon definierte Felder ausschließen
			if (expression.type === 'dictionary') {
				const filtered = allCompletionItems.filter(completionItem => {
					return !expression.symbols[completionItem.label];
				});
				return filtered;
			}
			return allCompletionItems;
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
					return symbolsToCompletionItems([destructuredValue.symbols], symbolFilter);
				default: {
					const dereferencedType = destructuredValue?.typeInfo?.dereferencedType;
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
		const innerParamsType = getDeclaredType(expression)?.dereferencedType;
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

	return symbolsToCompletionItems(allScopes);
});

//#region create CompletionItems

function parameterToCompletionItem(parameter: Parameter, index: number, isRest: boolean): CompletionItem {
	const completionItem: CompletionItem = {
		label: (isRest ? '...' : '') + parameter.name,
		kind: CompletionItemKind.Constant,
		detail: parameter.type === null
			? undefined
			: typeToString(parameter.type, 0),
		// documentation: parameter.description,
		sortText: '' + index,
	};
	return completionItem;
}

function getNestedReferenceCompletionItems(dereferencedType: CompileTimeType): CompletionItem[] {
	if (isBuiltInType(dereferencedType)) {
		switch (dereferencedType[_julTypeSymbol]) {
			case 'dictionaryLiteral':
				return dictionaryTypeToCompletionItems(dereferencedType.Fields);
			case 'function':
				return functionTypeToCompletionItems(dereferencedType);
			case 'reference':
				return getNestedReferenceCompletionItems(dereferencedType.dereferencedType);
			case 'stream':
				return [
					{
						label: 'getValue',
						kind: CompletionItemKind.Function,
						detail: typeToString(getStreamGetValueType(dereferencedType), 0),
					},
					// TODO? nur bei TypeOf(Stream)
					{
						label: 'ValueType',
						kind: CompletionItemKind.Constant,
						detail: typeToString(dereferencedType.ValueType, 0),
					},
				];
			default:
				return [];
		}
	}
	return [];
}

function functionTypeToCompletionItems(functionType: CompileTimeType | null): CompletionItem[] {
	// TODO ParamsType, ReturnType stattdessen als symbols?
	let paramsType: CompileTimeType | null;
	let returnType: CompileTimeType | null;
	if (isFunctionType(functionType)) {
		returnType = functionType.ReturnType;
		paramsType = functionType.ParamsType;
	}
	return [
		{
			label: 'ParamsType',
			kind: CompletionItemKind.Constant,
			detail: paramsType === null
				? undefined
				: typeToString(paramsType, 0),
		},
		{
			label: 'ReturnType',
			kind: CompletionItemKind.Constant,
			detail: returnType === null
				? undefined
				: typeToString(returnType, 0),
		},
	];
}

function stringToCompletionItem(value: string): CompletionItem {
	const completionItem: CompletionItem = {
		label: value,
		kind: CompletionItemKind.Constant,
		// kind: CompletionItemKind.Text,
		// detail: typeToString(value, 0),
		// documentation: symbol.description,
	};
	return completionItem;
}

function dictionaryTypeToCompletionItems(
	fields: CompileTimeDictionary,
): CompletionItem[] {
	return map(
		fields,
		(type, name) => {
			const completionItem: CompletionItem = {
				label: name,
				kind: CompletionItemKind.Constant,
				detail: typeToString(type, 0),
				// documentation: symbol.description,
			};
			return completionItem;
		});
}

function symbolsToCompletionItems(
	scopes: SymbolTable[],
	symbolFilter?: (symbol: SymbolDefinition, name: string) => boolean,
): CompletionItem[] {
	return scopes.flatMap(symbols => {
		return map(
			symbols,
			(symbol, name) => {
				const showSymbol = symbolFilter?.(symbol, name) ?? true;
				if (!showSymbol) {
					return undefined;
				}
				const symbolType = symbol.typeInfo;
				const isFunction = isFunctionType(symbolType?.dereferencedType);
				const completionItem: CompletionItem = {
					label: name,
					kind: isFunction
						? CompletionItemKind.Function
						: CompletionItemKind.Constant,
					detail: symbolType && typeToString(symbolType.rawType, 0),
					documentation: symbol.description,
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
			const normalizedFunctionType = functionSymbol.symbol.typeInfo;
			const signatureResult: SignatureHelp = {
				signatures: [{
					label: normalizedFunctionType
						? typeToString(normalizedFunctionType.rawType, 0)
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

//#region go to definition
const coreLibUri = pathToUri(coreLibPath);
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
	//#endregion go to imported file

	const foundSymbol = getSymbolDefinition(expression, scopes, folderPath);
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
		return {
			contents: getTypeMarkdown(symbol.typeInfo, symbol.description),
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
connection.onRenameRequest(renameParams => {
	const documentUri = renameParams.textDocument.uri;
	const parsedFile = getParsedFileByUri(documentUri);
	if (!parsedFile) {
		return;
	}

	// TODO rename across multiple files
	const { expression, scopes } = findExpressionInParsedFile(parsedFile, renameParams.position.line, renameParams.position.character);
	if (!expression) {
		return;
	}
	const documentPath = uriToPath(documentUri);
	const folderPath = dirname(documentPath);
	const foundSymbol = getSymbolDefinition(expression, scopes, folderPath);
	if (!foundSymbol || foundSymbol.isBuiltIn || !foundSymbol.symbol.definition) {
		return;
	}
	const searchTerm = foundSymbol.name;
	const occurences = findAllOccurrencesInParsedFile(parsedFile, foundSymbol.symbol.definition, searchTerm);
	return {
		changes: {
			[documentUri]: occurences.map(innerExpression => {
				return {
					range: positionedToRange(innerExpression),
					newText: renameParams.newName,
				};
			})
		}
	};
});
//#endregion rename

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
			return [
				...getDocumentSymbolsFromExpression(expression.value),
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
			const children = [
				...(expression.typeGuard
					? getDocumentSymbolsFromExpression(expression.typeGuard)
					: []),
				...(expression.value
					? getDocumentSymbolsFromExpression(expression.value)
					: []),
			];
			return createDocumentSymbol(expression, expression.name, SymbolKind.Field, children);
		}
		case 'singleDictionaryTypeField': {
			const children = expression.typeGuard && getDocumentSymbolsFromExpression(expression.typeGuard);
			return createDocumentSymbol(expression, expression.name, SymbolKind.Field, children);
		}
		case 'bracketed':
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
	switch (expression.type) {
		case 'bracketed': {
			const foundField = findExpressionInExpressions(expression.fields, rowIndex, columnIndex, scopes);
			return foundField ?? expression;
		}
		case 'branching': {
			if (isPositionInRange(rowIndex, columnIndex, expression.value)) {
				const foundValue = findExpressionInExpression(expression.value, rowIndex, columnIndex, scopes);
				return foundValue;
			}
			const foundBranch = findExpressionInExpressions(expression.branches, rowIndex, columnIndex, scopes);
			return foundBranch ?? expression;
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
			const value = expression.value;
			if (value && isPositionInRange(rowIndex, columnIndex, value)) {
				const foundValue = findExpressionInExpression(value, rowIndex, columnIndex, scopes);
				return foundValue;
			}
			return expression;
		}
		case 'destructuring': {
			if (isPositionInRange(rowIndex, columnIndex, expression.fields)) {
				const foundName = findExpressionInExpression(expression.fields, rowIndex, columnIndex, scopes);
				return foundName;
			}
			const value = expression.value;
			if (value && isPositionInRange(rowIndex, columnIndex, value)) {
				const foundValue = findExpressionInExpression(value, rowIndex, columnIndex, scopes);
				return foundValue;
			}
			return expression;
		}
		case 'destructuringField': {
			if (isPositionInRange(rowIndex, columnIndex, expression.name)) {
				return expression.name;
			}
			const typeGuard = expression.typeGuard;
			if (typeGuard && isPositionInRange(rowIndex, columnIndex, typeGuard)) {
				const foundType = findExpressionInExpression(typeGuard, rowIndex, columnIndex, scopes);
				return foundType;
			}
			const source = expression.source;
			if (source && isPositionInRange(rowIndex, columnIndex, source)) {
				return source;
			}
			return expression;
		}
		case 'destructuringFields':
		case 'dictionary':
		case 'dictionaryType': {
			const foundField = findExpressionInExpressions(expression.fields, rowIndex, columnIndex, scopes);
			return foundField ?? expression;
		}
		case 'empty':
			return expression;
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
			return expression;
		}
		case 'float':
			return expression;
		case 'fraction':
			return expression;
		case 'functionCall': {
			if (expression.prefixArgument && isPositionInRange(rowIndex, columnIndex, expression.prefixArgument)) {
				const foundPrefix = findExpressionInExpression(expression.prefixArgument, rowIndex, columnIndex, scopes);
				return foundPrefix;
			}
			const functionExpression = expression.functionExpression;
			if (functionExpression && isPositionInRange(rowIndex, columnIndex, functionExpression)) {
				const foundFunction = findExpressionInExpression(functionExpression, rowIndex, columnIndex, scopes);
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
			return foundBody ?? expression;
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
			return expression;
		}
		case 'index':
			return expression;
		case 'integer':
			return expression;
		case 'list': {
			const foundValue = findExpressionInExpressions(expression.values, rowIndex, columnIndex, scopes);
			return foundValue ?? expression;
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
			return foundValue ?? expression;
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
			return expression;
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
			return expression;
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
			const value = expression.value;
			if (value && isPositionInRange(rowIndex, columnIndex, value)) {
				const foundValue = findExpressionInExpression(value, rowIndex, columnIndex, scopes);
				return foundValue;
			}
			return expression;
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

function findAllOccurrencesInParsedFile(
	parsedFile: ParsedFile,
	definition: DefinitionExpression,
	searchTerm: string,
): PositionedExpression[] {
	const expressions = parsedFile.checked?.expressions;
	if (!expressions) {
		return [];
	}
	const parent = definition.parent;
	if (!parent) {
		// definition im root scope: alles umbenennen
		return findAllOccurrencesInExpressions(expressions, searchTerm);
	}
	switch (parent.type) {
		case 'dictionary':
			// TODO rename field
			return [];
		case 'functionLiteral':
			return findAllOccurrencesInExpressions(parent.body, searchTerm);
		// TODO other types?
		default:
			return [];
	}
}

function findAllOccurrencesInExpressions(
	expressions: PositionedExpression[],
	searchTerm: string,
): PositionedExpression[] {
	return expressions.flatMap(expression => {
		return findAllOccurrencesInExpression(expression, searchTerm);
	});
}

function findAllOccurrencesInExpression(
	expression: PositionedExpression | undefined,
	searchTerm: string,
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
			];
			return occurences;
		}
		case 'destructuring': {
			const occurences = [
				...findAllOccurrencesInExpression(expression.fields, searchTerm),
				...findAllOccurrencesInExpression(expression.value, searchTerm),
			];
			return occurences;
		}
		case 'destructuringField': {
			const occurences = [
				...findAllOccurrencesInExpression(expression.name, searchTerm),
				...findAllOccurrencesInExpression(expression.typeGuard, searchTerm),
				...findAllOccurrencesInExpression(expression.source, searchTerm),
			];
			return occurences;
		}
		case 'destructuringFields':
		case 'dictionary':
		case 'dictionaryType': {
			const occurences = findAllOccurrencesInExpressions(expression.fields, searchTerm);
			return occurences;
		}
		case 'empty':
			return [];
		case 'field':
			// TODO check name range, source, typeGuard
			// return expression;
			return [];
		case 'float':
			return [];
		case 'fraction':
			return [];
		case 'functionCall': {
			const occurences = [
				...findAllOccurrencesInExpression(expression.prefixArgument, searchTerm),
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
			return expression.name === searchTerm
				? [expression]
				: [];
		case 'nestedReference': {
			const occurences = [
				...findAllOccurrencesInExpression(expression.source, searchTerm),
			];
			// TODO rename field
			// const nestedKey = expression.nestedKey;
			// if (nestedKey) {
			// 	occurences.push(...findAllOccurrencesInExpression(nestedKey, searchTerm));
			// }
			return occurences;
		}
		case 'object':
			return findAllOccurrencesInExpressions(expression.values, searchTerm);
		case 'parameter': {
			const occurences = [
				...findAllOccurrencesInExpression(expression.name, searchTerm),
				...findAllOccurrencesInExpression(expression.typeGuard, searchTerm),
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
			return referenceName.name === searchTerm
				? [referenceName]
				: [];
		}
		case 'singleDictionaryField': {
			const occurences = [
				// TODO rename field
				// ...findAllOccurrencesInExpression(expression.name, searchTerm),
				...findAllOccurrencesInExpression(expression.typeGuard, searchTerm),
				...findAllOccurrencesInExpression(expression.value, searchTerm),
			];
			return occurences;
		}
		case 'singleDictionaryTypeField': {
			const occurences = [
				// TODO rename field
				// ...findAllOccurrencesInExpression(expression.name, searchTerm),
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

//#endregion findAllOccurrences

//#region get Symbol

function getSymbolDefinition(
	expression: PositionedExpression,
	scopes: SymbolTable[],
	folderPath: string,
): {
	isBuiltIn: boolean;
	symbol: SymbolDefinition;
	name: string;
	/**
	 * undefined, wenn Symbol in gleicher Datei gefunden
	 */
	filePath?: string;
} | undefined {
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
					return importedSymbol?.symbol && {
						name: name,
						isBuiltIn: false,
						symbol: importedSymbol.symbol,
						filePath: importedSymbol.filePath,
					};
				}
				case 'nestedReference': {
					const declaredSourceType = getDeclaredType(parent.source);
					const sourceType = declaredSourceType?.dereferencedType ?? parent.source.typeInfo?.dereferencedType;
					if (isDictionaryLiteralType(sourceType)) {
						const foundSymbol2 = sourceType.expression?.symbols[name];
						return foundSymbol2 && {
							name: name,
							isBuiltIn: false,
							symbol: foundSymbol2,
							// filePath: foundSymbol.filePath,
						};
					}
					const dereferencedSource = dereferenceTypeExpression(parent.source, scopes, folderPath);
					if (!dereferencedSource) {
						return undefined;
					}
					const dereferencedFolderPath = dereferencedSource.filePath
						? dirname(dereferencedSource.filePath)
						: folderPath;
					const foundSymbol = getSymbolFromDictionary(dereferencedSource.typeExpression, name, scopes, dereferencedFolderPath);
					return foundSymbol && {
						name: name,
						isBuiltIn: false,
						symbol: foundSymbol.symbol,
						filePath: foundSymbol.filePath ?? dereferencedSource.filePath,
					};
				}
				case 'singleDictionaryField':
				case 'singleDictionaryTypeField': {
					const declaredParentType = getDeclaredType(parent.parent!);
					const deref = isReferenceType(declaredParentType?.dereferencedType)
						? declaredParentType?.dereferencedType.dereferencedType
						: declaredParentType?.dereferencedType;
					if (isDictionaryLiteralType(deref)) {
						const foundSymbol2 = deref.expression?.symbols[name];
						return foundSymbol2 && {
							name: name,
							isBuiltIn: false,
							symbol: foundSymbol2,
							// filePath: foundSymbol.filePath,
						};
					}
					const foundSymbol = getSymbolFromDictionary(parent.parent, name, scopes, folderPath);
					return foundSymbol && {
						name: name,
						isBuiltIn: false,
						symbol: foundSymbol.symbol,
						filePath: foundSymbol.filePath,
					};
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
		case 'bracketed':
		case 'branching':
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

function getSymbolFromDictionary(
	dictionary: PositionedExpression | undefined,
	name: string,
	scopes: SymbolTable[],
	folderPath: string,
): {
	symbol: SymbolDefinition;
	/**
	 * undefined, wenn Symbol in gleicher Datei gefunden
	 */
	filePath?: string;
} | undefined {
	switch (dictionary?.type) {
		case 'dictionaryType':
		case 'dictionary': {
			// wenn möglich Symbol des Felds im deklarierten DictionaryLiteralTyp liefern
			const definition = dictionary.parent;
			if (definition?.type === 'definition'
				&& definition.value === dictionary
				&& definition.typeGuard) {
				const typeSymbol = getSymbolDefinition(definition.typeGuard, scopes, folderPath);
				const typeDefinition = typeSymbol?.symbol.definition;
				if (typeDefinition?.type === 'destructuringField') {
					const importedSymbol = getImportedSymbol(typeDefinition, folderPath);
					const typeDefinition2 = importedSymbol?.symbol?.definition;
					const fieldSymbol = getFieldSymbol(typeDefinition2, name);
					return fieldSymbol && {
						symbol: fieldSymbol,
						filePath: importedSymbol?.filePath,
					};
				}
				else {
					const fieldSymbol = getFieldSymbol(typeDefinition, name);
					return fieldSymbol && { symbol: fieldSymbol };
				}
			}
			const fieldSymbol = dictionary.symbols[name];
			return fieldSymbol && { symbol: fieldSymbol };
		}
		default:
			return undefined;
	}
}

function getFieldSymbol(
	typeDefinition: DefinitionExpression | undefined,
	name: string,
): SymbolDefinition | undefined {
	if (typeDefinition?.type === 'definition'
		&& typeDefinition.value?.type === 'dictionaryType') {
		const fieldSymbol = typeDefinition.value?.symbols[name];
		return fieldSymbol;
	}
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
				console.log(error);
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
					symbol: importedSymbol,
					filePath: fullPath,
				};
			}
		}
	}
}

//#endregion get Symbol

function dereferenceTypeExpression(
	sourceExpression: PositionedExpression,
	scopes: SymbolTable[],
	folderPath: string,
): {
	typeExpression: PositionedExpression;
	/**
	 * undefined, wenn Symbol in gleicher Datei gefunden
	 */
	filePath?: string;
} | undefined {
	switch (sourceExpression.type) {
		case 'reference':
			const dereferenced = findSymbolInScopesWithBuiltIns(sourceExpression.name.name, scopes);
			if (!dereferenced) {
				return undefined;
			}
			const dereferencedType = dereferenced.symbol.typeExpression;
			if (dereferencedType) {
				// TODO fix scopes?
				return dereferenceTypeExpression(dereferencedType, scopes, folderPath);
			}
			const definition = dereferenced.symbol.definition;
			if (definition?.type === 'destructuringField') {
				const importedSymbol = getImportedSymbol(definition, folderPath);
				const typeExpression = importedSymbol?.symbol?.typeExpression;
				return typeExpression && {
					typeExpression: typeExpression,
					filePath: importedSymbol.filePath,
				};
			}
			return undefined;
		default:
			return { typeExpression: sourceExpression };
	}
}

// TODO declaredType in TypeInfo packen (in checker inferType)?
// TODO CompileTimeType vs TypeExpression vs Symbol liefern?
function getDeclaredType(expression: PositionedExpression): TypeInfo | undefined {
	switch (expression.type) {
		case 'nestedReference': {
			const nestedKey = expression.nestedKey;
			if (!nestedKey) {
				return undefined;
			}
			const sourceType = getDeclaredType(expression.source);
			if (!sourceType) {
				return undefined;
			}
			switch (nestedKey.type) {
				case 'index': {
					const dereferencedType = dereferenceIndexFromObject(nestedKey.name, sourceType.dereferencedType);
					if (dereferencedType === null) {
						return undefined;
					}
					return {
						rawType: dereferencedType,
						dereferencedType: dereferencedType,
					};
				}
				case 'name':
				case 'text': {
					const fieldName = getCheckedEscapableName(nestedKey);
					if (!fieldName) {
						return undefined;
					}
					const dereferencedType = dereferenceNameFromObject(fieldName, sourceType.dereferencedType);
					if (dereferencedType === null) {
						return undefined;
					}
					return {
						rawType: dereferencedType,
						dereferencedType: dereferencedType,
					};
				}
				default: {
					const assertNever: never = nestedKey;
					throw new Error(`Unexpected nestedKey.type ${(assertNever as PositionedExpression).type}`);
				}
			}
		}
		case 'reference': {
			return expression.typeInfo;
			// TODO? stattdessen declaredType der symbol definition aus typeguard holen
			// const name = expression.name.name;
			// const definition = findSymbolInScopesWithBuiltIns(name, scopes);
			// return definition && {
			// 	...definition,
			// 	name: name,
			// };
		}
		default:
			break;
	}
	// TODO recursive getDeclaredType für List elements
	switch (expression.parent?.type) {
		case 'definition':
			if (expression.parent.value === expression
				&& expression.parent.typeGuard) {
				const typeGuardType = expression.parent.typeGuard.typeInfo;
				if (!typeGuardType) {
					return undefined;
				}
				if (isTypeOfType(typeGuardType.dereferencedType)) {
					// TODO? woher rawType? TypeInfo in CompileTimeTypeOfType.value?
					return {
						rawType: typeGuardType.dereferencedType.value,
						dereferencedType: typeGuardType.dereferencedType.value,
					};
				}
				return typeGuardType;
			}
			else {
				return undefined;
			}
		case 'functionLiteral': {
			if (expression.parent.params !== expression) {
				return undefined;
			}
			const functionLiteralDeclaredType = getDeclaredType(expression.parent);
			if (!functionLiteralDeclaredType) {
				return undefined;
			}
			if (!isFunctionType(functionLiteralDeclaredType.dereferencedType)) {
				return undefined;
			}
			// TODO? woher rawType? TypeInfo in CompileTimeFunctionType.ParamsType?
			return {
				rawType: functionLiteralDeclaredType.dereferencedType.ParamsType,
				dereferencedType: functionLiteralDeclaredType.dereferencedType.ParamsType,
			};
		}
		case 'functionCall': {
			// function call arg
			// TODO handle prefix arg
			if (expression.parent.arguments !== expression) {
				return undefined;
			}
			const functionExpression = expression.parent.functionExpression;
			if (!functionExpression) {
				return undefined;
			}
			const functionType = functionExpression.typeInfo;
			if (!functionType) {
				return undefined;
			}
			if (!isFunctionType(functionType.dereferencedType)) {
				return undefined;
			}
			// TODO? woher rawType? TypeInfo in CompileTimeFunctionType.ParamsType?
			return {
				rawType: functionType.dereferencedType.ParamsType,
				dereferencedType: functionType.dereferencedType.ParamsType,
			};
		}
		case 'list': {
			const list = expression.parent;
			const listType = getDeclaredType(list);
			if (!listType) {
				return undefined;
			}
			const functionCall = list.parent;
			if (functionCall?.type === 'functionCall'
				&& functionCall.arguments === list) {
				const dereferencedlistType = listType.dereferencedType;
				// function call arg
				if (!isParametersType(dereferencedlistType)) {
					return undefined;
				}
				const parameterCount = dereferencedlistType.singleNames.length + (dereferencedlistType.rest ? 1 : 0);
				const parameterIndex = getParameterIndex(functionCall, expression.startRowIndex, expression.startColumnIndex, parameterCount);
				const currentParameter = parameterIndex < dereferencedlistType.singleNames.length
					? dereferencedlistType.singleNames[parameterIndex]
					: dereferencedlistType.rest;
				if (!currentParameter) {
					return undefined;
				}
				// TODO? woher rawType? TypeInfo in Parameter.type?
				if (currentParameter.type === null) {
					return undefined;
				}
				return {
					rawType: currentParameter.type,
					dereferencedType: currentParameter.type,
				};
			}
			const index = list.values.indexOf(expression as any);
			const elementType = dereferenceIndexFromObject(index, listType.dereferencedType);
			// TODO? woher rawType?
			if (elementType === null) {
				return undefined;
			}
			return {
				rawType: elementType,
				dereferencedType: elementType,
			};
		}
		case 'singleDictionaryField': {
			const dictionary = expression.parent.parent;
			if (!dictionary) {
				return undefined;
			}
			const dictionaryDeclaredType = getDeclaredType(dictionary);
			if (!dictionaryDeclaredType) {
				return undefined;
			}
			const nameString = getCheckedEscapableName(expression.parent.name);
			if (!nameString) {
				return undefined;
			}
			const fieldType = dereferenceNameFromObject(nameString, dictionaryDeclaredType.dereferencedType);
			// TODO? woher rawType?
			if (fieldType === null) {
				return undefined;
			}
			return {
				rawType: fieldType,
				dereferencedType: fieldType,
			};
		}
		case undefined:
			return undefined;
		default:
			return undefined;
	}
}

/**
 * Ermittelt den Index des Parameters, für den das Argument ist, das an der Position liegt.
 * Position muss in arguments liegen.
 */
function getParameterIndex(
	functionCall: ParseFunctionCall,
	rowIndex: number,
	columnIndex: number,
	parameterCount: number,
) {
	const argsExpression = functionCall.arguments;
	// TODO parameter index ermitteln bei function call mit dictionary Argument
	let parameterIndex = functionCall.prefixArgument ? 1 : 0;
	if (argsExpression?.type === 'list') {
		argsExpression.values.forEach(value => {
			// values vor der aktuellen Position zählen
			if ((value.endRowIndex < rowIndex ||
				(value.endRowIndex === rowIndex && value.endColumnIndex < columnIndex))
				// TODO was wenn mehr values als Parameter (ohne Rest Parameter)?
				&& parameterIndex < parameterCount - 1
			) {
				parameterIndex++;
			}
		});
	}
	return parameterIndex;
}

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
	return URI.file(path).toString();
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

function getTypeMarkdown(
	type: TypeInfo | undefined,
	description: string | undefined,
): MarkupContent {
	console.log(type);
	const typeString = type
		? `\`\`\`jul
${typeToString(type.dereferencedType, 0)}
\`\`\`
`
		: '';
	return {
		kind: 'markdown',
		value: typeString + (description ?? ''),
	};
}

//#endregion helper