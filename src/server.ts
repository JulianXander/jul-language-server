import { readdirSync, statSync } from 'fs';
import { dirname, extname, join } from 'path';
import {
	LanguageService,
	getLanguageService as getHtmlLanguageService,
} from 'vscode-html-languageservice';
import {
	CompletionItem,
	CompletionItemKind,
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
	SemanticTokensBuilder,
	SignatureHelp,
	SymbolKind,
	TextDocumentIdentifier,
	TextDocuments,
	TextDocumentSyncKind,
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
	parseCode
} from 'jul-compiler/out/parser/parser.js';
import { getCheckedEscapableName } from 'jul-compiler/out/parser/parser-utils.js';
import { CompilerErrorSeverity, errorInfos, Positioned } from 'jul-compiler/out/compiler-errors.js';
import {
	CompileTimeDictionary,
	CompileTimeType,
	DefinitionExpression,
	forEachChild,
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
	TextLiteralType,
} from 'jul-compiler/out/syntax-tree.js';
import {
	resolvePlaceholders,
	builtInSymbols,
	checkTypes,
	dereferenceIndexFromObject,
	dereferenceNameFromObject,
	findSymbolInScopesWithBuiltIns,
	getStreamGetValueType,
	getTypeError,
	isDictionaryLiteralType,
	isFunctionType,
	isListType,
	isParameterReference,
	isParametersType,
	isTextLiteralType,
	isTupleType,
	isTypeOfType,
	ParsedDocuments,
	typeToString,
} from 'jul-compiler/out/checker/checker.js';
import { isDefined, isValidExtension, map, tryReadTextFile } from 'jul-compiler/out/util.js';

/**
 * Der Server zeigt und prüft Typen, verarbeitet sie aber nicht weiter - hier ist die aufgelöste
 * Form also durchgängig die richtige.
 */
function getResolvedType(typeInfo: TypeInfo | undefined): CompileTimeType | undefined {
	return typeInfo && resolvePlaceholders(typeInfo.type);
}

function getDeclaredResolvedType(expression: PositionedExpression): CompileTimeType | undefined {
	return getResolvedType(getDeclaredType(expression));
}

// For performance do not process large files
const maxFileSize = 100000;

// Create a connection for the server, using Node's IPC as a transport.
// Also include all preview / proposed LSP features.
const connection = createConnection(ProposedFeatures.all);

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
	const { errors } = parsed.checked!;
	const diagnostics: Diagnostic[] = errors.map(error => {
		const diagnostic: Diagnostic = {
			severity: diagnosticSeverities[errorInfos[error.code].severity],
			range: positionedToRange(error),
			code: error.code,
			message: error.message,
			source: 'jul'
		};
		if (hasDiagnosticRelatedInformationCapability && error.relatedInformation) {
			diagnostic.relatedInformation = [
				{
					location: {
						uri: textDocument.uri,
						range: positionedToRange(error.relatedInformation),
					},
					message: error.relatedInformation.message,
				},
			];
		}
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
		const prefixArgumentTypeRaw = infixFunctionCall.prefixArgument!.typeInfo!.type;
		let prefixArgumentType: CompileTimeType | undefined;
		if (isParameterReference(prefixArgumentTypeRaw)) {
			const dereferenced = findSymbolInScopesWithBuiltIns(prefixArgumentTypeRaw.name, scopes);
			prefixArgumentType = dereferenced?.symbol.typeInfo?.type;
		}
		else {
			prefixArgumentType = prefixArgumentTypeRaw;
		}

		symbolFilter = symbol => {
			if (!prefixArgumentType) {
				return false;
			}
			const symbolType = getResolvedType(symbol.typeInfo);
			if (isFunctionType(symbolType)) {
				const paramsType = symbolType.ParamsType;
				if (isParametersType(paramsType)) {
					let firstParameterType: CompileTimeType | undefined;
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
					if (!firstParameterType) {
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
		const dereferencedType = getResolvedType(expression.source?.typeInfo);
		return dereferencedType && getNestedReferenceCompletionItems(dereferencedType);
	}
	//#endregion / field reference

	//#region dictionary literal field
	if (expression?.type === 'empty'
		|| expression?.type === 'dictionary'
		|| expression?.type === 'object') {
		const declaredType = getDeclaredResolvedType(expression);
		const allCompletionItems = declaredType && getDictionaryFieldCompletionItemsFromType(declaredType);
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

	return symbolsToCompletionItems(allScopes);
});

//#region create CompletionItems

function getDictionaryFieldCompletionItemsFromType(declaredType: CompileTimeType): CompletionItem[] | undefined {
	switch (declaredType.julType) {
		case 'dictionaryLiteral':
			return dictionaryTypeToCompletionItems(declaredType.Fields);
		case 'or': {
			const allCompletionItems: CompletionItem[] = [];
			declaredType.ChoiceTypes.forEach(choiceType => {
				const completionItems = getDictionaryFieldCompletionItemsFromType(choiceType);
				completionItems?.forEach(newCompletionItem => {
					// Duplikate vermeiden
					if (!allCompletionItems?.some(existingCompletionItem => existingCompletionItem.label === newCompletionItem.label)) {
						allCompletionItems?.push(newCompletionItem);
					}
				});
			});
			return allCompletionItems;
		}
		case 'parameters': {
			// function call arg
			const allCompletionItems = declaredType.singleNames.map((singleName, index) => {
				return parameterToCompletionItem(singleName, index, false);
			});
			return allCompletionItems;
		}
		default:
			return undefined;
	}
}

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

function dictionaryTypeToCompletionItems(
	fields: CompileTimeDictionary,
): CompletionItem[] {
	return map(
		fields,
		(type, name) => {
			const completionItem: CompletionItem = {
				label: name,
				kind: CompletionItemKind.Constant,
				detail: typeToString(type, 0, 0),
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
				const symbolType = getResolvedType(symbol.typeInfo);
				const isFunction = isFunctionType(symbolType);
				const completionItem: CompletionItem = {
					label: name,
					kind: isFunction
						? CompletionItemKind.Function
						: CompletionItemKind.Constant,
					detail: symbolType && typeToString(symbolType, 0, 0),
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
		case 'binding':
		case 'data':
			// return findAllOccurrencesInExpression(expression.fields, name);
			return [];
		case 'branching': {
			const args = expression.args;
			const occurences = [
				...(args ? findAllOccurrencesInExpression(args, searchTerm) : []),
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

function getSymbolDefinition(
	expression: PositionedExpression,
	scopes: SymbolTable[],
	folderPath: string,
): SymbolInfo | undefined {
	switch (expression.type) {
		case 'reference': {
			const name = expression.name.name;
			const definition = findSymbolInScopesWithBuiltIns(name, scopes);
			return definition && resolveThroughImports({
				...definition,
				name: name,
			}, folderPath);
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
					return importedSymbol?.symbol && resolveThroughImports({
						name: name,
						isBuiltIn: false,
						symbol: importedSymbol.symbol,
						filePath: importedSymbol.filePath,
					}, dirname(importedSymbol.filePath));
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
					return definition && resolveThroughImports({
						...definition,
						name: name,
					}, folderPath);
				}
			}
		}
		case 'binding':
		case 'data':
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

function getSymbolFromDictionaryType(
	dictionaryType: CompileTimeType,
	name: string,
): SymbolInfo | undefined {
	switch (dictionaryType.julType) {
		case 'dictionaryLiteral': {
			const declaration = dictionaryType.declaration;
			if (!declaration) {
				return undefined;
			}
			const foundSymbol = declaration.expression.symbols[name];
			return foundSymbol && {
				name: name,
				isBuiltIn: declaration.filePath === '',
				symbol: foundSymbol,
				filePath: declaration.filePath,
			};
		}
		case 'or': {
			// TODO return list of Symbols?
			for (const choiceType of dictionaryType.ChoiceTypes) {
				const choiceSymbol = getSymbolFromDictionaryType(choiceType, name);
				if (choiceSymbol) {
					return choiceSymbol;
				}
			}
			return undefined;
		}
		case 'typeOf':
			return getSymbolFromDictionaryType(dictionaryType.value, name);
		default:
			return undefined;
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
					symbol: importedSymbol,
					filePath: fullPath,
				};
			}
		}
	}
}

// Löst Verweise auf importierte Symbole direkt bis zur tatsächlichen Deklaration auf, statt bei
// jedem Hop erneut an der lokalen Import-Zeile stehen zu bleiben (auch über mehrere Re-Exports).
function resolveThroughImports(symbolInfo: SymbolInfo, folderPath: string): SymbolInfo {
	let current = symbolInfo;
	let currentFolderPath = folderPath;
	const visitedFilePaths = new Set<string>();
	for (; ;) {
		const definition = current.symbol.definition;
		if (definition?.type !== 'destructuringField') {
			return current;
		}
		const imported = getImportedSymbol(definition, currentFolderPath);
		if (!imported?.symbol || visitedFilePaths.has(imported.filePath)) {
			return current;
		}
		visitedFilePaths.add(imported.filePath);
		current = {
			name: current.name,
			isBuiltIn: false,
			symbol: imported.symbol,
			filePath: imported.filePath,
		};
		currentFolderPath = dirname(imported.filePath);
	}
}

//#endregion get Symbol

// TODO declaredType in TypeInfo packen (in checker inferType)?
// TODO CompileTimeType vs TypeExpression vs Symbol liefern?
function getDeclaredType(expression: PositionedExpression): TypeInfo | undefined {
	switch (expression.type) {
		case 'functionCall':
			return expression.typeInfo;
		case 'name': {
			if (expression.parent?.type === 'nestedReference') {
				return getDeclaredType(expression.parent);
			}
			break;
		}
		case 'nestedReference': {
			const nestedKey = expression.nestedKey;
			if (!nestedKey) {
				return undefined;
			}
			const sourceType = getDeclaredResolvedType(expression.source);
			if (!sourceType) {
				return undefined;
			}
			switch (nestedKey.type) {
				case 'index': {
					const dereferencedType = dereferenceIndexFromObject(nestedKey.name, sourceType);
					if (!dereferencedType) {
						return undefined;
					}
					return { type: dereferencedType };
				}
				case 'name':
				case 'text': {
					const fieldName = getCheckedEscapableName(nestedKey);
					if (!fieldName) {
						return undefined;
					}
					const dereferencedType = dereferenceNameFromObject(fieldName, sourceType);
					if (!dereferencedType) {
						return undefined;
					}
					return { type: dereferencedType };
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
			if (expression.parent.value === expression) {
				if (expression.parent.typeGuard) {
					const typeGuardType = expression.parent.typeGuard.typeInfo;
					if (!typeGuardType) {
						return undefined;
					}
					const resolvedTypeGuardType = resolvePlaceholders(typeGuardType.type);
					if (isTypeOfType(resolvedTypeGuardType)) {
						return { type: resolvedTypeGuardType.value };
					}
					return typeGuardType;
				}
				else {
					return expression.typeInfo;
				}
			}
			else {
				return undefined;
			}
		case 'functionLiteral': {
			if (expression.parent.params !== expression) {
				return undefined;
			}
			const functionLiteralDeclaredType = getDeclaredResolvedType(expression.parent);
			if (!functionLiteralDeclaredType) {
				return undefined;
			}
			if (!isFunctionType(functionLiteralDeclaredType)) {
				return undefined;
			}
			return { type: functionLiteralDeclaredType.ParamsType };
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
			const functionType = getResolvedType(functionExpression.typeInfo);
			if (!functionType) {
				return undefined;
			}
			if (!isFunctionType(functionType)) {
				return undefined;
			}
			return { type: functionType.ParamsType };
		}
		case 'list': {
			const list = expression.parent;
			const listType = getDeclaredResolvedType(list);
			if (!listType) {
				return undefined;
			}
			const functionCall = list.parent;
			if (functionCall?.type === 'functionCall'
				&& functionCall.arguments === list) {
				const dereferencedlistType = listType;
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
				if (!currentParameter.type) {
					return undefined;
				}
				return { type: currentParameter.type };
			}
			const index = list.values.indexOf(expression as any);
			const elementType = dereferenceIndexFromObject(index, listType);
			if (!elementType) {
				return undefined;
			}
			return { type: elementType };
		}
		case 'singleDictionaryField': {
			const dictionary = expression.parent.parent;
			if (!dictionary) {
				return undefined;
			}
			const dictionaryDeclaredType = getDeclaredResolvedType(dictionary);
			if (!dictionaryDeclaredType) {
				return undefined;
			}
			const nameString = getCheckedEscapableName(expression.parent.name);
			if (!nameString) {
				return undefined;
			}
			const fieldType = dereferenceNameFromObject(nameString, dictionaryDeclaredType);
			if (!fieldType) {
				return undefined;
			}
			return { type: fieldType };
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

/**
 * Der Compiler kennt die LSP-Zahlen nicht - er läuft auch als CLI.
 * Hier werden seine Werte auf DiagnosticSeverity übersetzt.
 */
const diagnosticSeverities: { [Severity in CompilerErrorSeverity]: DiagnosticSeverity; } = {
	error: DiagnosticSeverity.Error,
	warning: DiagnosticSeverity.Warning,
	hint: DiagnosticSeverity.Hint,
};

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
