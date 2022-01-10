import {
	createConnection,
	TextDocuments,
	Diagnostic,
	DiagnosticSeverity,
	ProposedFeatures,
	InitializeParams,
	CompletionItem,
	CompletionItemKind,
	TextDocumentPositionParams,
	TextDocumentSyncKind,
	InitializeResult,
	Range,
} from 'vscode-languageserver/node';
import {
	TextDocument
} from 'vscode-languageserver-textdocument';

// import { builtInSymbols } from '../../jul-compiler/src/checker';
import { parseCode } from '../../jul-compiler/src/parser';
import { Positioned } from '../../jul-compiler/src/parser-combinator';
import {
	PositionedExpression,
	ParseValueExpression,
	SymbolTable,
	ParsedFile,
	SymbolDefinition,
	ParseFunctionCall,
	BracketedExpression,
} from '../../jul-compiler/src/syntax-tree';
import {
	checkTypes,
	coreLibPath,
	dereferenceWithBuiltIns,
} from '../../jul-compiler/src/type-checker';
import { BuiltInType, BuiltInTypeBase, Type } from '../../jul-compiler/src/runtime';
import { map } from '../../jul-compiler/src/util';

// Create a connection for the server, using Node's IPC as a transport.
// Also include all preview / proposed LSP features.
const connection = createConnection(ProposedFeatures.all);

// Create a simple text document manager.
const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);
const parsedDocuments: { [documentUri: string]: ParsedFile; } = {};

let hasDiagnosticRelatedInformationCapability = false;

connection.onInitialize((params: InitializeParams) => {
	const capabilities = params.capabilities;

	hasDiagnosticRelatedInformationCapability = !!capabilities.textDocument?.publishDiagnostics?.relatedInformation;

	const result: InitializeResult = {
		capabilities: {
			textDocumentSync: TextDocumentSyncKind.Incremental,
			// Tell the client that this server supports code completion.
			completionProvider: {
				resolveProvider: true
			},
			definitionProvider: true,
			hoverProvider: true,
		}
	};
	return result;
});

// This event is emitted when the text document first opened or when its content has changed.
documents.onDidChangeContent(change => {
	validateTextDocument(change.document);
});

async function validateTextDocument(textDocument: TextDocument): Promise<void> {
	const text = textDocument.getText();
	const parsed = parseCode(text);
	parsedDocuments[textDocument.uri] = parsed;
	// TODO recursively parse imported files
	// TODO invalidate imported inferred types of this file in other files
	// infertypes, typecheck
	checkTypes(parsedDocuments);
	const { errors } = parsed;

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

connection.onDidChangeWatchedFiles(_change => {
	// Monitored files have change in VSCode
	connection.console.log('We received an file change event');
});

//#region autocomplete
// This handler provides the initial list of the completion items.
connection.onCompletion((_textDocumentPosition: TextDocumentPositionParams): CompletionItem[] => {
	// The pass parameter contains the position of the text document in
	// which code complete got requested. For the example we ignore this
	// info and always provide the same completion items.
	return [
		{
			label: 'TypeScript',
			kind: CompletionItemKind.Text,
			data: 1
		},
		{
			label: 'JavaScript',
			kind: CompletionItemKind.Text,
			data: 2
		}
	];
});

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
connection.onDefinition((definitionParams) => {
	const documentUri = definitionParams.textDocument.uri;
	const parsed = parsedDocuments[documentUri];
	if (!parsed) {
		return;
	}
	const foundSymbol = getSymbolDefinition(parsed, definitionParams.position.line, definitionParams.position.character);
	if (foundSymbol) {
		return {
			uri: foundSymbol.isBuiltIn
				? coreLibPath
				: documentUri,
			range: positionedToRange(foundSymbol.symbol)
		};
	}
});
//#endregion go to definition

//#region hover
connection.onHover((hoverParams) => {
	const parsed = parsedDocuments[hoverParams.textDocument.uri];
	if (!parsed) {
		return;
	}
	// TODO find funciontLiteral, show param + return type
	// const foundExpression = findExpressionInParsedFile(parsed, hoverParams.position.line, hoverParams.position.character);
	// // TODO functionCall, definition berücksichtigen?
	// if (foundExpression?.type === 'reference') {
	// 	// hoverParams.position.
	// 	// TODO find expression by position row/column
	// 	// parsed.parsed
	// 	// TODO check for ref, provide Type information, doc comments
	// 	return {
	// 		contents: 'test: ' + foundExpression.names[0],
	// 	};
	// }
	// return {
	// 	contents: 'expr type: ' + foundExpression?.type,
	// };

	const foundSymbol = getSymbolDefinition(parsed, hoverParams.position.line, hoverParams.position.character);
	if (foundSymbol) {
		const symbol = foundSymbol.symbol;
		const symbolType = symbol.normalizedType;
		console.log(symbolType);
		const typeString = symbolType
			? `\`\`\`jul
${typeToString(symbolType)}
\`\`\`
`
			: '';
		return {
			contents: {
				kind: 'markdown',
				value: typeString + (symbol.description ?? ''),
			}
			// contents:  ['type: ', { language: 'jul', value: '(test: String) => stream$(12)' }, '\ndescription: ' + foundSymbol.symbol.description]
		};
	}
});
//#endregion hover

// Make the text document manager listen on the connection
// for open, change and close text document events
documents.listen(connection);

// Listen on the connection
connection.listen();

//#region helper

//#region findExpression

/**
 * Füllt scopes
 */
function findExpressionInParsedFile(
	parsedFile: ParsedFile,
	rowIndex: number,
	columnIndex: number,
	scopes: SymbolTable[],
): PositionedExpression | undefined {
	return parsedFile.expressions && findExpressionInExpressions(
		parsedFile.expressions,
		rowIndex,
		columnIndex,
		scopes);
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

		case 'field':
			// TODO check name range, source, typeguard, fallback
			return expression;

		case 'functionCall': {
			if (isPositionInRange(rowIndex, columnIndex, expression.functionReference)) {
				return expression.functionReference;
			}
			const foundArguments = findExpressionInExpression(expression.arguments, rowIndex, columnIndex, scopes);
			return foundArguments;
		}

		case 'functionLiteral': {
			scopes.push(expression.symbols);
			if (isPositionInRange(rowIndex, columnIndex, expression.params)) {
				const foundParams = findExpressionInExpression(expression.params, rowIndex, columnIndex, scopes);
				return foundParams;
			}
			const returnType = expression.returnType;
			if (returnType && isPositionInRange(rowIndex, columnIndex, returnType)) {
				const foundParams = findExpressionInExpression(returnType, rowIndex, columnIndex, scopes);
				return foundParams;
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

		case 'list': {
			const foundValue = findExpressionInExpressions(expression.values, rowIndex, columnIndex, scopes);
			return foundValue;
		}

		case 'name':
			return expression;

		case 'number':
			return undefined;

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
			// TODO find expression name aus dem array names
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

		case 'spreadDictionaryField': {
			if (isPositionInRange(rowIndex, columnIndex, expression.value)) {
				const foundValue = findExpressionInExpression(expression.value, rowIndex, columnIndex, scopes);
				return foundValue;
			}
			return expression;
		}

		case 'spreadDictionaryTypeField': {
			if (isPositionInRange(rowIndex, columnIndex, expression.value)) {
				const foundValue = findExpressionInExpression(expression.value, rowIndex, columnIndex, scopes);
				return foundValue;
			}
			return expression;
		}

		case 'string': {
			const values = expression.values.filter((value): value is ParseValueExpression =>
				value.type !== 'stringToken');
			const foundValue = findExpressionInExpressions(values, rowIndex, columnIndex, scopes);
			return foundValue;
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

// TODO got to source file bei import?
function getSymbolDefinition(
	parsedFile: ParsedFile,
	rowIndex: number,
	columnIndex: number,
): {
	isBuiltIn: boolean;
	symbol: SymbolDefinition;
} | undefined {
	const scopes: SymbolTable[] = [
		parsedFile.symbols,
	];
	const expression = findExpressionInParsedFile(parsedFile, rowIndex, columnIndex, scopes);
	if (!expression) {
		return undefined;
	}
	switch (expression.type) {
		case 'reference': {
			const definition = dereferenceWithBuiltIns(expression, scopes);
			return definition;
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
			// 	const importedFile = parsedDocuments[importedPath + '.jul'];
			// 	const importedSymbol = importedFile?.symbols[expression.fields];
			// }
			return undefined;
		}

		case 'bracketed':
		case 'branching':
		case 'dictionary':
		case 'dictionaryType':
		case 'empty':
		case 'field':
		case 'functionCall':
		case 'functionLiteral':
		case 'functionTypeLiteral':
		case 'index':
		case 'list':
		case 'name':
		case 'number':
		case 'parameter':
		case 'parameters':
		case 'singleDictionaryField':
		case 'singleDictionaryTypeField':
		case 'spreadDictionaryField':
		case 'spreadDictionaryTypeField':
		case 'string':
			return undefined;

		default: {
			const assertNever: never = expression;
			throw new Error(`Unexpected expression.type: ${(assertNever as PositionedExpression).type}`);
		}
	}
}

//#region import

function isImport(expression: ParseValueExpression): expression is ParseFunctionCall {
	if (expression.type !== 'functionCall') {
		return false;
	}
	const functionReferencePath = expression.functionReference.path;
	return functionReferencePath.length === 1
		&& functionReferencePath[0].name === 'import';
}

function getPathFromImport(importExpression: ParseFunctionCall): string | undefined {
	const pathExpression = getPathExpression(importExpression.arguments);
	if (pathExpression?.type === 'string'
		&& pathExpression.values.length === 1
		&& pathExpression.values[0]!.type === 'stringToken') {
		const importedPath = pathExpression.values[0].value;
		return importedPath;
	}
	// TODO dynamische imports verbieten???
	return undefined;
}

function getPathExpression(importParams: BracketedExpression): ParseValueExpression | undefined {
	switch (importParams.type) {
		case 'dictionary':
			return importParams.fields[0].value;

		case 'bracketed':
		case 'dictionaryType':
		case 'empty':
			return undefined;

		case 'list':
			return importParams.values[0];

		default: {
			const assertNever: never = importParams;
			throw new Error(`Unexpected importParams.type: ${(assertNever as BracketedExpression).type}`);
		}
	}
}

//#endregion import

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

//#region ToString

function typeToString(type: Type): string {
	switch (typeof type) {
		case 'string':
			return `§${type.replaceAll('§', '§§')}§`;

		case 'boolean':
		case 'number':
			return type.toString();

		case 'object': {
			if (type === null) {
				return '()';
			}
			if (Array.isArray(type)) {
				return arrayTypeToString(type);
			}
			if (type instanceof BuiltInTypeBase) {
				const builtInType: BuiltInType = type;
				switch (builtInType.type) {
					case 'and':
						return `And${arrayTypeToString(builtInType.choiceTypes)}`;

					case 'any':
						return 'Any';

					case 'boolean':
						return 'Boolean';

					case 'dictionary':
						return `Dictionary(${typeToString(builtInType.elementType)})`;

					case 'dictionaryLiteral':
						return dictionaryTypeToString(builtInType.fields, ': ');

					case 'error':
						return 'Error';

					case 'float64':
						return 'Float64';

					case 'function':
						return `${typeToString(builtInType.paramsType)} => ${typeToString(builtInType.returnType)}`;

					case 'list':
						return `List(${typeToString(builtInType.elementType)})`;

					case 'or':
						return `Or${arrayTypeToString(builtInType.choiceTypes)}`;

					case 'reference':
						return builtInType.path.map(pathSegment => {
							return pathSegment.name;
						}).join('/');

					case 'stream':
						return `Stream(${typeToString(builtInType.valueType)})`;

					case 'string':
						return 'String';

					case 'tuple':
						return arrayTypeToString(builtInType.elementTypes);

					case 'type':
						return 'Type';

					case 'typeOf':
						return `TypeOf(${typeToString(builtInType.value)})`;

					default: {
						const assertNever: never = builtInType;
						throw new Error(`Unexpected BuiltInType ${(builtInType as BuiltInType).type}`);
					}
				}
			}
			// Dictionary
			return dictionaryTypeToString(type, ' = ');
		}


		default:
			throw new Error(`Unexpected type ${typeof type}`);
	}
}

const maxElementsPerLine = 5;
function arrayTypeToString(array: Type[]): string {
	const multiline = array.length > maxElementsPerLine;
	const indent = multiline
		? '\t'
		: '';
	const bracketSeparator = multiline
		? '\n'
		: '';
	const lineSeparator = multiline
		? '\n'
		: ' ';
	return `(${bracketSeparator}${array.map(element =>
		indent + typeToString(element)).join(lineSeparator)}${bracketSeparator})`;
}

function dictionaryTypeToString(
	dictionary: { [key: string]: Type; },
	nameSeparator: string,
): string {
	const multiline = Object.keys(dictionary).length > 1;
	const indent = multiline
		? '\t'
		: '';
	const bracketSeparator = multiline
		? '\n'
		: '';
	return `(${bracketSeparator}${map(
		dictionary,
		(element, key) => {
			return `${indent}${key}${nameSeparator}${typeToString(element)}`;
		}).join('\n')}${bracketSeparator})`;
}

//#endregion ToString

//#endregion helper