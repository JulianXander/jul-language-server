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
	InitializeResult
} from 'vscode-languageserver/node';
import {
	TextDocument
} from 'vscode-languageserver-textdocument';

import { parseCode } from '../../jul-compiler/src/parser';
import { AbstractSyntaxTree, DefinitionNames, Expression, Positioned, PositionedExpression, ValueExpression } from '../../jul-compiler/src/abstract-syntax-tree';

// Create a connection for the server, using Node's IPC as a transport.
// Also include all preview / proposed LSP features.
const connection = createConnection(ProposedFeatures.all);

// Create a simple text document manager.
const documents: TextDocuments<TextDocument> = new TextDocuments(TextDocument);
const parsedDocuments: { [documentUri: string]: AbstractSyntaxTree; } = {};

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
			hoverProvider: true
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
	const { errors } = parsed;

	const diagnostics: Diagnostic[] = errors?.map(error => {
		const diagnostic: Diagnostic = {
			severity: DiagnosticSeverity.Error,
			range: {
				start: { line: error.rowIndex, character: error.columnIndex },
				// TODO error length/ start+endindex in parserError
				end: { line: error.rowIndex, character: error.columnIndex + 4 },
			},
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
	}) ?? [];

	// Send the computed diagnostics to VSCode.
	connection.sendDiagnostics({ uri: textDocument.uri, diagnostics });
}

connection.onDidChangeWatchedFiles(_change => {
	// Monitored files have change in VSCode
	connection.console.log('We received an file change event');
});

//#region autocomplete
// This handler provides the initial list of the completion items.
connection.onCompletion(
	(_textDocumentPosition: TextDocumentPositionParams): CompletionItem[] => {
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
	}
);

// This handler resolves additional information for the item selected in
// the completion list.
connection.onCompletionResolve(
	(item: CompletionItem): CompletionItem => {
		if (item.data === 1) {
			item.detail = 'TypeScript details';
			item.documentation = 'TypeScript documentation';
		} else if (item.data === 2) {
			item.detail = 'JavaScript details';
			item.documentation = 'JavaScript documentation';
		}
		return item;
	}
);
//#endregion autocomplete

//#region hover
connection.onHover((hoverParams) => {
	const parsed = parsedDocuments[hoverParams.textDocument.uri];
	if (!parsed) {
		return;
	}
	const foundExpression = findExpressionInAst(parsed, hoverParams.position.line, hoverParams.position.character);
	// TODO functionCall, definition berücksichtigen?
	if (foundExpression?.type === 'reference') {
		// hoverParams.position.
		// TODO find expression by position row/column
		// parsed.parsed
		// TODO check for ref, provide Type information, doc comments
		return {
			contents: 'test: ' + foundExpression.names[0],
		};
	}
	return {
		contents: 'expr type: ' + foundExpression?.type,
	};
});
//#endregion hover

// Make the text document manager listen on the connection
// for open, change and close text document events
documents.listen(connection);

// Listen on the connection
connection.listen();

//#region helper

//#region findExpression

function findExpressionInAst(
	ast: AbstractSyntaxTree,
	rowIndex: number,
	columnIndex: number,
): PositionedExpression | undefined {
	return ast.parsed && findExpressionInExpressions(ast.parsed, rowIndex, columnIndex);
}

function findExpressionInExpressions(
	expressions: PositionedExpression[],
	rowIndex: number,
	columnIndex: number,
): PositionedExpression | undefined {
	const foundOuter = expressions.find(expression => {
		return isPositionInRange(rowIndex, columnIndex, expression);
	});
	if (!foundOuter) {
		return undefined;
	}
	const foundInner = findExpressionInExpression(foundOuter, rowIndex, columnIndex);
	return foundInner;
}

function findExpressionInExpression(
	expression: PositionedExpression,
	rowIndex: number,
	columnIndex: number,
): PositionedExpression | undefined {
	switch (expression.type) {
		case 'branching': {
			if (isPositionInRange(rowIndex, columnIndex, expression.value)) {
				const foundValue = findExpressionInExpression(expression.value, rowIndex, columnIndex);
				return foundValue;
			}
			const foundBranch = findExpressionInExpressions(expression.branches, rowIndex, columnIndex);
			return foundBranch;
		}

		case 'definition': {
			if (isPositionInRange(rowIndex, columnIndex, expression.name)) {
				return expression.name;
			}
			const foundValue = findExpressionInExpression(expression.value, rowIndex, columnIndex);
			return foundValue;
		}

		case 'definitionNames': {
			// TODO rest
			// if (expression.rest &&  isPositionInRange(rowIndex, columnIndex, expression.rest)) {
			// 	return expression.rest;
			// }
			const foundValue = findExpressionInExpressions(expression.singleNames, rowIndex, columnIndex);
			return foundValue;
		}

		case 'destructuring': {
			if (isPositionInRange(rowIndex, columnIndex, expression.names)) {
				const foundName = findExpressionInExpression(expression.names, rowIndex, columnIndex);
				return foundName;
			}
			const foundValue = findExpressionInExpression(expression.value, rowIndex, columnIndex);
			return foundValue;
		}

		case 'dictionary': {
			// if (isPositionInRange(rowIndex, columnIndex, expression.names)) {
			// 	const foundName = findExpressionInExpression(expression.names, rowIndex, columnIndex);
			// 	return foundName;
			// }
			// const foundValue = findExpressionInExpression(expression.values, rowIndex, columnIndex);
			// return foundValue;
			return undefined;
		}

		case 'empty':
			return undefined;

		case 'functionCall': {
			if (isPositionInRange(rowIndex, columnIndex, expression.functionReference)) {
				return expression.functionReference;
			}
			const foundArguments = findExpressionInExpression(expression.arguments, rowIndex, columnIndex);
			return foundArguments;
		}

		case 'functionLiteral': {
			if (isPositionInRange(rowIndex, columnIndex, expression.params)) {
				const foundParams = findExpressionInExpression(expression.params, rowIndex, columnIndex);
				return foundParams;
			}
			const foundBody = findExpressionInExpressions(expression.body, rowIndex, columnIndex);
			return foundBody;
		}

		case 'list': {
			const foundValue = findExpressionInExpressions(expression.values, rowIndex, columnIndex);
			return foundValue;
		}

		case 'name': {
			// TODO check name range, source, typeguard, fallback
			return expression;
		}

		case 'number':
			return undefined;

		case 'reference':
			// TODO find expression name aus dem array names
			return expression;

		case 'string': {
			const values = expression.values.filter((value): value is ValueExpression => value.type !== 'stringToken');
			const foundValue = findExpressionInExpressions(values, rowIndex, columnIndex);
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

//#endregion helper