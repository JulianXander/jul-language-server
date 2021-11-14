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
import { AbstractSyntaxTree, Expression, PositionedExpression } from '../../jul-compiler/src/abstract-syntax-tree';

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
	const foundExpression = findExpressionByPosition(parsed, hoverParams.position.line, hoverParams.position.character);
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

function findExpressionByPosition(
	ast: AbstractSyntaxTree,
	rowIndex: number,
	columnIndex: number,
): Expression | undefined {
	return ast.parsed && findExpressionByPosition2(ast.parsed, rowIndex, columnIndex);
}

function findExpressionByPosition2(
	expressions: Expression[],
	rowIndex: number,
	columnIndex: number,
): Expression | undefined {
	const foundOuter = expressions.find(expression => {
		return isPositionInRange(rowIndex, columnIndex, expression);
	});
	if (!foundOuter) {
		return undefined;
	}
	const foundInner = findInnerExpressionByPosition(foundOuter, rowIndex, columnIndex);
	return foundInner;
}

function findInnerExpressionByPosition(
	expression: Expression,
	rowIndex: number,
	columnIndex: number,
): Expression | undefined {
	switch (expression.type) {
		case 'definition': {
			// TODO check name range
			// return expression.name
			const foundInner = findInnerExpressionByPosition(expression.value, rowIndex, columnIndex);
			return foundInner;
		}

		case 'reference':
			// TODO find expression name aus dem array names
			return expression;

		default: {
			// TODO
			return undefined;
			// const assertNever: never = expression.type;
			// throw new Error(`Unexpected expression.type: ${assertNever}`);
		}
	}
}

function isPositionInRange(
	rowIndex: number,
	columnIndex: number,
	range: PositionedExpression,
): boolean {
	return (range.startRowIndex < rowIndex
		|| (range.startRowIndex === rowIndex && range.startColumnIndex <= columnIndex))
		&& (range.endRowIndex > rowIndex
			|| (range.endRowIndex === rowIndex && range.endColumnIndex >= columnIndex));
}

//#endregion helper