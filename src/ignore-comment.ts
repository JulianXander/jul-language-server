import { ErrorCode, errorInfos } from 'jul-compiler/out/compiler-errors.js';
import { CodeAction, CodeActionKind, Diagnostic, TextEdit } from 'vscode-languageserver';

/**
 * Quick Fix "Warnung hier abschalten": eine Zeile `#ignore JUL<nr>` direkt über der Zeile,
 * in der die Warnung beginnt, gleich eingerückt. Der Kommentar gilt für genau diese Zeile.
 */
export function getIgnoreCommentCodeActions(
	documentUri: string,
	lines: readonly string[],
	diagnostics: readonly Diagnostic[],
): CodeAction[] {
	return diagnostics.flatMap(diagnostic => {
		const code = diagnostic.code;
		if (typeof code !== 'number' || !isSuppressible(code)) {
			return [];
		}
		const rowIndex = diagnostic.range.start.line;
		return [{
			title: `Suppress JUL${code} for this line`,
			kind: CodeActionKind.QuickFix,
			diagnostics: [diagnostic],
			edit: {
				changes: {
					[documentUri]: [createIgnoreCommentEdit(lines[rowIndex] ?? '', rowIndex, code)],
				},
			},
		}];
	});
}

export function createIgnoreCommentEdit(line: string, rowIndex: number, code: number): TextEdit {
	const indent = /^\t*/.exec(line)![0];
	return {
		range: {
			start: { line: rowIndex, character: 0 },
			end: { line: rowIndex, character: 0 },
		},
		newText: `${indent}#ignore JUL${code}\n`,
	};
}

/**
 * Nur Warnungen, und nicht die Warnungen über #ignore selbst: die behebt man am Kommentar.
 */
function isSuppressible(code: number): boolean {
	if (code === ErrorCode.unusedIgnoreComment
		|| code === ErrorCode.invalidIgnoreComment) {
		return false;
	}
	return errorInfos[code as ErrorCode]?.severity === 'warning';
}
