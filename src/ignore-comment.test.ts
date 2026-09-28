import { expect } from 'chai';
import { ErrorCode } from 'jul-compiler/out/compiler-errors.js';
import { Diagnostic, DiagnosticSeverity } from 'vscode-languageserver';
import { getIgnoreCommentCodeActions } from './ignore-comment.js';

const uri = 'file:///main.jul';

function diagnostic(code: number, line: number, character: number, severity: DiagnosticSeverity): Diagnostic {
	return {
		range: {
			start: { line, character },
			end: { line, character: character + 1 },
		},
		code: code,
		message: '',
		severity: severity,
	};
}

describe('getIgnoreCommentCodeActions', () => {
	it('inserts the comment above the line with the indentation of that line', () => {
		const lines = ['f = () =>', '\tseconds$ = timer$(1f)', '\tseconds$'];
		const actions = getIgnoreCommentCodeActions(uri, lines, [
			diagnostic(ErrorCode.streamNeverCompleted, 1, 12, DiagnosticSeverity.Warning),
		]);
		expect(actions.map(action => action.title)).to.deep.equal(['Suppress JUL2800 for this line']);
		expect(actions[0]!.edit!.changes![uri]).to.deep.equal([{
			range: {
				start: { line: 1, character: 0 },
				end: { line: 1, character: 0 },
			},
			newText: '\t# jul-ignore JUL2800\n',
		}]);
	});
	// Fehler lassen sich nicht abschalten.
	it('offers nothing for errors', () => {
		const actions = getIgnoreCommentCodeActions(uri, ['a'], [
			diagnostic(ErrorCode.notDefined, 0, 0, DiagnosticSeverity.Error),
		]);
		expect(actions).to.deep.equal([]);
	});
	// Eine Warnung über den Kommentar selbst behebt man am Kommentar, nicht mit einem weiteren.
	it('offers nothing for warnings about jul-ignore itself', () => {
		const actions = getIgnoreCommentCodeActions(uri, ['# jul-ignore JUL2800', 'x = 1'], [
			diagnostic(ErrorCode.unusedIgnoreComment, 0, 0, DiagnosticSeverity.Warning),
		]);
		expect(actions).to.deep.equal([]);
	});
});
