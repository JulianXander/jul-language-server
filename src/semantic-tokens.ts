import { SemanticTokens, SemanticTokensBuilder } from 'vscode-languageserver';
import { Positioned } from 'jul-compiler/out/compiler-errors.js';
import {
	CompileTimeFunctionType,
	CompileTimeType,
	forEachChild,
	ParsedFile,
	PositionedExpression,
	SymbolDefinition,
	SymbolTable,
	TypeInfo,
} from 'jul-compiler/out/syntax-tree.js';
import { findSymbolInScopesWithBuiltIns } from 'jul-compiler/out/checker/checker.js';
import { isFunctionType, isTypeOfType } from 'jul-compiler/out/checker/type-algebra.js';
import { pushScope } from './symbol-lookup.js';

// Die Grammatik rät den Bezeichnertyp an der Schreibweise. Der checker weiß ihn - Werte und Typen
// teilen in JUL denselben Namensraum, dort rät die Grammatik zwangsläufig falsch.
const semanticTokenTypes = ['type', 'typeConstructor', 'function', 'parameter', 'variable', 'property'] as const;
const semanticTokenModifiers = ['declaration', 'defaultLibrary', 'readonly', 'stream', 'impure'] as const;
export const semanticTokenLegend = {
	tokenTypes: [...semanticTokenTypes],
	tokenModifiers: [...semanticTokenModifiers],
};
type SemanticTokenType = typeof semanticTokenTypes[number];
type SemanticTokenModifier = typeof semanticTokenModifiers[number];

interface SemanticTokenInfo {
	line: number;
	character: number;
	length: number;
	tokenType: number;
	tokenModifiers: number;
}

/**
 * Die Semantic Tokens einer geprüften Datei, kodiert nach semanticTokenLegend.
 * Ohne checked (Datei über maxFileSize oder nicht geladen) gibt es keine.
 */
export function getSemanticTokens(parsedFile: ParsedFile | undefined): SemanticTokens {
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
}

/**
 * Funktionen mit Sonderverhalten, die nur direkt aufgerufen werden dürfen und deshalb wie ein
 * Keyword gefärbt werden, über die Grammatik statt über Semantic Tokens.
 */
const keywordFunctionNames = ['import', 'test'];

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
			// import und test bekommen ebenfalls keinen Semantic Token: beide sind nur als direkter
			// Aufruf erlaubt (JUL3140, JUL2704), sollen also wie ein Keyword gefärbt werden
			// (Grammatik-Scopes keyword.control.import.jul/keyword.control.test.jul), nicht wie eine
			// eingebaute Funktion/Variable.
			if (keywordFunctionNames.includes(expression.name.name)) {
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
			// Die Deklarationen von import/test/true/false in core-lib.jul bekommen ebenfalls keinen
			// Semantic Token, aus demselben Grund wie an der jeweiligen Referenzstelle oben.
			if (keywordFunctionNames.includes(expression.name.name)) {
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
			if (expression.source) {
				// Alias (enabled = value): source ist der Parameter, name nur die lokale Konstante.
				pushSemanticToken(tokens, expression.source, 'parameter', []);
				pushSemanticToken(
					tokens,
					expression.name,
					getSemanticTokenType(expression.typeInfo, undefined),
					['declaration', ...getSemanticTokenModifiers(expression.typeInfo, false)]);
				return;
			}
			pushSemanticToken(
				tokens,
				expression.name,
				isTypeValue(expression.typeInfo?.type) ? 'type' : 'parameter',
				['declaration']);
			return;
		case 'destructuringField': {
			// Der Typ steht am Symbol, nicht am Feld selbst.
			const symbol = findSymbolInScopes(expression.name.name, scopes);
			pushSemanticToken(
				tokens,
				expression.name,
				getSemanticTokenType(symbol?.typeInfo, symbol),
				['declaration', ...getSemanticTokenModifiers(symbol?.typeInfo, false)]);
			if (expression.source) {
				pushSemanticToken(tokens, expression.source, 'property', []);
			}
			return;
		}
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
	// Mit Alias (enabled = value) ist der Name nur eine lokale Konstante, kein Parameter.
	if (symbol?.definition?.type === 'parameter' && !symbol.definition.source) {
		// Ein Parameter vom Typ Type (T: Type) steht für einen Typ und wird wie einer gefärbt.
		return isTypeValue(symbol.definition.typeInfo?.type)
			? 'type'
			: 'parameter';
	}
	// Der unaufgelöste Typ genügt: gefragt ist die Art des Bezeichners, nicht sein Inhalt.
	// resolvePlaceholders pro Referenz kostet mehr als der ganze restliche Durchlauf.
	const type = getFunctionValueType(typeInfo) ?? typeInfo?.type;
	if (isTypeValue(type)) {
		return 'type';
	}
	if (isFunctionType(type)) {
		// Wie List oder Or: eine Funktion, die sicher einen Typ liefert. Bei Any bleibt es function.
		return isTypeValue(type.ReturnType)
			? 'typeConstructor'
			: 'function';
	}
	return 'variable';
}

/**
 * Der Wert ist selbst ein Typ, kein Wert dieses Typs. Ein Tuple-Literal, dessen Elemente alle
 * Typen sind (auch verschachtelt, z.B. [[Cell Cell] [Cell Cell]]), ist ein Tuple-Typ.
 */
function isTypeValue(type: CompileTimeType | undefined): boolean {
	if (type?.julType === 'tuple') {
		return type.ElementTypes.length > 0 && type.ElementTypes.every(isTypeValue);
	}
	return isTypeOfType(type) || type?.julType === 'type';
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
	// Die Purity steht sonst nur am Pfeil ~> im Hover. Mit eigener Farbe ist ein Seiteneffekt an
	// jedem Aufruf zu sehen. Nur das sichere impure: pureIfArgsPure hängt vom einzelnen Aufruf ab.
	if (getFunctionValueType(typeInfo)?.purity === 'impure') {
		modifiers.push('impure');
	}
	return modifiers;
}

/**
 * Der Funktionstyp eines Bezeichners, dessen Wert eine Funktion ist. Eine Referenz auf ein
 * Funktionsliteral kann als TypeOf(Funktion) ankommen - das ist der Funktionswert selbst, kein Typ.
 */
function getFunctionValueType(typeInfo: TypeInfo | undefined): CompileTimeFunctionType | undefined {
	const type = typeInfo?.type;
	if (isFunctionType(type)) {
		return type;
	}
	if (isTypeOfType(type) && isFunctionType(type.value)) {
		return type.value;
	}
	return undefined;
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
