import { fork } from 'child_process';
import { existsSync, readdirSync, readFileSync, statSync } from 'fs';
import { join, resolve } from 'path';
import { pathToFileURL } from 'url';

/**
 * Minimaler LSP-Client über Node-IPC, gemeinsam genutzt von bench.mjs und snapshot.mjs.
 * Nur so viel Protokoll wie diese beiden brauchen.
 */

export const serverPath = resolve(import.meta.dirname, '../out/server.js');
// Der Server überspringt größere Dateien, sie würden nie Diagnostics senden
export const maxFileSize = 100000;

const requestTimeout = 30000;

export function startServer() {
	const child = fork(serverPath, ['--node-ipc'], { execArgv: [] });
	const pendingRequests = new Map();
	/** uri -> resolve, wird von publishDiagnostics bedient */
	const pendingDiagnostics = new Map();
	let nextId = 1;

	child.on('message', (message) => {
		if (message.id !== undefined && pendingRequests.has(message.id)) {
			const pending = pendingRequests.get(message.id);
			pendingRequests.delete(message.id);
			if (message.error) {
				pending.reject(new Error(message.error.message));
			}
			else {
				pending.resolve(message.result);
			}
			return;
		}
		if (message.method === 'textDocument/publishDiagnostics') {
			const uri = message.params.uri;
			const resolveDiagnostics = pendingDiagnostics.get(uri);
			if (resolveDiagnostics) {
				pendingDiagnostics.delete(uri);
				resolveDiagnostics(message.params.diagnostics);
			}
		}
	});

	function notify(method, params) {
		child.send({ jsonrpc: '2.0', method: method, params: params });
	}

	function request(method, params) {
		const id = nextId++;
		return new Promise((resolveRequest, rejectRequest) => {
			const timeout = setTimeout(() => {
				pendingRequests.delete(id);
				rejectRequest(new Error(`timeout bei ${method}`));
			}, requestTimeout);
			pendingRequests.set(id, {
				resolve: (result) => {
					clearTimeout(timeout);
					resolveRequest(result);
				},
				reject: (error) => {
					clearTimeout(timeout);
					rejectRequest(error);
				},
			});
			child.send({ jsonrpc: '2.0', id: id, method: method, params: params });
		});
	}

	/** muss vor der auslösenden Notification aufgerufen werden, sonst geht die Antwort verloren */
	function waitForDiagnostics(uri) {
		return new Promise((resolveDiagnostics, rejectDiagnostics) => {
			const timeout = setTimeout(() => {
				pendingDiagnostics.delete(uri);
				rejectDiagnostics(new Error(`timeout bei diagnostics für ${uri}`));
			}, requestTimeout);
			pendingDiagnostics.set(uri, (diagnostics) => {
				clearTimeout(timeout);
				resolveDiagnostics(diagnostics);
			});
		});
	}

	return {
		notify: notify,
		request: request,
		waitForDiagnostics: waitForDiagnostics,
		stop: () => child.kill(),
	};
}

export async function initialize(client, rootPath) {
	const result = await client.request('initialize', {
		processId: process.pid,
		rootUri: pathToFileURL(rootPath).href,
		capabilities: {
			textDocument: {
				completion: { completionItem: { snippetSupport: false } },
				hover: { contentFormat: ['markdown', 'plaintext'] },
				publishDiagnostics: { relatedInformation: true },
				semanticTokens: {
					requests: { full: true },
					tokenTypes: [],
					tokenModifiers: [],
					formats: ['relative'],
				},
				synchronization: { dynamicRegistration: false },
			},
		},
		workspaceFolders: [{ uri: pathToFileURL(rootPath).href, name: 'jul' }],
	});
	client.notify('initialized', {});
	return result;
}

/** öffnet die Datei und wartet, bis der Server sie verarbeitet hat */
export async function openDocument(client, filePath) {
	const text = readFileSync(filePath, { encoding: 'utf8' });
	const uri = pathToFileURL(filePath).href;
	const diagnosticsPromise = client.waitForDiagnostics(uri);
	client.notify('textDocument/didOpen', {
		textDocument: { uri: uri, languageId: 'jul', version: 1, text: text },
	});
	const diagnostics = await diagnosticsPromise;
	return { uri: uri, text: text, diagnostics: diagnostics };
}

export function findJulFiles(target) {
	if (!existsSync(target)) {
		return [];
	}
	if (!statSync(target).isDirectory()) {
		return target.endsWith('.jul') ? [target] : [];
	}
	return readdirSync(target).sort().flatMap(entry => {
		if (entry === 'out' || entry === 'node_modules' || entry === '.git') {
			return [];
		}
		return findJulFiles(join(target, entry));
	});
}

/** verteilt Positionen über alle nicht leeren Zeilen, jeweils in der Mitte des Zeileninhalts */
export function collectPositions(rows, maxPositionCount) {
	const positions = [];
	rows.forEach((row, rowIndex) => {
		const trimmed = row.trim();
		if (!trimmed || trimmed.startsWith('#')) {
			return;
		}
		positions.push({ line: rowIndex, character: Math.floor(row.length / 2) });
	});
	const step = Math.max(1, Math.ceil(positions.length / maxPositionCount));
	return positions.filter((_position, index) => index % step === 0);
}
