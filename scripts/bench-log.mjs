import { execFileSync } from 'child_process';
import { appendFileSync, existsSync, readFileSync, writeFileSync } from 'fs';
import { hostname } from 'os';

/**
 * Append-only Protokoll der Wall-Clock Messungen. Eine Zeile je Messwert, damit nicht nur der
 * letzte Schritt sichtbar ist, sondern der Verlauf: viele kleine Verschlechterungen fallen
 * einzeln unter jede Schwelle und summieren sich trotzdem.
 * Wall-Clock ist nur innerhalb derselben Maschine vergleichbar, deshalb steht sie in der Zeile
 * und Vergleiche überspringen fremde Einträge.
 * Bewusste Kopie aus jul-compiler/scripts, die Projekte sind eigene Repos.
 */

const logHeader = '# timestamp\tcommit\tdeps\tmachine\ttarget\tlabel\tn\tmedian\tp95\tmax\tnote';

export const noticeableDeviation = 0.2;
export const alarmingDeviation = 0.5;
/**
 * Unter dieser Dauer sagt eine prozentuale Abweichung nichts: 0.07 statt 0.10 ms sind -28%
 * und trotzdem derselbe Code. Solche Werte werden protokolliert, aber nicht bewertet.
 */
export const minComparableMedian = 0.5;

export function isComparable(values, previous) {
	return values.median >= minComparableMedian
		&& previous.median >= minComparableMedian;
}

/** Hash mit + wenn der Baum schmutzig ist: dann beschreibt der Hash den gemessenen Stand nicht */
export function getCommit(folder) {
	const options = { encoding: 'utf8', cwd: folder };
	try {
		const commit = execFileSync('git', ['rev-parse', '--short', 'HEAD'], options).trim();
		const dirty = execFileSync('git', ['status', '--porcelain'], options).trim();
		return dirty
			? `${commit}+`
			: commit;
	}
	catch {
		return '-';
	}
}

export function getMachine() {
	return hostname();
}

export function stats(durations) {
	const sorted = [...durations].sort((a, b) => a - b);
	return {
		count: durations.length,
		median: sorted[Math.floor(sorted.length / 2)],
		p95: sorted[Math.min(sorted.length - 1, Math.floor(sorted.length * 0.95))],
		max: sorted[sorted.length - 1],
	};
}

function readEntries(logPath) {
	if (!existsSync(logPath)) {
		return [];
	}
	return readFileSync(logPath, { encoding: 'utf8' }).split('\n').flatMap(row => {
		if (!row || row.startsWith('#')) {
			return [];
		}
		const [timestamp, commit, deps, machine, target, label, count, median, p95, max, note] = row.split('\t');
		return [{
			timestamp: timestamp,
			commit: commit,
			deps: deps,
			machine: machine,
			target: target,
			label: label,
			count: Number(count),
			median: Number(median),
			p95: Number(p95),
			max: Number(max),
			note: note ?? '',
		}];
	});
}

/** letzter Eintrag je Label für dieses Ziel auf dieser Maschine */
export function readPrevious(logPath, target, machine) {
	const previous = {};
	readEntries(logPath).forEach(entry => {
		if (entry.target === target && entry.machine === machine) {
			previous[entry.label] = entry;
		}
	});
	return Object.keys(previous).length
		? previous
		: undefined;
}

/**
 * Vergleich gegen die älteste Messung auf dieser Maschine: schleichende Verschlechterung liegt
 * gegenüber dem jeweils letzten Eintrag jedes Mal im Rauschen und wird erst über die Zeit sichtbar.
 */
export function getDrift(logPath, target, machine, label, values) {
	const first = readEntries(logPath).find(entry =>
		entry.target === target
		&& entry.machine === machine
		&& entry.label === label);
	if (!first || !isComparable(values, first)) {
		return undefined;
	}
	return {
		first: first,
		deviation: getDeviation(values, first),
	};
}

/** deps: Commits der gemessenen Abhängigkeiten, ohne die der eigene Commit die Messung nicht erklärt */
export function appendEntries(logPath, results, target, note, deps = '-') {
	if (!existsSync(logPath)) {
		writeFileSync(logPath, `${logHeader}\n`);
	}
	const timestamp = new Date().toISOString().slice(0, 19).replace('T', ' ');
	const commit = getCommit();
	const machine = getMachine();
	const rows = results.map(({ label, values }) => [
		timestamp,
		commit,
		sanitize(deps),
		machine,
		target,
		label,
		values.count,
		values.median.toFixed(2),
		values.p95.toFixed(2),
		values.max.toFixed(2),
		sanitize(note) || '-',
	].join('\t'));
	appendFileSync(logPath, `${rows.join('\n')}\n`);
}

export function getDeviation(values, previous) {
	return (values.median - previous.median) / previous.median;
}

/** das Format kennt kein Quoting, ein Tab oder Umbruch im Freitext würde die Spalten verschieben */
function sanitize(text) {
	return text.replace(/[\t\r\n]+/g, ' ').trim();
}

export function formatResult(label, values, previous) {
	const base = `  ${label.padEnd(24)} n=${String(values.count).padStart(5)}`
		+ `  median ${values.median.toFixed(2).padStart(8)} ms`
		+ `  p95 ${values.p95.toFixed(2).padStart(8)} ms`
		+ `  max ${values.max.toFixed(2).padStart(8)} ms`;
	if (!previous) {
		return base;
	}
	const deviation = getDeviation(values, previous);
	const sign = deviation >= 0 ? '+' : '';
	if (!isComparable(values, previous)) {
		return `${base}   vorher ${previous.median.toFixed(2)} ms (unter Messgrenze)`;
	}
	const marker = deviation >= alarmingDeviation
		? '  <== ALARM'
		: Math.abs(deviation) >= noticeableDeviation
			? (deviation > 0 ? '  <== langsamer' : '  <== schneller')
			: '';
	return `${base}   vorher ${previous.median.toFixed(2)} ms (${sign}${(deviation * 100).toFixed(0)}%)${marker}`;
}

export function parseArgs(argv) {
	const save = argv.includes('--save');
	const noteIndex = argv.indexOf('--note');
	const note = noteIndex >= 0
		? argv[noteIndex + 1] ?? ''
		: '';
	const targets = argv.filter((arg, index) =>
		arg !== '--save'
		&& (noteIndex < 0 || (index !== noteIndex && index !== noteIndex + 1)));
	return {
		save: save,
		note: note,
		targets: targets,
	};
}
