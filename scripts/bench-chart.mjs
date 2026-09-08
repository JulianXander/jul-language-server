import { existsSync, readFileSync, writeFileSync } from 'fs';
import { hostname } from 'os';
import { resolve } from 'path';

/**
 * Zeichnet den Verlauf aus bench-log.tsv als SVG, ein Diagramm je Label.
 * Getrennte y-Achsen statt einer gemeinsamen: die Labels liegen zwei Größenordnungen
 * auseinander, in einem Bild wären die schnellen Werte eine flache Linie auf der Nulllinie.
 * Aufruf: node scripts/bench-chart.mjs [--log pfad] [--target name] [--out pfad]
 */

const cellWidth = 340;
const cellHeight = 190;
const columnCount = 2;
const padding = { top: 40, right: 16, bottom: 46, left: 52 };

function parseArgs(argv) {
	const getValue = name => {
		const index = argv.indexOf(name);
		return index >= 0
			? argv[index + 1]
			: undefined;
	};
	return {
		logPath: resolve(getValue('--log') ?? resolve(import.meta.dirname, 'bench-log.tsv')),
		target: getValue('--target'),
		machine: getValue('--machine') ?? hostname(),
		outPath: getValue('--out'),
	};
}

function readEntries(logPath) {
	return readFileSync(logPath, { encoding: 'utf8' }).split('\n').flatMap(row => {
		if (!row || row.startsWith('#')) {
			return [];
		}
		const [timestamp, commit, deps, machine, target, label, count, median] = row.split('\t');
		return [{
			timestamp: timestamp,
			commit: commit,
			deps: deps,
			machine: machine,
			target: target,
			label: label,
			count: Number(count),
			median: Number(median),
		}];
	});
}

function escapeText(text) {
	return text
		.replace(/&/g, '&amp;')
		.replace(/</g, '&lt;')
		.replace(/>/g, '&gt;');
}

/** ein Diagramm, x nach Position im Protokoll, nicht nach Datum: Messungen sind ungleich verteilt */
function renderCell(label, entries, offsetX, offsetY) {
	const plotWidth = cellWidth - padding.left - padding.right;
	const plotHeight = cellHeight - padding.top - padding.bottom;
	const maxValue = Math.max(...entries.map(entry => entry.median)) * 1.15 || 1;
	const getX = index => entries.length > 1
		? padding.left + (index / (entries.length - 1)) * plotWidth
		: padding.left + plotWidth / 2;
	const getY = value => padding.top + plotHeight - (value / maxValue) * plotHeight;
	const points = entries.map((entry, index) => `${getX(index).toFixed(1)},${getY(entry.median).toFixed(1)}`);
	const dots = entries.map((entry, index) =>
		`<circle cx="${getX(index).toFixed(1)}" cy="${getY(entry.median).toFixed(1)}" r="2.5" fill="#2b7bb9">`
		+ `<title>${escapeText(`${entry.timestamp}\n${entry.commit}${entry.deps === '-' ? '' : `, ${entry.deps}`}\n${entry.median.toFixed(2)} ms`)}</title>`
		+ '</circle>').join('');
	const gridLines = [0, 0.5, 1].map(fraction => {
		const value = maxValue * fraction;
		const y = getY(value).toFixed(1);
		return `<line x1="${padding.left}" y1="${y}" x2="${padding.left + plotWidth}" y2="${y}" stroke="#e0e0e0"/>`
			+ `<text x="${padding.left - 6}" y="${y}" text-anchor="end" dominant-baseline="middle" font-size="9" fill="#888">${value.toFixed(2)}</text>`;
	}).join('');
	const first = entries[0];
	const last = entries[entries.length - 1];
	const trend = entries.length > 1 && first.median
		? ` (${last.median >= first.median ? '+' : ''}${(((last.median - first.median) / first.median) * 100).toFixed(0)}% seit ${first.timestamp.slice(0, 10)})`
		: '';
	const axisY = padding.top + plotHeight;
	const dateLabels = entries.length > 1
		? `<text x="${padding.left}" y="${axisY + 14}" font-size="9" fill="#888">${first.timestamp.slice(0, 10)}</text>`
		+ `<text x="${padding.left + plotWidth}" y="${axisY + 14}" text-anchor="end" font-size="9" fill="#888">${last.timestamp.slice(0, 10)}</text>`
		: '';
	return `<g transform="translate(${offsetX} ${offsetY})">`
		+ `<rect x="6" y="4" width="${cellWidth - 12}" height="${cellHeight - 12}" fill="none" stroke="#f0f0f0"/>`
		+ `<text x="${padding.left}" y="20" font-size="11" font-weight="bold" fill="#333">${escapeText(label)}</text>`
		+ `<text x="${padding.left}" y="33" font-size="9" fill="#888">${escapeText(`${last.median.toFixed(2)} ms${trend}`)}</text>`
		+ gridLines
		+ dateLabels
		+ `<polyline fill="none" stroke="#2b7bb9" stroke-width="1.5" points="${points.join(' ')}"/>`
		+ dots
		+ '</g>';
}

const { logPath, target, machine, outPath } = parseArgs(process.argv.slice(2));
if (!existsSync(logPath)) {
	console.error(`${logPath} fehlt, zuerst messen: npm run bench -- --save --note "grund"`);
	process.exitCode = 1;
}
else {
	const allEntries = readEntries(logPath).filter(entry => entry.machine === machine);
	// ohne Ziel wird das mit den meisten Messungen gewählt, das ist die Reihe, die etwas hergibt
	const targetCounts = {};
	allEntries.forEach(entry => {
		targetCounts[entry.target] = (targetCounts[entry.target] ?? 0) + 1;
	});
	const chosenTarget = target ?? Object.keys(targetCounts).sort((a, b) => targetCounts[b] - targetCounts[a])[0];
	const entries = allEntries.filter(entry => entry.target === chosenTarget);
	if (!entries.length) {
		console.error(`keine Einträge für ${chosenTarget ?? '(kein Ziel)'} auf ${machine} in ${logPath}`);
		process.exitCode = 1;
	}
	else {
		const labels = [...new Set(entries.map(entry => entry.label))];
		const rowCount = Math.ceil(labels.length / columnCount);
		const width = columnCount * cellWidth;
		const height = rowCount * cellHeight + 40;
		const cells = labels.map((label, index) => renderCell(
			label,
			entries.filter(entry => entry.label === label),
			(index % columnCount) * cellWidth,
			40 + Math.floor(index / columnCount) * cellHeight,
		)).join('');
		const measurementCount = entries.length / labels.length;
		const svg = `<svg xmlns="http://www.w3.org/2000/svg" width="${width}" height="${height}" font-family="sans-serif">`
			+ `<rect width="${width}" height="${height}" fill="#fff"/>`
			+ `<text x="${padding.left}" y="22" font-size="13" font-weight="bold" fill="#333">${escapeText(`${chosenTarget} auf ${machine}`)}</text>`
			+ `<text x="${width - 12}" y="22" text-anchor="end" font-size="10" fill="#888">${Math.round(measurementCount)} Messungen, Median in ms</text>`
			+ cells
			+ '</svg>';
		const chartPath = outPath
			? resolve(outPath)
			: logPath.replace(/\.tsv$/, '.svg');
		writeFileSync(chartPath, svg);
		console.log(`${chartPath}: ${labels.length} Diagramm${labels.length === 1 ? '' : 'e'},`
			+ ` ${Math.round(measurementCount)} Messung${Math.round(measurementCount) === 1 ? '' : 'en'}`);
	}
}
