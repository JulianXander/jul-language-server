import { expect } from 'chai';
import { createChangeDebouncer, Scheduler } from './change-debouncer.js';

/** Zeit läuft nur über advance, Timer feuern in der Reihenfolge ihrer Fälligkeit */
function createFakeScheduler() {
	let currentTime = 0;
	let nextHandle = 1;
	const timers = new Map<number, { dueAt: number, callback: () => void }>();
	const scheduler: Scheduler = {
		setTimeout: (callback, delayMs) => {
			const handle = nextHandle++;
			timers.set(handle, { dueAt: currentTime + delayMs, callback: callback });
			return handle;
		},
		clearTimeout: handle => {
			timers.delete(handle as number);
		},
		now: () => currentTime,
	};
	function advance(ms: number): void {
		const target = currentTime + ms;
		while (true) {
			const next = [...timers.entries()]
				.filter(([, timer]) => timer.dueAt <= target)
				.sort((a, b) => a[1].dueAt - b[1].dueAt)[0];
			if (!next) {
				break;
			}
			timers.delete(next[0]);
			currentTime = next[1].dueAt;
			next[1].callback();
		}
		currentTime = target;
	}
	return { scheduler, advance };
}

function setup(delayMs = 100, maxDelayMs = 500) {
	const { scheduler, advance } = createFakeScheduler();
	const processed: string[] = [];
	const debouncer = createChangeDebouncer<string>(
		(key, value) => processed.push(`${key}:${value}`),
		delayMs,
		maxDelayMs,
		scheduler,
	);
	return { debouncer, processed, advance };
}

describe('createChangeDebouncer', () => {
	it('verarbeitet die erste Änderung eines Schlüssels sofort', () => {
		const { debouncer, processed } = setup();
		debouncer.schedule('a', '1');
		expect(processed).to.deep.equal(['a:1']);
	});

	it('verarbeitet eine einzelne Änderung nach der Wartezeit', () => {
		const { debouncer, processed, advance } = setup();
		debouncer.schedule('a', 'open');
		debouncer.schedule('a', '1');
		advance(99);
		expect(processed).to.deep.equal(['a:open']);
		advance(1);
		expect(processed).to.deep.equal(['a:open', 'a:1']);
	});

	it('fasst Änderungen mit kurzem Abstand zum letzten Stand zusammen', () => {
		const { debouncer, processed, advance } = setup();
		debouncer.schedule('a', 'open');
		debouncer.schedule('a', '1');
		advance(40);
		debouncer.schedule('a', '2');
		advance(40);
		debouncer.schedule('a', '3');
		advance(100);
		expect(processed).to.deep.equal(['a:open', 'a:3']);
	});

	it('verarbeitet beim Dauertippen spätestens nach der maximalen Wartezeit', () => {
		const { debouncer, processed, advance } = setup(100, 500);
		debouncer.schedule('a', 'open');
		// 3 s lang alle 40 ms ein Anschlag, nie 100 ms Pause
		for (let keystroke = 1; keystroke <= 75; keystroke++) {
			debouncer.schedule('a', String(keystroke));
			advance(40);
		}
		expect(processed.length).to.be.greaterThanOrEqual(6);
		expect(processed.length).to.be.lessThanOrEqual(8);
		// Zwischenstände kommen regelmäßig, nicht erst am Ende
		const intermediate = processed.slice(1, -1);
		expect(intermediate.length).to.be.greaterThan(0);
	});

	it('hält beim Dauertippen den Abstand der Zwischenstände unter maximaler Wartezeit plus einem Anschlag', () => {
		const { scheduler, advance } = createFakeScheduler();
		const processedAt: number[] = [];
		const debouncer = createChangeDebouncer<number>(
			() => processedAt.push(scheduler.now()),
			100,
			500,
			scheduler,
		);
		debouncer.schedule('a', 0);
		for (let keystroke = 1; keystroke <= 75; keystroke++) {
			debouncer.schedule('a', keystroke);
			advance(40);
		}
		const gaps = processedAt.slice(1).map((time, index) => time - processedAt[index]!);
		// Die Obergrenze gilt ab der ersten unverarbeiteten Änderung, die kommt bis zu einen Anschlag nach dem letzten Zwischenstand
		gaps.slice(1).forEach(gap => expect(gap).to.be.at.most(500 + 40));
	});

	it('flush verarbeitet ausstehende Änderungen sofort und genau einmal', () => {
		const { debouncer, processed, advance } = setup();
		debouncer.schedule('a', 'open');
		debouncer.schedule('a', '1');
		debouncer.flush('a');
		expect(processed).to.deep.equal(['a:open', 'a:1']);
		advance(1000);
		expect(processed).to.deep.equal(['a:open', 'a:1']);
	});

	it('flushAll verarbeitet alle Schlüssel', () => {
		const { debouncer, processed } = setup();
		debouncer.schedule('a', 'open');
		debouncer.schedule('b', 'open');
		debouncer.schedule('a', '1');
		debouncer.schedule('b', '2');
		debouncer.flushAll();
		expect(processed).to.deep.equal(['a:open', 'b:open', 'a:1', 'b:2']);
	});

	it('flush ohne Ausstehendes tut nichts', () => {
		const { debouncer, processed } = setup();
		debouncer.flush('a');
		debouncer.flushAll();
		expect(processed).to.deep.equal([]);
	});

	it('behandelt Schlüssel unabhängig voneinander', () => {
		const { debouncer, processed, advance } = setup();
		debouncer.schedule('a', 'open');
		debouncer.schedule('b', 'open');
		debouncer.schedule('a', '1');
		advance(60);
		debouncer.schedule('b', '1');
		advance(40);
		expect(processed).to.deep.equal(['a:open', 'b:open', 'a:1']);
		advance(60);
		expect(processed).to.deep.equal(['a:open', 'b:open', 'a:1', 'b:1']);
	});

	it('forget verwirft Ausstehendes, die nächste Änderung läuft wieder sofort', () => {
		const { debouncer, processed, advance } = setup();
		debouncer.schedule('a', 'open');
		debouncer.schedule('a', '1');
		debouncer.forget('a');
		advance(1000);
		expect(processed).to.deep.equal(['a:open']);
		debouncer.schedule('a', 'reopen');
		expect(processed).to.deep.equal(['a:open', 'a:reopen']);
	});
});
