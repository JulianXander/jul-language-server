/**
 * Fasst schnell aufeinanderfolgende Änderungen je Schlüssel (Dokument) zusammen.
 *
 * Parsen und Checken kostet bei großen Dateien über 100 ms und läuft im einzigen Thread des Servers.
 * Würde jeder Tastendruck sofort verarbeitet, staut sich beim schnellen Tippen eine Warteschlange auf
 * und die Diagnostics hinken sekundenlang hinterher.
 *
 * - Eine Änderung wird erst verarbeitet, wenn delayMs lang keine weitere kam.
 * - Beim Dauertippen würde das ewig dauern, deshalb wird spätestens maxDelayMs nach der ersten
 *   unverarbeiteten Änderung trotzdem verarbeitet.
 * - Die erste Änderung eines Schlüssels (Öffnen der Datei) läuft sofort.
 * - flush verarbeitet ausstehende Änderungen sofort, damit Anfragen nie einen veralteten Stand sehen.
 */

export type Scheduler = {
	setTimeout: (callback: () => void, delayMs: number) => unknown;
	clearTimeout: (handle: unknown) => void;
	now: () => number;
};

export const realScheduler: Scheduler = {
	setTimeout: (callback, delayMs) => setTimeout(callback, delayMs),
	clearTimeout: handle => clearTimeout(handle as NodeJS.Timeout),
	now: () => performance.now(),
};

type Pending<T> = {
	value: T;
	timer: unknown;
	firstScheduledAt: number;
};

export type ChangeDebouncer<T> = {
	/** verarbeitet sofort (erste Änderung dieses Schlüssels) oder merkt sich value für später */
	schedule: (key: string, value: T) => void;
	flush: (key: string) => void;
	flushAll: () => void;
	/** verwirft Ausstehendes und vergisst den Schlüssel, die nächste Änderung läuft wieder sofort */
	forget: (key: string) => void;
};

export function createChangeDebouncer<T>(
	process: (key: string, value: T) => void,
	delayMs: number,
	maxDelayMs: number,
	scheduler: Scheduler = realScheduler,
): ChangeDebouncer<T> {
	const pendingChanges = new Map<string, Pending<T>>();
	const seenKeys = new Set<string>();

	function flush(key: string): void {
		const pending = pendingChanges.get(key);
		if (!pending) {
			return;
		}
		scheduler.clearTimeout(pending.timer);
		pendingChanges.delete(key);
		process(key, pending.value);
	}

	return {
		schedule: (key, value) => {
			if (!seenKeys.has(key)) {
				seenKeys.add(key);
				process(key, value);
				return;
			}
			const now = scheduler.now();
			const existing = pendingChanges.get(key);
			if (existing) {
				scheduler.clearTimeout(existing.timer);
			}
			const firstScheduledAt = existing?.firstScheduledAt ?? now;
			const remainingMax = Math.max(0, firstScheduledAt + maxDelayMs - now);
			pendingChanges.set(key, {
				value: value,
				firstScheduledAt: firstScheduledAt,
				timer: scheduler.setTimeout(() => flush(key), Math.min(delayMs, remainingMax)),
			});
		},
		flush: flush,
		flushAll: () => [...pendingChanges.keys()].forEach(flush),
		forget: key => {
			const pending = pendingChanges.get(key);
			if (pending) {
				scheduler.clearTimeout(pending.timer);
				pendingChanges.delete(key);
			}
			seenKeys.delete(key);
		},
	};
}
