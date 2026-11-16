import { createRequire } from 'module';

// Die Zugriffe auf das CheckResult prüfen, ob der Knoten zur Datei des Ergebnisses gehört (siehe
// jul-compiler/src/checker/check-result.ts). Vor dem ersten Import des Checkers, der das Flag beim
// Laden liest.
process.env.JUL_VERIFY_NODE_OWNERSHIP = '1';

// Prüft die core-lib und setzt TypeScript, für alle Tests, die Code über createInMemoryHost laden.
// Über require statt import: tsx übersetzt die Testdateien nach CommonJS und lädt den Compiler dort
// als eigene Modulinstanz. Ein import erreichte nur die ESM-Instanz, die die Tests nicht sehen.
createRequire(import.meta.url)('jul-compiler/out/compiler/node-host.js');
