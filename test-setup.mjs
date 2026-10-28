// Die Zugriffe auf das CheckResult prüfen, ob der Knoten zur Datei des Ergebnisses gehört (siehe
// jul-compiler/src/checker/check-result.ts). Vor dem ersten Import des Checkers, der die core-lib
// beim Laden prüft.
process.env.JUL_VERIFY_NODE_OWNERSHIP = '1';
