// Targets the duplicate-detection branch inside `validate builtin set names`
// (spectec/document/js-api/index.bs:1861-1864: "If |builtinSetNames| contains
// any duplicates, return false."), compiled via ExpandHasDuplicatesPass's
// generic duplicate-scan loop. Found via esmeta.wji.WjiCoverage's annotated
// IR: the function itself is visited 1402 times by the official corpus (every
// `{ builtins: [...] }` compile/validate call reaches it), but every one of
// those calls passes at most a single builtin-set name (only "js-string"
// exists today -- spectec/test/js-api/js-string/*.any.js's own usage is
// always `{ builtins: ["js-string"] }`), so the duplicate-scan loop's body
// never actually runs: its outer index starts at 1, and `1 < length` is false
// for any list of length <= 1.
//
// Node 26.12.1 can't confirm this: V8 returns `true` even for
// `{ builtins: ["js-string", "js-string"] }`, never validating this option
// (docs/engine_bugs.md #7). Cross-checked with tests/wji/scripts/diff-engines
// instead: JSC passes.
function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

// magic + version only, no sections -- the smallest valid module, so nothing
// about its own contents (imports, in particular) can matter here: step
// 404/1863's duplicate check happens unconditionally before any per-import
// logic ever runs.
const emptyModule = new Uint8Array([0, 97, 115, 109, 1, 0, 0, 0]);

assertEq(WebAssembly.validate(emptyModule, {}), true, "no builtins option at all");
assertEq(
  WebAssembly.validate(emptyModule, { builtins: ["js-string"] }),
  true,
  "a single, non-duplicated builtin set name",
);
assertEq(
  WebAssembly.validate(emptyModule, { builtins: ["js-string", "js-string"] }),
  false,
  "a duplicated builtin set name must fail validation",
);

globalThis.__wjiOk = true;
