#!/usr/bin/env node
// ------------------------------------------------------------------------------
// Usage: node tests/wji/scripts/wji-generate-js-api-tests.js
//
// Regenerates tests/wji/js-api/generated/ from every *.any.js file under
// spectec/test/js-api/ (recursively): resolves each file's META script
// dependencies and writes a single self-contained fixture (shell-shim.js +
// testharness-lite.js + resolved deps + the test body + report-shim.js) to
// the same relative path under generated/. Mirrors scripts/wat2js's
// philosophy -- this is an authoring/sync-time convenience, not a build step;
// nothing verifies automatically that generated/ is still in sync with
// spectec/test/js-api, re-run this by hand after a spectec submodule bump
// touches that corpus.
//
// No category is excluded, even ones exercising newer/tentative wasm
// proposals (gc/, exception/, tag/, js-string/) -- earlier scoping those out
// on the assumption WJI mechanizes none of it turned out to be wrong (see
// tests/wji/manual/wasm-throw-propagation.js, which already exercises
// WebAssembly.Tag/Exception end to end), and per-file gaps surface just fine
// as knownFailing entries same as everything else.
// ------------------------------------------------------------------------------
"use strict";

const fs = require("fs");
const path = require("path");
const { repoRoot, jsApiRoot, readResolvedDeps } = require("./js-api-meta");

const wjiJsApiDir = path.join(repoRoot, "tests/wji/js-api");
const generatedDir = path.join(wjiJsApiDir, "generated");

function listAnyJsFiles(dir) {
  const out = [];
  for (const entry of fs.readdirSync(dir, { withFileTypes: true })) {
    const full = path.join(dir, entry.name);
    if (entry.isDirectory()) out.push(...listAnyJsFiles(full));
    else if (entry.name.endsWith(".any.js")) out.push(full);
  }
  return out.sort();
}

const testFiles = listAnyJsFiles(jsApiRoot);

const shellShim = fs.readFileSync(path.join(wjiJsApiDir, "shell-shim.js"), "utf8");
const testharnessLite = fs.readFileSync(path.join(wjiJsApiDir, "testharness-lite.js"), "utf8");
const skipKnownGaps = fs.readFileSync(path.join(wjiJsApiDir, "skip-known-gaps.js"), "utf8");
const reportShim = fs.readFileSync(path.join(wjiJsApiDir, "report-shim.js"), "utf8");
const dataViewPolyfill = fs.readFileSync(path.join(wjiJsApiDir, "dataview-polyfill.js"), "utf8");

// Known bugs in the vendored spectec/test/js-api corpus itself, applied to a
// file's own body text before assembly -- the test-source analogue of
// SpecPatch.scala's patches on spectec/document/js-api/index.bs (same
// rationale and same shape: applied unconditionally as a literal string
// replacement, relying on each `from` being distinctive enough in the corpus
// as a whole -- not scoped per file, since a from/to pair that's safe to
// `.replace()` globally needs no scoping, and one that isn't shouldn't be
// trusted with scoping alone either).
const testPatches = [
  // `for (argument of invalidValues) { ... }` (no `let`/`const`) assigns to
  // an undeclared bare identifier every iteration -- incidental to what the
  // test actually checks (that getArg/is throw for invalid argument types),
  // not something about sloppy vs strict mode, so this is a real corpus bug
  // worth fixing at the source rather than a known-gap to skip: sloppy mode
  // silently creates an implicit global for it, but ESMeta mainline hardcodes
  // strict mode, which throws `ReferenceError` instead. Occurs verbatim in
  // exactly two files (exception/{getArg,is}.tentative.any.js).
  ["for (argument of invalidValues) {", "for (let argument of invalidValues) {"],
];

// Per-file patches that neuter just the individual pieces too expensive to
// actually run -- unlike `testPatches` above (fixing a corpus bug so the test
// can run as-is), these are calls that will never finish under an AST-walking
// interpreter no matter how long they're given (or, for the OOM cases below,
// blow well past this repo's `-Xmx3g`), so this is a permanent, deliberate
// decision (see docs/out_of_scope.md for why each one is here), not a
// workaround for something fixable. Each entry uses whichever patch shape
// fits how its source actually reaches the expensive call:
//   - a call that's already a standalone top-level statement (`limits.any.js`
//     below): prefixing its own first line with `if (false) ` is enough
//     regardless of how many lines/args it spans -- `if (false) EXPR;` only
//     needs `EXPR;` to be one statement -- so this stays a small,
//     single-line-per-entry patch rather than wrapping the whole multi-line
//     call body.
//   - a call reached lazily, inside a `test()` callback that only runs when
//     that specific subtest is invoked (`instance/constructor.any.js`
//     below): filtering the array/loop that drives `test()`'s own name
//     argument is enough -- the callback (and the expensive call inside it)
//     then simply never gets invoked, the same "not run at all" outcome
//     `skip-known-gaps.js` gives its own matches, just scoped to one file
//     instead of global (these names collide with unrelated, actually-fine
//     tests in other files, so they can't go in that shared list).
//   - a call sitting inside an array *literal* that's itself built eagerly
//     (`instance/constructor-bad-imports.any.js` below): neither of the
//     above apply (an array element is an expression, not a statement, so
//     `if (false)` can't prefix just one entry) -- the one array element is
//     deleted outright instead, which is safe here since each array's own
//     consuming loop already derives its subtest name from the *rest* of
//     each entry, so removing one entry cleanly removes exactly the one
//     subtest built from it and nothing else.
const badImportsPatches = [
  [
    '    [new WebAssembly.Memory({"initial": 256}), "WebAssembly.Memory object (too large)"],\n',
    "",
  ],
  [
    '    [new WebAssembly.Table({"element": "anyfunc", "initial": 256}), "WebAssembly.Table object (too large)"],\n',
    "",
  ],
];

const perFilePatches = {
  // `limits.any.js` (docs/out_of_scope.md #3) -- the corpus's one
  // `// META: timeout=long` file. Every one of these calls runs
  // synchronously at top-level script execution, before any `test()`
  // callback even gets registered -- a title-based skip like
  // `skip-known-gaps.js` can't help (the expensive work isn't inside any
  // `test()` callback to filter out). Only the ones that actually build
  // something proportional to a huge count are neutered here; the small ones
  // (`function params`/`function returns`, capped at 1000; `memories`,
  // capped at 1; `function locals`/`function params+locals`, which pass
  // their huge count as a single `addLocals({i32_count: count})` argument --
  // `WasmModuleBuilder`'s own `getNumLocals`/`addLocals`
  // (spectec/test/js-api/wasm-module-builder.js) never loops over `count`
  // itself, so this is O(1) regardless of its magnitude) and the two
  // `testDynamicLimit` calls plus the final bare `test()` (which only pass a
  // huge *number* as a size bound for the engine's own validation to reject,
  // never loop over it in JS) are left alone, so this file still surfaces
  // real `SUMMARY N/M` signal instead of a blanket `0/0`.
  "limits.any.js": [
    [
      'testLimit("types", 1, kJSEmbeddingMaxTypes, (builder, count) => {',
      'if (false) testLimit("types", 1, kJSEmbeddingMaxTypes, (builder, count) => {',
    ],
    [
      'testLimit("functions", 1, kJSEmbeddingMaxFunctions, (builder, count) => {',
      'if (false) testLimit("functions", 1, kJSEmbeddingMaxFunctions, (builder, count) => {',
    ],
    [
      'testLimit("imports", 1, kJSEmbeddingMaxImports, (builder, count) => {',
      'if (false) testLimit("imports", 1, kJSEmbeddingMaxImports, (builder, count) => {',
    ],
    [
      'testLimit("exports", 1, kJSEmbeddingMaxExports, (builder, count) => {',
      'if (false) testLimit("exports", 1, kJSEmbeddingMaxExports, (builder, count) => {',
    ],
    [
      'testLimit("globals", 1, kJSEmbeddingMaxGlobals, (builder, count) => {',
      'if (false) testLimit("globals", 1, kJSEmbeddingMaxGlobals, (builder, count) => {',
    ],
    [
      'testLimit("data segments", 1, kJSEmbeddingMaxDataSegments, (builder, count) => {',
      'if (false) testLimit("data segments", 1, kJSEmbeddingMaxDataSegments, (builder, count) => {',
    ],
    [
      'testLimit("function size", 2, kJSEmbeddingMaxFunctionSize, (builder, count) => {',
      'if (false) testLimit("function size", 2, kJSEmbeddingMaxFunctionSize, (builder, count) => {',
    ],
    [
      "testLimit(\"element segments\", 1, kJSEmbeddingMaxElementSegments,",
      "if (false) testLimit(\"element segments\", 1, kJSEmbeddingMaxElementSegments,",
    ],
    [
      'testLimit("tables", 0, kJSEmbeddingMaxTables, (builder, count) => {',
      'if (false) testLimit("tables", 0, kJSEmbeddingMaxTables, (builder, count) => {',
    ],
    [
      "testModuleSizeLimit(kJSEmbeddingMaxModuleSize, true);",
      "if (false) testModuleSizeLimit(kJSEmbeddingMaxModuleSize, true);",
    ],
    [
      "testModuleSizeLimit(kJSEmbeddingMaxModuleSize + 1, false);",
      "if (false) testModuleSizeLimit(kJSEmbeddingMaxModuleSize + 1, false);",
    ],
  ],

  // `instance/constructor.any.js` (docs/out_of_scope.md #4) -- 4 of
  // `instanceTestFactory`'s entries build a `new WebAssembly.Memory({
  // initial: 64, maximum: 128 })` (4MB, each byte individually JSON-encoded
  // over the SpecTec RPC bridge -- see docs/out_of_scope.md #4 for why that
  // blows well past this repo's `-Xmx3g`) inside their own factory function,
  // only actually called when `test()` invokes that specific subtest's
  // callback -- so filtering them out of the array `test()`'s own driving
  // loop consumes is enough; nothing upstream of that loop ever calls their
  // factory function at all.
  "instance/constructor.any.js": [
    [
      "for (const [name, fn] of instanceTestFactory) {",
      'for (const [name, fn] of instanceTestFactory.filter(([n]) => !["getter order for imports object", "imports", "imports with empty module names", "imports with empty names"].includes(n))) {',
    ],
  ],

  // `instance/constructor-bad-imports.any.js` and
  // `constructor/instantiate-bad-imports.any.js` (docs/out_of_scope.md #4) --
  // same root cause as `instance/constructor.any.js` above (a `new
  // WebAssembly.Memory`/`Table` too large for the SpecTec RPC bridge), but a
  // different shape and a different source: both files pull in
  // `spectec/test/js-api/bad-imports.js` (a shared META script dependency,
  // resolved into `depsSrc` below, not `body`) for its `nonMemories`/
  // `nonTables` array *literals*, built eagerly as soon as the shared
  // `test_bad_imports` function itself runs (called at each file's own
  // top-level), each element later driving one `t(...)` subtest. An array
  // element is an expression, not a statement, so it can't take an
  // `if (false) ` prefix the way `limits.any.js`'s entries do above --
  // deleting the one offending element outright is safe here since nothing
  // else in either array depends on its presence or position.
  // `constructor/instantiate-bad-imports.any.js` didn't reach this at the
  // time this patch was written (blocked earlier by an IEEE754-rounding gap,
  // since fixed) but was patched proactively anyway, since it pulls in the
  // exact same shared script -- otherwise this OOM would just resurface the
  // moment whatever's blocking it earlier gets fixed.
  "instance/constructor-bad-imports.any.js": badImportsPatches,
  "constructor/instantiate-bad-imports.any.js": badImportsPatches,
};

fs.rmSync(generatedDir, { recursive: true, force: true });

let count = 0;
for (const testFilePath of testFiles) {
  const relPath = path.relative(jsApiRoot, testFilePath);
  const resolved = readResolvedDeps(testFilePath);
  if (!resolved) {
    console.log(`SKIP (no jsshell scope) ${relPath}`);
    continue;
  }
  const { meta } = resolved;
  let { depsSrc } = resolved;
  const usesWasmModuleBuilder = meta.scripts.some((ref) => ref.endsWith("/wasm-module-builder.js"));
  let body = meta.body;
  for (const [from, to] of testPatches) body = body.replaceAll(from, to);
  // a per-file patch's target text may live in either the test's own body or
  // a shared META script dependency (`depsSrc`) -- e.g. `bad-imports.js`'s
  // `nonMemories`/`nonTables`, see `badImportsPatches` above -- so both get
  // the same patch list applied; whichever one doesn't contain a given
  // `from` is simply left unchanged by that no-op replaceAll.
  for (const [from, to] of perFilePatches[relPath] ?? []) {
    body = body.replaceAll(from, to);
    depsSrc = depsSrc.replaceAll(from, to);
  }
  const src = [
    shellShim,
    testharnessLite,
    skipKnownGaps,
    ...(usesWasmModuleBuilder ? [dataViewPolyfill] : []),
    depsSrc,
    body,
    reportShim,
  ].join("\n");

  const outPath = path.join(generatedDir, relPath);
  fs.mkdirSync(path.dirname(outPath), { recursive: true });
  fs.writeFileSync(outPath, src);
  count++;
}

console.log(`Generated ${count} fixture(s) under ${path.relative(repoRoot, generatedDir)}/`);
