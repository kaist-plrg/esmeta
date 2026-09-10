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

// Per-file patches that neuter (`if (false) `-prefix) just the individual
// top-level statements too expensive to actually run -- unlike `testPatches`
// above (fixing a corpus bug so the test can run as-is), these are calls that
// will never finish under an AST-walking interpreter no matter how long
// they're given, so this is a permanent, deliberate decision (see
// docs/out_of_scope.md for why), not a workaround for something fixable.
// Prefixing a call's own first line with `if (false) ` is enough regardless
// of how many lines/args the call spans -- `if (false) EXPR;` only needs
// `EXPR;` to be one statement, which every one of these already is (a bare
// function call) -- so this stays a small, surgical, single-line-per-entry
// patch rather than needing to wrap each multi-line call body individually.
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
  const { meta, depsSrc } = resolved;
  const usesWasmModuleBuilder = meta.scripts.some((ref) => ref.endsWith("/wasm-module-builder.js"));
  let body = meta.body;
  for (const [from, to] of testPatches) body = body.replaceAll(from, to);
  for (const [from, to] of perFilePatches[relPath] ?? []) body = body.replaceAll(from, to);
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
