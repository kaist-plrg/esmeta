#!/usr/bin/env node
// ------------------------------------------------------------------------------
// Usage: node tests/wji/scripts/wji-generate-js-api-tests.js
//
// Regenerates tests/wji/js-api/generated/ from spectec/test/js-api/: for each
// selected *.any.js file, resolves its META script dependencies and writes a
// single self-contained fixture (shell-shim.js + testharness-lite.js +
// resolved deps + the test body + report-shim.js) to the same relative path
// under generated/. Mirrors scripts/wat2js's philosophy -- this is an
// authoring/sync-time convenience, not a build step; nothing verifies
// automatically that generated/ is still in sync with spectec/test/js-api,
// re-run this by hand after a spectec submodule bump touches that corpus.
//
// Scope is deliberately narrower than the full corpus: gc/, exception/,
// tag/, and js-string/ all exercise newer/tentative wasm proposals (GC
// types, exception handling, ...) that WJI doesn't mechanize at all yet, so
// including them would just produce a wall of expected failures. Extend
// `categories`/`looseFiles` below once WJI's own surface grows to cover them.
// ------------------------------------------------------------------------------
"use strict";

const fs = require("fs");
const path = require("path");
const { repoRoot, jsApiRoot, readResolvedDeps } = require("./js-api-meta");

const wjiJsApiDir = path.join(repoRoot, "tests/wji/js-api");
const generatedDir = path.join(wjiJsApiDir, "generated");

const categories = ["constructor", "instance", "memory", "table", "global", "module"];
const looseFiles = ["interface.any.js", "limits.any.js", "prototypes.any.js"];

function listAnyJsFiles(dir) {
  return fs
    .readdirSync(dir)
    .filter((f) => f.endsWith(".any.js"))
    .map((f) => path.join(dir, f))
    .sort();
}

const testFiles = [
  ...looseFiles.map((f) => path.join(jsApiRoot, f)),
  ...categories.flatMap((c) => listAnyJsFiles(path.join(jsApiRoot, c))),
];

const shellShim = fs.readFileSync(path.join(wjiJsApiDir, "shell-shim.js"), "utf8");
const testharnessLite = fs.readFileSync(path.join(wjiJsApiDir, "testharness-lite.js"), "utf8");
const reportShim = fs.readFileSync(path.join(wjiJsApiDir, "report-shim.js"), "utf8");

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
  const src = [shellShim, testharnessLite, depsSrc, meta.body, reportShim].join("\n");

  const outPath = path.join(generatedDir, relPath);
  fs.mkdirSync(path.dirname(outPath), { recursive: true });
  fs.writeFileSync(outPath, src);
  count++;
}

console.log(`Generated ${count} fixture(s) under ${path.relative(repoRoot, generatedDir)}/`);
