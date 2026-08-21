#!/usr/bin/env node
// ------------------------------------------------------------------------------
// Usage: node scripts/wji-harness-check.js <spectec/test/js-api/foo.any.js>
//
// Validates tests/wji/js-api/harness.js (our from-scratch, regex-free
// reimplementation of the small slice of spectec/test/harness/testharness.js
// the js-api corpus actually uses) against the real testharness.js -- NOT
// against WJI. Both variants get the exact same resolved META scripts and
// test body; only the harness implementation differs. If our harness is
// correct, every subtest should PASS/FAIL identically under both.
//
// This can't run under WJI itself: real testharness.js hits ESMeta's
// unmechanized-RegExp gap immediately (see tests/wji/js-api/harness.js's own
// doc comment) -- that's the whole reason harness.js exists. So this only
// tells you "does our harness agree with the real one", under Node for both
// sides; whether WJI itself passes a given test is a separate question
// (`sbt run wji-eval <concatenated file> -silent`).
//
// Not wired into any sbt task -- run by hand when adding/changing harness.js,
// or via scripts/wji-harness-check (loops over the whole js-api corpus).
// ------------------------------------------------------------------------------
"use strict";

const fs = require("fs");
const os = require("os");
const path = require("path");
const { spawnSync } = require("child_process");

const repoRoot = path.resolve(__dirname, "..");
const jsApiRoot = path.join(repoRoot, "spectec/test/js-api");
const wjiJsApiDir = path.join(repoRoot, "tests/wji/js-api");

const testFile = process.argv[2];
if (!testFile) {
  console.error("Usage: node scripts/wji-harness-check.js <path/to/foo.any.js>");
  process.exit(2);
}
const testFilePath = path.resolve(testFile);
const content = fs.readFileSync(testFilePath, "utf8");

function parseMeta(src) {
  const scripts = [];
  let globalScope = "";
  const bodyLines = [];
  for (const line of src.split("\n")) {
    const m = line.match(/^\/\/ META: (\S+)=(.*)$/);
    if (m) {
      const [, key, value] = m;
      if (key === "global") globalScope = value;
      else if (key === "script") scripts.push(value);
      continue;
    }
    bodyLines.push(line);
  }
  return { globalScope, scripts, body: bodyLines.join("\n") };
}

function resolveScript(ref, testFilePath) {
  if (ref.startsWith("/wasm/jsapi/")) {
    return path.join(jsApiRoot, ref.slice("/wasm/jsapi/".length));
  }
  return path.join(path.dirname(testFilePath), ref);
}

const meta = parseMeta(content);
if (!meta.globalScope.split(",").includes("jsshell")) {
  console.log(`SKIP (no jsshell scope) ${path.relative(repoRoot, testFilePath)}`);
  process.exit(0);
}

let depsSrc;
try {
  depsSrc = meta.scripts
    .map((ref) => fs.readFileSync(resolveScript(ref, testFilePath), "utf8"))
    .join("\n");
} catch (e) {
  console.log(`SKIP (missing META script: ${e.message}) ${path.relative(repoRoot, testFilePath)}`);
  process.exit(0);
}

const selfShim = fs.readFileSync(path.join(wjiJsApiDir, "shell-shim.js"), "utf8");
const ourHarness = fs.readFileSync(path.join(wjiJsApiDir, "harness.js"), "utf8");
const realHarness = fs.readFileSync(
  path.join(repoRoot, "spectec/test/harness/testharness.js"),
  "utf8",
);
const printPolyfill =
  'if (typeof globalThis.print !== "function") globalThis.print = console.log.bind(console);\n';

// Reports {name, status} pairs as parseable lines, independent of either
// harness's own human-readable print format (report-shim.js) -- only status
// (PASS/FAIL), not message wording, is what this check compares.
const jsonReporter = `
add_completion_callback((tests) => {
  for (const t of tests) {
    console.log("WJI_HARNESS_CHECK_RESULT " + JSON.stringify({ name: t.name, status: t.status }));
  }
  console.log("WJI_HARNESS_CHECK_DONE");
});
`;

const realSrc = [selfShim, printPolyfill, realHarness, depsSrc, meta.body, jsonReporter].join("\n");
const oursSrc = [selfShim, ourHarness, depsSrc, meta.body, jsonReporter].join("\n");

function run(src, label) {
  const tmp = path.join(os.tmpdir(), `wji-harness-check-${label}-${process.pid}.js`);
  fs.writeFileSync(tmp, src);
  try {
    const result = spawnSync("node", [tmp], { encoding: "utf8", timeout: 15000 });
    const lines = (result.stdout || "").split("\n");
    const results = [];
    let completed = false;
    for (const line of lines) {
      if (line === "WJI_HARNESS_CHECK_DONE") completed = true;
      else if (line.startsWith("WJI_HARNESS_CHECK_RESULT ")) {
        results.push(JSON.parse(line.slice("WJI_HARNESS_CHECK_RESULT ".length)));
      }
    }
    return {
      results,
      completed,
      crashed: result.status !== 0 && !completed,
      stderr: (result.stderr || "").trim().split("\n").slice(-3).join(" / "),
    };
  } finally {
    fs.unlinkSync(tmp);
  }
}

const relPath = path.relative(repoRoot, testFilePath);
const real = run(realSrc, "real");
if (real.crashed || !real.completed) {
  // A pending promise alone doesn't keep Node's event loop alive, so a
  // promise_test whose body never settles (e.g. missing host support for
  // something the test needs) exits cleanly with no output, not a crash --
  // distinguish that from an actual thrown/uncaught error for a clearer message.
  const reason = real.crashed
    ? `crashed: ${real.stderr}`
    : "never completed (a promise_test's promise likely never settled)";
  console.log(`SKIP (ground truth ${reason}) ${relPath}`);
  process.exit(0);
}

const ours = run(oursSrc, "ours");

const realByName = new Map(real.results.map((r) => [r.name, r.status]));
const oursByName = new Map(ours.results.map((r) => [r.name, r.status]));
const allNames = new Set([...realByName.keys(), ...oursByName.keys()]);

const mismatches = [];
for (const name of allNames) {
  const r = realByName.has(name) ? realByName.get(name) : "MISSING";
  const o = oursByName.has(name) ? oursByName.get(name) : "MISSING";
  if (r !== o) mismatches.push({ name, real: r, ours: o });
}

if (ours.crashed || !ours.completed) {
  console.log(`FAIL (our harness crashed: ${ours.stderr}) ${relPath}`);
  process.exit(1);
} else if (mismatches.length > 0) {
  console.log(`FAIL (${mismatches.length}/${allNames.size} subtests diverge) ${relPath}`);
  for (const m of mismatches) {
    console.log(`  "${m.name}": real=${m.real} ours=${m.ours}`);
  }
  process.exit(1);
} else {
  console.log(`MATCH (${allNames.size} subtests) ${relPath}`);
  process.exit(0);
}
