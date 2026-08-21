// Shared `// META:` parsing for spectec/test/js-api/**/*.any.js, used by both
// wji-harness-check.js (validates testharness-lite.js against the real
// testharness.js) and wji-generate-js-api-tests.js (produces the
// self-contained fixtures under tests/wji/js-api/generated/).
"use strict";

const fs = require("fs");
const path = require("path");

const repoRoot = path.resolve(__dirname, "../../..");
const jsApiRoot = path.join(repoRoot, "spectec/test/js-api");

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

// Reads and resolves every META script a test file declares, in order, or
// throws if one is missing. Returns null (not a throw) if the file doesn't
// declare jsshell scope -- callers should treat that as "skip", matching
// what a real jsshell-based WPT runner would do.
function readResolvedDeps(testFilePath) {
  const content = fs.readFileSync(testFilePath, "utf8");
  const meta = parseMeta(content);
  if (!meta.globalScope.split(",").includes("jsshell")) return null;
  const depsSrc = meta.scripts
    .map((ref) => fs.readFileSync(resolveScript(ref, testFilePath), "utf8"))
    .join("\n");
  return { meta, depsSrc };
}

module.exports = { repoRoot, jsApiRoot, parseMeta, resolveScript, readResolvedDeps };
