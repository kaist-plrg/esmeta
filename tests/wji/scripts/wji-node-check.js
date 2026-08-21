#!/usr/bin/env node
// ------------------------------------------------------------------------------
// Runs a single tests/wji/manual/*.js fixture under real Node (V8's
// WebAssembly implementation) and reports whether it behaved the way the
// project's __wjiOk convention expects (see tests/wji/manual/README.md): no
// uncaught synchronous throw, no unhandled promise rejection, and
// globalThis.__wjiOk ends up true.
//
// This checks the fixture itself against a real, standards-compliant engine
// -- independent of whether ESMeta/WJI happens to mechanize the relevant
// spec behavior yet. A fixture that fails here is a bug in the fixture; one
// that passes here but fails (or is cancelled) under `wjiEvalTest` is
// exercising a real WJI gap, not a bad test.
//
// Not wired into any sbt task -- run by hand when authoring/reviewing a
// fixture: `node tests/wji/scripts/wji-node-check.js tests/wji/manual/<name>.js`
// ------------------------------------------------------------------------------
global.print = (...args) => console.log("  [print]", ...args);

const path = require("path");
const file = process.argv[2];

if (!file) {
  console.error("Usage: node tests/wji/scripts/wji-node-check.js <fixture.js>");
  process.exit(2);
}

let hadError = false;
process.on("uncaughtException", (e) => {
  hadError = true;
  console.error("  uncaughtException:", e && e.message);
});
process.on("unhandledRejection", (reason) => {
  hadError = true;
  console.error("  unhandledRejection:", reason && reason.message);
});

try {
  require(path.resolve(file));
} catch (e) {
  hadError = true;
  console.error("  sync throw:", e && e.message);
}

// WebAssembly.instantiate's compile step can run on a background thread, so
// a fixed number of setImmediate/microtask turns isn't reliably enough turns
// to wait for it -- give it real wall-clock time instead. If __wjiOk is
// still unset once this fires, either something never finished or the
// fixture genuinely never set it (both are a FAIL either way).
setTimeout(() => {
  const ok = globalThis.__wjiOk === true && !hadError;
  console.log(
    `  __wjiOk=${globalThis.__wjiOk} hadError=${hadError} => ${ok ? "PASS" : "FAIL"}`,
  );
  process.exit(ok ? 0 : 1);
}, 500);
