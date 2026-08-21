// testharness.js has no jsshell output of its own (testharnessreport.js is
// browser-only, e.g. reads window.opener), so `print` isn't defined by
// anything else here -- wasm-module-builder.js's debug logging is the only
// other thing that calls it. Load this right after testharness.js, before
// any test file content.
if (typeof globalThis.print !== "function") globalThis.print = console.log.bind(console);

add_completion_callback((tests, harness_status) => {
  for (const t of tests) {
    print((t.status === 0 ? "PASS" : "FAIL") + " " + t.name + (t.message ? " - " + t.message : ""));
  }
});
