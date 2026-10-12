// Targets the "options conversion itself is abrupt" branch of
// WebAssembly.compile (spectec/document/js-api/index.bs's `compile(bytes,
// options)`): converting the explicitly-supplied `options` argument to
// WebAssemblyCompileOptions can itself throw -- not because of an invalid
// value (WebAssemblyCompileOptions has no required/enum members to reject),
// but because WebIDL dictionary conversion does a real `Get` on each member
// name, and a getter can throw. `WebAssembly.compile` must reject its
// returned promise with that same error rather than letting it escape
// synchronously or silently swallowing it. Found via esmeta.wji.WjiCoverage's
// annotated IR: 0 hits on this branch -- every compile.any.js/instantiate.
// any.js subtest that supplies an explicit `options` argument always passes
// a plain object whose own property reads never throw.
//
// Confirmed against real Node 24.2.0: `WebAssembly.compile(bytes, {get
// builtins() { throw new TypeError(...) }})`'s returned promise rejects with
// that exact TypeError.
function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

const emptyModule = new Uint8Array([0, 97, 115, 109, 1, 0, 0, 0]);
const thrown = new TypeError("boom from a throwing builtins getter");
const throwingOptions = {
  get builtins() {
    throw thrown;
  },
};

WebAssembly.compile(emptyModule, throwingOptions).then(
  () => {
    throw new Error("expected the promise to reject, but it resolved");
  },
  (reason) => {
    assertEq(reason, thrown, "compile() must reject with the getter's own thrown error");
    globalThis.__wjiOk = true;
  },
);
