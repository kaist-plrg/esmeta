// Targets the "options conversion itself is abrupt" branch of both
// WebAssembly.compile and WebAssembly.instantiate(bytes, ...)
// (spectec/document/js-api/index.bs's `compile(bytes, options)`/
// `instantiate(bytes, importObject, options)`): converting the
// explicitly-supplied `options` argument to WebAssemblyCompileOptions can
// itself throw -- not because of an invalid value (WebAssemblyCompileOptions
// has no required/enum members to reject), but because WebIDL dictionary
// conversion does a real `Get` on each member name, and a getter can throw.
// Both must reject their returned promise with that same error rather than
// letting it escape synchronously or silently swallowing it. Found via
// esmeta.wji.WjiCoverage's annotated IR: 0 hits on this branch in both
// functions -- every compile.any.js/instantiate.any.js subtest that supplies
// an explicit `options` argument always passes a plain object whose own
// property reads never throw.
//
// Confirmed against real Node 24.2.0: both `WebAssembly.compile(bytes, {get
// builtins() { throw ... }})` and `WebAssembly.instantiate(bytes, {}, {get
// builtins() { throw ... }})`'s returned promises reject with that exact
// error.
function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

function throwingOptions(thrown) {
  return {
    get builtins() {
      throw thrown;
    },
  };
}

const emptyModule = new Uint8Array([0, 97, 115, 109, 1, 0, 0, 0]);

const compileThrown = new TypeError("boom from compile()'s throwing builtins getter");
const compileDone = WebAssembly.compile(emptyModule, throwingOptions(compileThrown)).then(
  () => {
    throw new Error("expected compile()'s promise to reject, but it resolved");
  },
  (reason) => {
    assertEq(reason, compileThrown, "compile() must reject with the getter's own thrown error");
  },
);

const instantiateThrown = new TypeError("boom from instantiate()'s throwing builtins getter");
const instantiateDone = WebAssembly.instantiate(emptyModule, {}, throwingOptions(instantiateThrown)).then(
  () => {
    throw new Error("expected instantiate()'s promise to reject, but it resolved");
  },
  (reason) => {
    assertEq(reason, instantiateThrown, "instantiate() must reject with the getter's own thrown error");
  },
);

Promise.all([compileDone, instantiateDone]).then(() => {
  globalThis.__wjiOk = true;
});
