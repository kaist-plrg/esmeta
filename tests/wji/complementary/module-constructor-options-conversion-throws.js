// Targets the "options conversion itself is abrupt" branch of the
// `new WebAssembly.Module(bytes, options)` constructor (spectec/document/
// js-api/index.bs's `Module(bytes, options)`) -- the sibling of
// compile-options-conversion-throws.js's WebAssembly.compile/.instantiate
// coverage, but for the *synchronous* constructor path: converting an
// explicitly-supplied `options` argument to WebAssemblyCompileOptions does a
// real Get on each member name, which can throw. The constructor must
// propagate that error synchronously (not wrap it in a rejected promise).
// Found via esmeta.wji.WjiCoverage's annotated IR: 0 hits on this branch --
// no existing test constructs `new WebAssembly.Module` with an explicit
// `options` argument whose own property reads can throw.
//
// NOT covered here (documented in personal/coverage-gaps.md instead): the
// *other* half of this same step -- when no `options` argument is supplied
// at all, the spec still converts a freshly created `OrdinaryObjectCreate(null)`
// object through the same dictionary conversion. That object has a null
// prototype and no own properties, so the conversion can never actually
// observe a throw -- structurally unreachable from any JS input.
//
// Confirmed against real Node 24.2.0: the thrown error propagates verbatim
// (by reference equality) out of the `new WebAssembly.Module(...)` call.
function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

const emptyModule = new Uint8Array([0, 97, 115, 109, 1, 0, 0, 0]);
const thrown = new TypeError("boom from Module constructor's throwing builtins getter");

let caught;
try {
  new WebAssembly.Module(emptyModule, {
    get builtins() {
      throw thrown;
    },
  });
} catch (e) {
  caught = e;
}
assertEq(caught, thrown, "new WebAssembly.Module(bytes, options) must propagate the getter's own thrown error");

globalThis.__wjiOk = true;
