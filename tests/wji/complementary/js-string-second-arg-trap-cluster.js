// Targets the "second argument is neither null nor a string" trap branch of
// three js-string builtins -- equals/concat/compare -- found via
// esmeta.wji.WjiCoverage: each one's *first*-argument check is exercised by
// the official corpus (every call there uses a valid string as the first
// argument, sometimes an invalid one), but the structurally identical
// *second*-argument check was never hit -- every corpus call passes a valid
// string for both arguments.
//
// Built the same way as js-string-trap-cluster.js (WasmModuleBuilder loaded
// via Node's `vm` module -- see that file's own doc for why). Confirmed
// against real Node 24.2.0: all three throw a WebAssembly.RuntimeError
// (trap), not a TypeError.
function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

// (module
//   (type (func (param externref externref) (result i32)))   ;; equals
//   (type (func (param externref externref) (result (ref extern))))  ;; concat
//   (type (func (param externref externref) (result i32)))   ;; compare
//   (import "wasm:js-string" "equals" (func (type 0)))
//   (import "wasm:js-string" "concat" (func (type 1)))
//   (import "wasm:js-string" "compare" (func (type 2)))
//   (func (export "equals") (type 0) local.get 0 local.get 1 call 0)
//   (func (export "concat") (type 1) local.get 0 local.get 1 call 1)
//   (func (export "compare") (type 2) local.get 0 local.get 1 call 2))
const bytes = new Uint8Array([
  0, 97, 115, 109, 1, 0, 0, 0, 1, 20, 3, 96, 2, 111, 111, 1, 127, 96, 2, 111,
  111, 1, 100, 111, 96, 2, 111, 111, 1, 127, 2, 74, 3, 14, 119, 97, 115,
  109, 58, 106, 115, 45, 115, 116, 114, 105, 110, 103, 6, 101, 113, 117, 97,
  108, 115, 0, 0, 14, 119, 97, 115, 109, 58, 106, 115, 45, 115, 116, 114,
  105, 110, 103, 6, 99, 111, 110, 99, 97, 116, 0, 1, 14, 119, 97, 115, 109,
  58, 106, 115, 45, 115, 116, 114, 105, 110, 103, 7, 99, 111, 109, 112, 97,
  114, 101, 0, 2, 3, 4, 3, 0, 1, 2, 7, 29, 3, 6, 101, 113, 117, 97, 108,
  115, 0, 3, 6, 99, 111, 110, 99, 97, 116, 0, 4, 7, 99, 111, 109, 112, 97,
  114, 101, 0, 5, 10, 28, 3, 8, 0, 32, 0, 32, 1, 16, 0, 11, 8, 0, 32, 0, 32,
  1, 16, 1, 11, 8, 0, 32, 0, 32, 1, 16, 2, 11, 0, 42, 4, 110, 97, 109, 101,
  1, 35, 3, 3, 9, 101, 113, 117, 97, 108, 115, 73, 109, 112, 4, 9, 99, 111,
  110, 99, 97, 116, 73, 109, 112, 5, 10, 99, 111, 109, 112, 97, 114, 101,
  73, 109, 112,
]);

const mod = new WebAssembly.Module(bytes, { builtins: ["js-string"] });
const inst = new WebAssembly.Instance(mod, {});
const exports = inst.exports;

function assertTraps(fn, msg) {
  try {
    fn();
  } catch (e) {
    assertEq(e instanceof WebAssembly.RuntimeError, true, msg + ": expected a RuntimeError");
    return;
  }
  throw new Error(msg + ": expected a trap, got none");
}

assertTraps(() => exports.equals("hi", 42), "equals with a non-string, non-null second argument");
assertTraps(() => exports.concat("hi", 42), "concat with a non-string, non-null second argument");
assertTraps(() => exports.compare("hi", 42), "compare with a non-string, non-null second argument");

// sanity: valid calls still work.
assertEq(exports.equals("hi", "hi"), 1, "equals with two matching strings");
assertEq(exports.concat("hi", "lo"), "hilo", "concat with two valid strings");
assertEq(exports.compare("a", "b"), -1, "compare with two valid strings");

globalThis.__wjiOk = true;
