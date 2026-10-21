// Targets the js-string builtins' out-of-range trap branches that neither the
// official corpus nor js-string-trap-cluster.js exercises, found via
// esmeta.wji.WjiCoverage:
//
// 1. charCodeAt: index >= the string's length.
// 2. fromCharCodeArray: end < start, or the array is shorter than end.
// 3. intoCharCodeArray: the array can't hold start + the string's length.
//
// js-string-trap-cluster.js left (2)/(3) as a follow-up, believing it needed
// a GC-typed local variable to build a real (non-null) array. It doesn't:
// each wrapper below builds its array inline with `array.new_default` and
// passes it straight to the builtin, taking the array's length as its own
// argument so the same function covers both the trapping and the valid path.
// Bytes generated with spectec/test/js-api/wasm-module-builder.js under
// Node's `vm` module, same as js-string-trap-cluster.js.
//
// Confirmed against real Node 24.2.0: every out-of-range case throws a
// WebAssembly.RuntimeError (trap), every valid case returns normally.
function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

// (module
//   (type $arr (array (mut i16)))   ;; final, as js-string builtins require
//   (import "wasm:js-string" "charCodeAt"
//     (func $charCodeAt (param externref i32) (result i32)))
//   (import "wasm:js-string" "fromCharCodeArray"
//     (func $fromCCA (param (ref null $arr) i32 i32) (result (ref extern))))
//   (import "wasm:js-string" "intoCharCodeArray"
//     (func $intoCCA (param externref (ref null $arr) i32) (result i32)))
//   (func (export "charCodeAt") (param externref i32) (result i32)
//     (call $charCodeAt (local.get 0) (local.get 1)))
//   (func (export "fromCharCodeArray") (param $len i32) (param $start i32) (param $end i32)
//     (result (ref extern))
//     (call $fromCCA (array.new_default $arr (local.get $len)) (local.get $start) (local.get $end)))
//   (func (export "intoCharCodeArray") (param $s externref) (param $len i32) (param $start i32)
//     (result i32)
//     (call $intoCCA (local.get $s) (array.new_default $arr (local.get $len)) (local.get $start))))
const bytes = new Uint8Array([
  0, 97, 115, 109, 1, 0, 0, 0, 1, 42, 6, 94, 119, 1, 96, 2, 111, 127, 1, 127,
  96, 3, 99, 0, 127, 127, 1, 100, 111, 96, 3, 111, 99, 0, 127, 1, 127, 96, 3, 127,
  127, 127, 1, 100, 111, 96, 3, 111, 127, 127, 1, 127, 2, 99, 3, 14, 119, 97, 115, 109,
  58, 106, 115, 45, 115, 116, 114, 105, 110, 103, 10, 99, 104, 97, 114, 67, 111, 100, 101, 65,
  116, 0, 1, 14, 119, 97, 115, 109, 58, 106, 115, 45, 115, 116, 114, 105, 110, 103, 17, 102,
  114, 111, 109, 67, 104, 97, 114, 67, 111, 100, 101, 65, 114, 114, 97, 121, 0, 2, 14, 119,
  97, 115, 109, 58, 106, 115, 45, 115, 116, 114, 105, 110, 103, 17, 105, 110, 116, 111, 67, 104,
  97, 114, 67, 111, 100, 101, 65, 114, 114, 97, 121, 0, 3, 3, 4, 3, 1, 4, 5, 7,
  54, 3, 10, 99, 104, 97, 114, 67, 111, 100, 101, 65, 116, 0, 3, 17, 102, 114, 111, 109,
  67, 104, 97, 114, 67, 111, 100, 101, 65, 114, 114, 97, 121, 0, 4, 17, 105, 110, 116, 111,
  67, 104, 97, 114, 67, 111, 100, 101, 65, 114, 114, 97, 121, 0, 5, 10, 38, 3, 8, 0,
  32, 0, 32, 1, 16, 0, 11, 13, 0, 32, 0, 251, 7, 0, 32, 1, 32, 2, 16, 1,
  11, 13, 0, 32, 0, 32, 1, 251, 7, 0, 32, 2, 16, 2, 11, 0, 58, 4, 110, 97,
  109, 101, 1, 51, 3, 3, 10, 99, 104, 97, 114, 67, 111, 100, 101, 65, 116, 4, 17, 102,
  114, 111, 109, 67, 104, 97, 114, 67, 111, 100, 101, 65, 114, 114, 97, 121, 5, 17, 105, 110,
  116, 111, 67, 104, 97, 114, 67, 111, 100, 101, 65, 114, 114, 97, 121,
]);

const mod = new WebAssembly.Module(bytes, { builtins: ["js-string"] });
const exports = new WebAssembly.Instance(mod, {}).exports;

function assertTraps(fn, msg) {
  try {
    fn();
  } catch (e) {
    assertEq(e instanceof WebAssembly.RuntimeError, true, msg + ": expected a RuntimeError");
    return;
  }
  throw new Error(msg + ": expected a trap, got none");
}

assertTraps(() => exports.charCodeAt("abc", 5), "charCodeAt with index past the end");
assertTraps(() => exports.fromCharCodeArray(4, 2, 1), "fromCharCodeArray with end < start");
assertTraps(() => exports.fromCharCodeArray(2, 0, 3), "fromCharCodeArray with end past the array");
assertTraps(() => exports.intoCharCodeArray("hello", 2, 0), "intoCharCodeArray into a too-short array");
assertTraps(() => exports.intoCharCodeArray("hello", 10, 8), "intoCharCodeArray with start too close to the end");

// sanity: the same wrappers with in-range arguments don't trap.
assertEq(exports.charCodeAt("abc", 1), 98, "charCodeAt with a valid index");
assertEq(exports.fromCharCodeArray(3, 0, 3), "\0\0\0", "fromCharCodeArray with a valid range");
assertEq(exports.intoCharCodeArray("hi", 4, 1), 2, "intoCharCodeArray with enough room");

globalThis.__wjiOk = true;
