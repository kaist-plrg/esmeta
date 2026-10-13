// Targets the "a wasm value's type is v128" rejection branch shared by six
// functions (spectec/document/js-api/index.bs's `GetGlobalValue`, the
// {{Global}} `value` getter/setter/`valueOf`, `call an exported function`,
// and `run a host function`) -- JS has no way to represent a v128 value, so
// each of these unconditionally throws a TypeError the moment a v128-typed
// wasm value would otherwise have to cross into/out of JS. Found via
// esmeta.wji.WjiCoverage's annotated IR: all six sat at 0 hits -- no existing
// test ever builds a module with a v128-typed export/import, since doing so
// needs real SIMD instructions in the module (`v128.const`), which none of
// the official corpus's or this directory's modules happen to use.
//
// One module exercises all six:
// - a v128-typed mutable global export -> the getter/setter/valueOf/
//   GetGlobalValue branches.
// - an exported function with a v128 parameter -> `call an exported
//   function`'s branch (checked over the whole signature, param or result).
// - a function that itself calls a host-supplied (imported) function with a
//   v128 argument -> `run a host function`'s own identical check, triggered
//   from the *wasm-calls-into-JS* direction instead.
//
// Confirmed against real Node 24.2.0: all five observable call sites below
// throw a TypeError (the getter/setter/valueOf's messages literally say
// "Can't get/set the value of s128 WebAssembly.Global"; the two function-call
// sites say "type incompatibility when transforming from/to JS").
function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

// (module
//   (import "m" "hostFn" (func $hostFn (param v128)))
//   (global (export "g") (mut v128) (v128.const i32x4 1 2 3 4))
//   (func (export "idv128") (param v128) (result v128) local.get 0)
//   (func (export "callHost")
//     v128.const i32x4 1 2 3 4
//     call $hostFn))
const bytes = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x0d, 0x03, 0x60, 0x01, 0x7b, 0x00, 0x60, 0x01, 0x7b, 0x01,
  0x7b, 0x60, 0x00, 0x00, 0x02, 0x0c, 0x01, 0x01, 0x6d, 0x06, 0x68, 0x6f, 0x73, 0x74, 0x46, 0x6e, 0x00, 0x00, 0x03,
  0x03, 0x02, 0x01, 0x02, 0x06, 0x16, 0x01, 0x7b, 0x01, 0xfd, 0x0c, 0x01, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00,
  0x03, 0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0x0b, 0x07, 0x19, 0x03, 0x01, 0x67, 0x03, 0x00, 0x06, 0x69, 0x64,
  0x76, 0x31, 0x32, 0x38, 0x00, 0x01, 0x08, 0x63, 0x61, 0x6c, 0x6c, 0x48, 0x6f, 0x73, 0x74, 0x00, 0x02, 0x0a, 0x1d,
  0x02, 0x04, 0x00, 0x20, 0x00, 0x0b, 0x16, 0x00, 0xfd, 0x0c, 0x01, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x03,
  0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0x10, 0x00, 0x0b,
]);

const hostFn = (v) => {
  throw new Error("hostFn should never actually run -- the v128 check must fire first");
};
const mod = new WebAssembly.Module(bytes);
const inst = new WebAssembly.Instance(mod, { m: { hostFn } });
const exports = inst.exports;

function assertTypeError(fn, msg) {
  try {
    fn();
  } catch (e) {
    assertEq(e instanceof TypeError, true, msg + ": expected a TypeError, got " + e);
    return;
  }
  throw new Error(msg + ": expected a TypeError, got none");
}

assertTypeError(() => exports.g.value, "reading a v128 global's .value");
assertTypeError(() => {
  exports.g.value = 1;
}, "writing a v128 global's .value");
assertTypeError(() => exports.g.valueOf(), "v128 global's .valueOf()");
assertTypeError(() => exports.idv128(1), "calling an exported function with a v128 parameter");
assertTypeError(() => exports.callHost(), "an exported function that itself calls a host function with a v128 argument");

globalThis.__wjiOk = true;
