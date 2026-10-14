// Targets `run a host function`'s (spectec/document/js-api/index.bs)
// multi-value return path: when a host (JS) function is imported as a wasm
// function with two or more declared results, its JS return value must be an
// iterable yielding exactly that many values.
// - "Let method be ?GetMethod(ret, @@iterator). If method is undefined,
//   throw a TypeError." -- the return value isn't iterable at all.
// - "If |values|'s size is not |resultsSize|, throw a TypeError." -- it's
//   iterable but doesn't yield the right count.
// Found via esmeta.wji.WjiCoverage's annotated IR: both sat at 0 hits --
// across the whole official corpus, the multi-result path (resultsSize > 1)
// itself only executes once, and that one case already returns a
// correctly-shaped array.
//
// Confirmed against real Node 24.2.0: a non-iterable return ("number 42 is
// not iterable") and a too-short iterable ("multi-return length mismatch")
// both throw TypeError; a correctly-shaped array (sanity check) succeeds.
function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

// (module
//   (import "m" "f" (func $f (result i32 i32)))
//   (func (export "callF") (result i32)
//     call $f
//     i32.add))
const bytes = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x0a, 0x02, 0x60, 0x00, 0x02, 0x7f, 0x7f, 0x60, 0x00, 0x01,
  0x7f, 0x02, 0x07, 0x01, 0x01, 0x6d, 0x01, 0x66, 0x00, 0x00, 0x03, 0x02, 0x01, 0x01, 0x07, 0x09, 0x01, 0x05, 0x63,
  0x61, 0x6c, 0x6c, 0x46, 0x00, 0x01, 0x0a, 0x07, 0x01, 0x05, 0x00, 0x10, 0x00, 0x6a, 0x0b,
]);
const mod = new WebAssembly.Module(bytes);

function assertTypeError(hostFn, msg) {
  const inst = new WebAssembly.Instance(mod, { m: { f: hostFn } });
  try {
    inst.exports.callF();
  } catch (e) {
    assertEq(e instanceof TypeError, true, msg + ": expected a TypeError, got " + e);
    return;
  }
  throw new Error(msg + ": expected a TypeError, got none");
}

assertTypeError(() => 42, "a non-iterable return value for a multi-result host function");
assertTypeError(() => [1], "an iterable return value with fewer elements than declared results");

// sanity: a correctly-shaped iterable return still works.
const okInst = new WebAssembly.Instance(mod, { m: { f: () => [1, 2] } });
assertEq(okInst.exports.callF(), 3, "a correctly-shaped multi-value return must still work");

globalThis.__wjiOk = true;
