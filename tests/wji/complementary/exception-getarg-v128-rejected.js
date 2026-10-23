// Targets the "a tag's declared parameter type at the requested index is
// v128 or (ref null exn)" rejection branch of {{Exception}}'s `getArg`
// (spectec/document/js-api/index.bs) -- JS has no way to represent a v128
// value, so reading it out of a caught exception's payload unconditionally
// throws a TypeError. Found via esmeta.wji.WjiCoverage's annotated IR: this
// branch sat at 0 hits -- no existing test constructs a tag with a v128
// parameter.
//
// Unlike `call_an_exported_function`'s own v128/exnref check (covered by
// v128-global-and-function-rejected.js), which looks at the *called
// function's own* signature, this one looks at the *tag's* declared
// parameter types -- so an ordinary `() -> ()` exported function that
// internally throws a v128-carrying tag is enough to trigger it; no v128
// value ever has to cross the JS/wasm boundary directly.
//
// Confirmed against real Node 26.12.1: the caught value is a real
// WebAssembly.Exception, and calling `.getArg(tag, 0)` on it throws a
// TypeError ("Invalid type v128").
function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

// (module
//   (tag $t (param v128))
//   (func (export "f")
//     v128.const i32x4 1 2 3 4
//     throw $t)
//   (export "t" (tag $t)))
const bytes = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x08, 0x02, 0x60, 0x01, 0x7b, 0x00, 0x60, 0x00, 0x00, 0x03,
  0x02, 0x01, 0x01, 0x0d, 0x03, 0x01, 0x00, 0x00, 0x07, 0x09, 0x02, 0x01, 0x66, 0x00, 0x00, 0x01, 0x74, 0x04, 0x00,
  0x0a, 0x18, 0x01, 0x16, 0x00, 0xfd, 0x0c, 0x01, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00,
  0x04, 0x00, 0x00, 0x00, 0x08, 0x00, 0x0b,
]);

const mod = new WebAssembly.Module(bytes);
const inst = new WebAssembly.Instance(mod, {});

let caught;
try {
  inst.exports.f();
  throw new Error("expected the exported function to throw");
} catch (e) {
  caught = e;
}
assertEq(caught instanceof WebAssembly.Exception, true, "the caught value must be a real WebAssembly.Exception");

let threw = false;
try {
  caught.getArg(inst.exports.t, 0);
} catch (e) {
  threw = true;
  assertEq(e instanceof TypeError, true, "getArg on a v128 argument must throw a TypeError, got " + e);
}
assertEq(threw, true, "expected getArg to throw");

globalThis.__wjiOk = true;
