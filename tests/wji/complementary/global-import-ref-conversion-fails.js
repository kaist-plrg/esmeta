// Targets `read the imports`'s non-Global global-import path
// (spectec/document/js-api/index.bs): "Let |value| be
// ToWebAssemblyValue(|v|, |valtype|). If this operation throws a TypeError,
// catch it, and throw a LinkError exception." -- for a reference-typed global
// import, a plain value that isn't a valid reference (here, a Number for a
// funcref) makes that conversion throw, which must surface as a LinkError.
// Found via esmeta.wji.WjiCoverage's annotated IR: the LinkError branch sat at
// 0 hits -- the earlier Number/BigInt/v128 checks catch every numeric case
// before ToWebAssemblyValue is ever reached.
//
// Confirmed against real Node 24.2.0. SpiderMonkey throws the TypeError itself
// instead of a LinkError (docs/engine_bugs.md #16).
function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

// global-import-ref-conversion-fails.wat
const bytes = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x02, 0x0a, 0x01, 0x03,
  0x65, 0x6e, 0x76, 0x01, 0x67, 0x03, 0x70, 0x00,
]);
const mod = new WebAssembly.Module(bytes);

let caught;
try {
  new WebAssembly.Instance(mod, { env: { g: 5 } });
} catch (e) {
  caught = e;
}
assertEq(caught instanceof WebAssembly.LinkError, true, "a non-function funcref global import must throw LinkError");

// null is a valid funcref, so the same import succeeds with it.
new WebAssembly.Instance(mod, { env: { g: null } });

globalThis.__wjiOk = true;
