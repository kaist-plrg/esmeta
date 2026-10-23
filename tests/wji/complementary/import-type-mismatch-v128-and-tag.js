// Targets two of `read the imports`'s (spectec/document/js-api/index.bs)
// per-externtype LinkError branches that the official corpus never exercises,
// found via esmeta.wji.WjiCoverage's annotated IR:
//
// 1. A GLOBAL import declared with value type v128: "If valtype is v128,
//    throw a LinkError exception" -- unconditional, since JS has no way to
//    supply a v128 value directly (only via a WebAssembly.Global wrapping
//    one, which is its own separate untested v128 cluster -- see
//    personal/coverage-gaps.md). Here we only need the *import declaration*
//    itself to be v128-typed; the supplied JS value can be anything that
//    doesn't implement Global.
// 2. A TAG import whose supplied JS value doesn't implement {{Tag}}: "If v
//    does not implement Tag, throw a LinkError exception."
//
// Neither needs any SIMD/GC construction -- just a module that *declares* an
// import of the right shape, built directly from WAT (wat2wasm), no
// WasmModuleBuilder needed.
//
// Confirmed against real Node 26.12.1:
// - v128 global import + plain Number value -> LinkError ("global import of
//   type v128 must be a WebAssembly.Global").
// - Tag import + plain object value -> LinkError ("tag import requires a
//   WebAssembly.Tag").
function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

// (module (import "m" "g" (global v128)))
const v128GlobalModule = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x02, 0x08, 0x01, 0x01, 0x6d, 0x01, 0x67, 0x03, 0x7b, 0x00,
]);

// (module (import "m" "t" (tag (param i32))))
const tagModule = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x05, 0x01, 0x60, 0x01, 0x7f, 0x00, 0x02, 0x08, 0x01, 0x01,
  0x6d, 0x01, 0x74, 0x04, 0x00, 0x00,
]);

function assertLinkError(fn, msg) {
  try {
    fn();
  } catch (e) {
    assertEq(e instanceof WebAssembly.LinkError, true, msg + ": expected a LinkError, got " + e);
    return;
  }
  throw new Error(msg + ": expected a LinkError, got none");
}

assertLinkError(() => {
  new WebAssembly.Instance(new WebAssembly.Module(v128GlobalModule), { m: { g: 42 } });
}, "v128 global import given a plain Number");

assertLinkError(() => {
  new WebAssembly.Instance(new WebAssembly.Module(tagModule), { m: { t: {} } });
}, "tag import given a non-Tag object");

globalThis.__wjiOk = true;
