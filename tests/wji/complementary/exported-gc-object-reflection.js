// Targets [[GetOwnProperty]]/[[HasProperty]] of an Exported GC Object
// (spectec/document/js-api/index.bs:1604/1616) -- both are one-liners
// ("Return undefined."/"Return false.") that WJI already mechanizes
// correctly, but no subtest in spectec/test/js-api/gc/exported-object.
// tentative.any.js actually calls Object.getOwnPropertyDescriptor/the `in`
// operator on a struct -- it exercises [[Get]] (`struct.foo`) and
// [[OwnPropertyKeys]] (`Object.getOwnPropertyNames`) instead, which are
// different internal methods. Found via esmeta.wji.WjiCoverage's
// zero-coverage-funcs list.
//
// No .wat sidecar: this repo's local wat2wasm (WABT 1.0.42, the current
// stable release) doesn't parse the `struct.new` instruction text form at
// all ("unexpected token struct.new, expected an instr") even with
// --enable-gc/--enable-all -- its GC proposal support only covers type
// declarations, not every instruction. Built instead the same way
// spectec/test/js-api/gc/exported-object.tentative.any.js itself does. one
// struct type with a single mutable i32 field, and one zero-argument
// exported function returning a freshly allocated instance of it:
//   const builder = new WasmModuleBuilder();
//   const structIndex = builder.addStruct([makeField(kWasmI32, true)]);
//   builder.addFunction("make", makeSig_r_v(wasmRefType(structIndex)))
//     .addBody([kExprI32Const, 0, ...GCInstr(kExprStructNew), structIndex])
//     .exportFunc();
// (run via Node's `vm` module against wasm-module-builder.js directly,
// sidestepping wat2wasm entirely -- the builder is pure JS, no toolchain
// dependency). Verified against real Node (V8) first.
const bytes = new Uint8Array([
  0, 97, 115, 109, 1, 0, 0, 0, 1, 12, 2, 80, 0, 95, 1, 127, 1, 96, 0, 1, 100,
  0, 3, 2, 1, 1, 7, 8, 1, 4, 109, 97, 107, 101, 0, 0, 10, 9, 1, 7, 0, 65, 0,
  251, 0, 0, 11, 0, 14, 4, 110, 97, 109, 101, 1, 7, 1, 0, 4, 109, 97, 107,
  101,
]);

function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

WebAssembly.instantiate(bytes.buffer, {}).then(({ instance }) => {
  const obj = instance.exports.make();

  assertEq(typeof obj, "object", "exported struct should be an object");
  assertEq(
    Object.getOwnPropertyDescriptor(obj, "foo"),
    undefined,
    "[[GetOwnProperty]] of an Exported GC Object must return undefined",
  );
  assertEq(
    "foo" in obj,
    false,
    "[[HasProperty]] of an Exported GC Object must return false",
  );

  globalThis.__wjiOk = true;
}).catch(e => print("uncaught: " + e));
