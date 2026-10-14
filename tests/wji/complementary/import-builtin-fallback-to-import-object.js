// Targets `read the imports`'s (spectec/document/js-api/index.bs) builtin-set
// fallback step: "If o is not an Object or if HasProperty(o, componentName)
// is false, Set o to ?Get(importObject, moduleName)." -- when a module
// imports from an enabled builtin set's module name (e.g. "wasm:js-string")
// but asks for a component name the builtin set doesn't actually provide,
// lookup falls back to the real importObject instead of failing outright.
// Found via esmeta.wji.WjiCoverage's annotated IR: this whole fallback
// cluster sat at 0 hits -- no existing test imports a nonexistent member of
// an enabled builtin set.
//
// Two sub-cases, both via the same module (imports "wasm:js-string"
// "nonExistentFn", a name the real js-string builtin set never defines):
// 1. importObject supplies "wasm:js-string" itself as a plain object with
//    that member -- the fallback succeeds and instantiation completes.
// 2. importObject's own "wasm:js-string" getter throws -- that abrupt
//    Get(importObject, moduleName) must propagate (the ?Get idiom).
//
// Confirmed against real Node 24.2.0: both behave as asserted below (the
// first instantiates successfully, the second rejects by reference equality
// with the getter's own thrown error).
function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

// (module (import "wasm:js-string" "nonExistentFn" (func (param externref) (result i32))))
const bytes = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x06, 0x01, 0x60, 0x01, 0x6f, 0x01, 0x7f, 0x02, 0x20, 0x01,
  0x0e, 0x77, 0x61, 0x73, 0x6d, 0x3a, 0x6a, 0x73, 0x2d, 0x73, 0x74, 0x72, 0x69, 0x6e, 0x67, 0x0d, 0x6e, 0x6f, 0x6e,
  0x45, 0x78, 0x69, 0x73, 0x74, 0x65, 0x6e, 0x74, 0x46, 0x6e, 0x00, 0x00,
]);
const mod = new WebAssembly.Module(bytes, { builtins: ["js-string"] });

// sub-case 1: the builtin set doesn't provide "nonExistentFn" -- fall back to
// importObject's own "wasm:js-string" member, which does.
const fallbackFn = (s) => s.length;
const inst = new WebAssembly.Instance(mod, { "wasm:js-string": { nonExistentFn: fallbackFn } });
assertEq(inst instanceof WebAssembly.Instance, true, "builtin-miss fallback to importObject must succeed");

// sub-case 2: same builtin-miss, but importObject's own lookup of the module
// name is itself abrupt -- that error must propagate.
const thrown = new TypeError("boom from importObject's own wasm:js-string getter");
const throwingImportObject = {
  get "wasm:js-string"() {
    throw thrown;
  },
};
let caught;
try {
  new WebAssembly.Instance(mod, throwingImportObject);
} catch (e) {
  caught = e;
}
assertEq(caught, thrown, "a throwing importObject getter during fallback must propagate verbatim");

globalThis.__wjiOk = true;
