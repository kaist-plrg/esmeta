// Targets the "options.builtins"/"options.importedStringConstants" exists
// branches of `asynchronously_compile_a_webassembly_module_closure1` --
// WebAssembly.compile's own async continuation. Found via esmeta.wji.
// WjiCoverage's annotated IR: both sat at 0 hits even though the *sync*
// counterparts (`WebAssembly.validate`, `new WebAssembly.Module(...)`) do
// exercise the exact same `exists options.builtins` check elsewhere --
// nothing in the official corpus or this directory's own complementary
// tests had called `WebAssembly.compile`/`.instantiate` (the Promise-based
// paths) with an explicit `builtins`/`importedStringConstants` option before.
//
// Confirmed against real Node 24.2.0: resolves to a real Module with both
// options present.
function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

const emptyModule = new Uint8Array([0, 97, 115, 109, 1, 0, 0, 0]);

WebAssembly.compile(emptyModule, {
  builtins: ["js-string"],
  importedStringConstants: "m",
}).then(
  (module) => {
    assertEq(module instanceof WebAssembly.Module, true, "compile() with builtins/importedStringConstants must still resolve to a Module");
    globalThis.__wjiOk = true;
  },
  (reason) => {
    throw new Error("expected compile() to resolve, but it rejected with " + reason);
  },
);
