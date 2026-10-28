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
// Also covers the same continuation's rejection branch: a builtin import
// whose type doesn't match the builtin's ("If validating builtins and
// imported string ... returns false, reject |promise| with a CompileError").
//
// Confirmed against real Node 26.12.1: resolves to a real Module with both
// options present, and rejects with a CompileError for the mismatched import.
function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

const emptyModule = new Uint8Array([0, 97, 115, 109, 1, 0, 0, 0]);

// compile-with-builtins-option.wat: imports js-string's `cast` with a wrong
// type, (param i32) instead of (param externref) (result (ref extern)).
const badCastImport = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x05, 0x01, 0x60,
  0x01, 0x7f, 0x00, 0x02, 0x17, 0x01, 0x0e, 0x77, 0x61, 0x73, 0x6d, 0x3a,
  0x6a, 0x73, 0x2d, 0x73, 0x74, 0x72, 0x69, 0x6e, 0x67, 0x04, 0x63, 0x61,
  0x73, 0x74, 0x00, 0x00,
]);

WebAssembly.compile(emptyModule, {
  builtins: ["js-string"],
  importedStringConstants: "m",
}).then(
  (module) => {
    assertEq(module instanceof WebAssembly.Module, true, "compile() with builtins/importedStringConstants must still resolve to a Module");
    return WebAssembly.compile(badCastImport, { builtins: ["js-string"] }).then(
      () => {
        throw new Error("expected compile() of a mismatched builtin import to reject");
      },
      (reason) => {
        assertEq(reason instanceof WebAssembly.CompileError, true, "a mismatched builtin import must reject with CompileError");
        globalThis.__wjiOk = true;
      },
    );
  },
  (reason) => {
    throw new Error("expected compile() to resolve, but it rejected with " + reason);
  },
);
