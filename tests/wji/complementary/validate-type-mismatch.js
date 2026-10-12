// Targets the "module_validate fails" branch of `compile a WebAssembly
// module` (spectec/document/js-api/index.bs's `compile_a_webassembly_module`,
// which separately decodes then validates): every "invalid module" case in
// the official corpus fails at the earlier *decode* step (a malformed
// binary -- bad magic/section layout/LEB128 encoding/etc.), so the later,
// distinct *validate* step's own failure path (a well-formed binary that
// decodes fine but fails wasm's type-checking rules) was never reached.
// Found via esmeta.wji.WjiCoverage's annotated IR: decode-failure sits at
// 62/62 hits, validate-failure at 0.
//
// `validate-type-mismatch.wat` is intentionally invalid (a function declared
// to return i32 whose body instead leaves an i64 on the stack) -- normal
// `wat2wasm` refuses to emit it ("type mismatch in implicit return"), so the
// bytes below were produced with `wat2wasm --no-check` to force emission of
// a structurally well-formed but type-invalid binary.
//
// Confirmed against real Node 24.2.0: `WebAssembly.validate` returns
// `false` and `WebAssembly.compile`'s promise rejects with a `CompileError`
// (not a `TypeError`/`CompileError` from a decode-level problem) for these
// exact bytes.
function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

// (module (func (result i32) i64.const 0))
const bytes = new Uint8Array([
  0, 97, 115, 109, 1, 0, 0, 0, 1, 5, 1, 96, 0, 1, 127, 3, 2, 1, 0, 10, 6, 1,
  4, 0, 66, 0, 11,
]);

assertEq(WebAssembly.validate(bytes), false, "a well-formed-but-ill-typed module must fail validation");

WebAssembly.compile(bytes).then(
  () => {
    throw new Error("expected compile() to reject, but it resolved");
  },
  (reason) => {
    assertEq(
      reason instanceof WebAssembly.CompileError,
      true,
      "compile() must reject with a CompileError for a validate-time failure",
    );
    globalThis.__wjiOk = true;
  },
);
