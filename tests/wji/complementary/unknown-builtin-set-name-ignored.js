// Targets `read the imports`'s own loop over `options.builtins` (spectec/
// document/js-api/index.bs): for each requested builtin set name, it only
// actually instantiates one when the name is "js-string" -- any other name
// is silently skipped (a no-op), since "js-string" is the only builtin set
// this spec snapshot defines. Found via esmeta.wji.WjiCoverage's annotated
// IR: the "name is not js-string, skip it" branch sat at 0 hits -- every
// corpus/complementary test that supplies a `builtins` option always uses
// exactly `["js-string"]`, never an unrecognized name.
//
// Confirmed against real Node 24.2.0: `WebAssembly.instantiate(bytes, {},
// {builtins: ["unknown-builtin-xyz"]})` resolves normally (the unrecognized
// name is simply ignored, not rejected).
//
// The second case also targets `find a builtin` (index.bs:1842)'s "If
// builtinSetName does not refer to a builtin set, then continue." and
// `validate builtin set names`'s duplicate check over two *different* names:
// both were unreached because the module above has no imports to look up, and
// no test passed more than one distinct name.
function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

const emptyModule = new Uint8Array([0, 97, 115, 109, 1, 0, 0, 0]);

// unknown-builtin-set-name-ignored.wat
const importingModule = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x05, 0x01, 0x60,
  0x00, 0x01, 0x7f, 0x02, 0x0a, 0x01, 0x03, 0x65, 0x6e, 0x76, 0x01, 0x6e,
  0x03, 0x7f, 0x00, 0x03, 0x02, 0x01, 0x00, 0x07, 0x05, 0x01, 0x01, 0x6e,
  0x00, 0x00, 0x0a, 0x06, 0x01, 0x04, 0x00, 0x23, 0x00, 0x0b,
]);
const mod = new WebAssembly.Module(importingModule, {
  builtins: ["unknown-builtin-xyz", "js-string"],
});
const { n } = new WebAssembly.Instance(mod, { env: { n: 7 } }).exports;
assertEq(n(), 7, "an ordinary import still resolves from importObject");

WebAssembly.instantiate(emptyModule, {}, { builtins: ["unknown-builtin-xyz"] }).then(
  (result) => {
    assertEq(result.instance instanceof WebAssembly.Instance, true, "an unrecognized builtin set name must be silently ignored, not rejected");
    globalThis.__wjiOk = true;
  },
  (reason) => {
    throw new Error("expected instantiate() to resolve, but it rejected with " + reason);
  },
);
