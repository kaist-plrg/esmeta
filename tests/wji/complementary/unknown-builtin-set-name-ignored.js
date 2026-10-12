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
function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

const emptyModule = new Uint8Array([0, 97, 115, 109, 1, 0, 0, 0]);

WebAssembly.instantiate(emptyModule, {}, { builtins: ["unknown-builtin-xyz"] }).then(
  (result) => {
    assertEq(result.instance instanceof WebAssembly.Instance, true, "an unrecognized builtin set name must be silently ignored, not rejected");
    globalThis.__wjiOk = true;
  },
  (reason) => {
    throw new Error("expected instantiate() to resolve, but it rejected with " + reason);
  },
);
