// Targets `run a host function`'s "If resultsSize is 0, return « »." step
// (spectec/document/js-api/index.bs:1330): a JS function imported with an empty
// result type, called from wasm, must return normally with its own JS return
// value discarded. Found via esmeta.wji.WjiCoverage's annotated IR: neither the
// official corpus nor any other complementary test calls such an import to
// completion, so this branch sat at 0 hits.
//
// Confirmed against real Node 26.12.1.
function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

// host-function-no-results.wat
const bytes = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x04, 0x01, 0x60,
  0x00, 0x00, 0x02, 0x09, 0x01, 0x03, 0x65, 0x6e, 0x76, 0x01, 0x66, 0x00,
  0x00, 0x03, 0x02, 0x01, 0x00, 0x07, 0x07, 0x01, 0x03, 0x72, 0x75, 0x6e,
  0x00, 0x01, 0x0a, 0x06, 0x01, 0x04, 0x00, 0x10, 0x00, 0x0b,
]);

let calls = 0;
const f = () => {
  calls++;
  return 42;
};
const { run } = new WebAssembly.Instance(new WebAssembly.Module(bytes), {
  env: { f },
}).exports;

assertEq(run(), undefined, "the import's own return value is discarded");
assertEq(calls, 1, "the import ran exactly once");

globalThis.__wjiOk = true;
