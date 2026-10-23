// Targets `run a host function`'s result handling
// (spectec/document/js-api/index.bs), for a JS function imported into wasm:
//   - a v128 in the import's result types throws a TypeError when it's called,
//   - with several results, the JS return value is iterated: a throwing
//     @@iterator getter, an @@iterator returning a non-object, and a throwing
//     `next` all propagate, as does a returned value whose own
//     ToWebAssemblyValue conversion throws.
// Found via esmeta.wji.WjiCoverage's annotated IR: all of these sat at 0
// hits (host-function-multi-return-mismatch.js only covers a wrong count).
//
// Confirmed against real Node 24.2.0.
function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}
function thrown(f) {
  try {
    f();
  } catch (e) {
    return e;
  }
  return undefined;
}

// host-function-result-errors.wat
const bytes = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x0d, 0x03, 0x60,
  0x00, 0x01, 0x7b, 0x60, 0x00, 0x02, 0x7f, 0x7f, 0x60, 0x00, 0x00, 0x02,
  0x1d, 0x02, 0x03, 0x65, 0x6e, 0x76, 0x0a, 0x76, 0x31, 0x32, 0x38, 0x52,
  0x65, 0x73, 0x75, 0x6c, 0x74, 0x00, 0x00, 0x03, 0x65, 0x6e, 0x76, 0x04,
  0x70, 0x61, 0x69, 0x72, 0x00, 0x01, 0x03, 0x03, 0x02, 0x02, 0x02, 0x07,
  0x17, 0x02, 0x08, 0x63, 0x61, 0x6c, 0x6c, 0x56, 0x31, 0x32, 0x38, 0x00,
  0x02, 0x08, 0x63, 0x61, 0x6c, 0x6c, 0x50, 0x61, 0x69, 0x72, 0x00, 0x03,
  0x0a, 0x0e, 0x02, 0x05, 0x00, 0x10, 0x00, 0x1a, 0x0b, 0x06, 0x00, 0x10,
  0x01, 0x1a, 0x1a, 0x0b,
]);

let pairResult;
const { callV128, callPair } = new WebAssembly.Instance(new WebAssembly.Module(bytes), {
  env: {
    v128Result: () => 0,
    pair: () => pairResult,
  },
}).exports;

assertEq(thrown(callV128) instanceof TypeError, true, "a v128 import result must throw TypeError");

const iteratorGetterErr = new Error("@@iterator getter");
pairResult = {
  get [Symbol.iterator]() {
    throw iteratorGetterErr;
  },
};
assertEq(thrown(callPair), iteratorGetterErr, "a throwing @@iterator getter must propagate");

pairResult = { [Symbol.iterator]: () => 1 };
assertEq(thrown(callPair) instanceof TypeError, true, "an @@iterator returning a non-object must throw TypeError");

const nextErr = new Error("next");
pairResult = {
  [Symbol.iterator]: () => ({
    next() {
      throw nextErr;
    },
  }),
};
assertEq(thrown(callPair), nextErr, "a throwing next must propagate");

const valueOfErr = new Error("valueOf");
pairResult = [{ valueOf() { throw valueOfErr; } }, 1];
assertEq(thrown(callPair), valueOfErr, "a result whose conversion throws must propagate");

globalThis.__wjiOk = true;
