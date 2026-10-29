// `WebAssembly.instantiate` has two WebIDL overloads, one taking a Module and
// one taking a BufferSource; overload resolution picks the Module one only when
// the first argument is a platform object implementing Module. The official
// corpus never reaches these other two cases:
//
// - no argument at all: no overload takes zero arguments, so overload
//   resolution itself throws -> TypeError.
// - a platform object that is not a Module (here a Memory): resolution falls
//   through to the BufferSource overload, whose conversion fails -> TypeError.
//
// Both are promise-returning, so the TypeError comes back as a rejection.
// Confirmed against real Node 26.12.1: both reject with TypeError.
function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

function expectTypeErrorRejection(promise, msg) {
  return promise.then(
    () => { throw new Error(msg + ": expected a rejection, got fulfillment"); },
    (e) => assertEq(e instanceof TypeError, true, msg + ": expected a TypeError, got " + e),
  );
}

Promise.all([
  expectTypeErrorRejection(WebAssembly.instantiate(), "no argument"),
  expectTypeErrorRejection(
    WebAssembly.instantiate(new WebAssembly.Memory({ initial: 1 })),
    "a Memory instead of a Module or bytes",
  ),
]).then(() => {
  globalThis.__wjiOk = true;
});
