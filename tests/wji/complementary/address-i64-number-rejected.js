// Targets `AddressValueToU64`'s i64 case (spectec/document/js-api/index.bs):
// "Let |n| be ? ToBigInt(|jsValue|)." -- for a 64-bit-addressed Table or
// Memory, an address given as a Number (not a BigInt) must throw a
// TypeError, from the constructor's descriptor as well as from grow/get.
// Found via esmeta.wji.WjiCoverage's annotated IR: the ToBigInt abrupt branch
// sat at 0 hits -- every i64 case in the official corpus passes BigInts.
//
// Confirmed against real Node 24.2.0.
function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}
function throwsTypeError(f) {
  try {
    f();
  } catch (e) {
    return e instanceof TypeError;
  }
  return false;
}

assertEq(
  throwsTypeError(() => new WebAssembly.Table({ element: "anyfunc", initial: 1, address: "i64" })),
  true,
  "Table descriptor with a Number initial",
);
const table = new WebAssembly.Table({ element: "anyfunc", initial: 1n, address: "i64" });
assertEq(throwsTypeError(() => table.grow(1)), true, "Table.prototype.grow with a Number delta");
assertEq(throwsTypeError(() => table.get(0)), true, "Table.prototype.get with a Number index");

assertEq(
  throwsTypeError(() => new WebAssembly.Memory({ initial: 1, address: "i64" })),
  true,
  "Memory descriptor with a Number initial",
);
const memory = new WebAssembly.Memory({ initial: 1n, address: "i64" });
assertEq(throwsTypeError(() => memory.grow(1)), true, "Memory.prototype.grow with a Number delta");

globalThis.__wjiOk = true;
