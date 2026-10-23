// Targets the "a table's element type is (ref null exn)" rejection branch
// shared by {{Table}}'s `get`/`set` methods (spectec/document/js-api/
// index.bs) -- JS has no way to represent an exnref value, so both
// unconditionally throw a TypeError the moment an exnref-typed element would
// otherwise have to cross into/out of JS. Found via esmeta.wji.WjiCoverage's
// annotated IR: both sat at 0 hits -- no existing test builds a Table with
// an exnref element type.
//
// Confirmed against real Node 26.12.1: both `.get(0)` and `.set(0, null)` throw a
// TypeError ("invalid type exnref" / "invalid type (ref null exn)").
function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

// (module (table (export "t") 1 exnref))
const bytes = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x04, 0x04, 0x01, 0x69, 0x00, 0x01, 0x07, 0x05, 0x01, 0x01, 0x74,
  0x01, 0x00,
]);
const mod = new WebAssembly.Module(bytes);
const inst = new WebAssembly.Instance(mod, {});
const t = inst.exports.t;

function assertTypeError(fn, msg) {
  try {
    fn();
  } catch (e) {
    assertEq(e instanceof TypeError, true, msg + ": expected a TypeError, got " + e);
    return;
  }
  throw new Error(msg + ": expected a TypeError, got none");
}

assertTypeError(() => t.get(0), "Table.prototype.get on an exnref table");
assertTypeError(() => t.set(0, null), "Table.prototype.set on an exnref table");

globalThis.__wjiOk = true;
