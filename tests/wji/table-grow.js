// table-grow.wat: exports a funcref table (initial 1, max 4) pre-populated
// at slot 0 via an active element segment, a second wasm function "g", and
// wasmGrowTable(delta) (raw table.grow instruction, filling with ref.null).
// Verifies Table.prototype.grow and the table.grow instruction both cross
// the JS/wasm boundary correctly: length, old-length return value, fill
// value (explicit and default-null), and failure once maximum is reached.
const bytes = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x0a, 0x02, 0x60,
  0x00, 0x01, 0x7f, 0x60, 0x01, 0x7f, 0x01, 0x7f, 0x03, 0x04, 0x03, 0x00,
  0x00, 0x01, 0x04, 0x05, 0x01, 0x70, 0x01, 0x01, 0x04, 0x07, 0x1d, 0x03,
  0x05, 0x74, 0x61, 0x62, 0x6c, 0x65, 0x01, 0x00, 0x01, 0x67, 0x00, 0x01,
  0x0d, 0x77, 0x61, 0x73, 0x6d, 0x47, 0x72, 0x6f, 0x77, 0x54, 0x61, 0x62,
  0x6c, 0x65, 0x00, 0x02, 0x09, 0x07, 0x01, 0x00, 0x41, 0x00, 0x0b, 0x01,
  0x00, 0x0a, 0x15, 0x03, 0x04, 0x00, 0x41, 0x07, 0x0b, 0x04, 0x00, 0x41,
  0x08, 0x0b, 0x09, 0x00, 0xd0, 0x70, 0x20, 0x00, 0xfc, 0x0f, 0x00, 0x0b
]);

function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

WebAssembly.instantiate(bytes.buffer, {}).then(({ module, instance }) => {
  const table = instance.exports.table;

  assertEq(table.length, 1, "initial length");
  assertEq(table.get(0)(), 7, "pre-populated slot 0");

  // JS-side grow with an explicit fill value: returns the old length, new
  // slots hold the given (wasm-wrapped) function.
  const oldLen1 = table.grow(1, instance.exports.g);
  assertEq(oldLen1, 1, "grow() returns old length");
  assertEq(table.length, 2, "length after explicit-fill grow");
  assertEq(table.get(1)(), 8, "explicit fill value applied");

  // JS-side grow with no fill value: defaults to null for a funcref table.
  const oldLen2 = table.grow(1);
  assertEq(oldLen2, 2, "grow() (default fill) returns old length");
  assertEq(table.length, 3, "length after default-fill grow");
  assertEq(table.get(2), null, "default fill is null");

  // wasm-side grow (the table.grow instruction, filling with ref.null func).
  const oldLen3 = instance.exports.wasmGrowTable(1);
  assertEq(oldLen3, 3, "wasmGrowTable() returns old length");
  assertEq(table.length, 4, "length after wasm-side grow");
  assertEq(table.get(3), null, "wasm-side fill is null");

  // Exceeding the declared maximum (4, already reached): the JS method
  // throws, the raw instruction just signals failure with -1.
  let threw = false;
  try {
    table.grow(1);
  } catch (e) {
    threw = e instanceof RangeError;
  }
  assertEq(threw, true, "grow() past maximum throws RangeError");
  assertEq(table.length, 4, "length unchanged after failed JS-side grow");

  assertEq(instance.exports.wasmGrowTable(1), -1, "wasmGrowTable() past maximum returns -1");
  assertEq(table.length, 4, "length unchanged after failed wasm-side grow");

  globalThis.__wjiOk = true;
}).catch(e => print("uncaught: " + e));
