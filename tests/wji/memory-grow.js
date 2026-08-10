// memory-grow.wat: exports a memory (initial 1 page, max 4), wasmGrow(delta)
// (raw memory.grow instruction), wasmWrite(offset, value), wasmRead(offset).
// Verifies Memory.prototype.grow and the memory.grow instruction both cross
// the JS/wasm boundary correctly: page count, buffer detach-and-replace on
// grow, data persistence across grow, and failure behavior once maximum is
// reached (RangeError from the JS method vs -1 from the instruction).
const bytes = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x0b, 0x02, 0x60,
  0x01, 0x7f, 0x01, 0x7f, 0x60, 0x02, 0x7f, 0x7f, 0x00, 0x03, 0x04, 0x03,
  0x00, 0x01, 0x00, 0x05, 0x04, 0x01, 0x01, 0x01, 0x04, 0x07, 0x2c, 0x04,
  0x06, 0x6d, 0x65, 0x6d, 0x6f, 0x72, 0x79, 0x02, 0x00, 0x08, 0x77, 0x61,
  0x73, 0x6d, 0x47, 0x72, 0x6f, 0x77, 0x00, 0x00, 0x09, 0x77, 0x61, 0x73,
  0x6d, 0x57, 0x72, 0x69, 0x74, 0x65, 0x00, 0x01, 0x08, 0x77, 0x61, 0x73,
  0x6d, 0x52, 0x65, 0x61, 0x64, 0x00, 0x02, 0x0a, 0x1a, 0x03, 0x06, 0x00,
  0x20, 0x00, 0x40, 0x00, 0x0b, 0x09, 0x00, 0x20, 0x00, 0x20, 0x01, 0x36,
  0x02, 0x00, 0x0b, 0x07, 0x00, 0x20, 0x00, 0x28, 0x02, 0x00, 0x0b
]);

const PAGE = 65536;

function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

WebAssembly.instantiate(bytes.buffer, {}).then(({ module, instance }) => {
  const memory = instance.exports.memory;

  assertEq(memory.buffer.byteLength, PAGE, "initial size");

  instance.exports.wasmWrite(0, 42);
  assertEq(new Int32Array(memory.buffer)[0], 42, "initial write visible");

  // JS-side grow: returns the old page count, replaces .buffer with a fresh
  // (detached-from-the-old-one) ArrayBuffer, and preserves the data.
  const beforeJsGrow = memory.buffer;
  const oldPages1 = memory.grow(1);
  assertEq(oldPages1, 1, "grow() returns old page count");
  assertEq(memory.buffer.byteLength, 2 * PAGE, "size after JS-side grow");
  assertEq(beforeJsGrow.byteLength, 0, "old buffer detached after JS-side grow");
  assertEq(new Int32Array(memory.buffer)[0], 42, "data survives JS-side grow");

  // wasm-side grow (the memory.grow instruction): same refresh/detach
  // behavior, driven from the wasm side instead.
  const beforeWasmGrow = memory.buffer;
  const oldPages2 = instance.exports.wasmGrow(1);
  assertEq(oldPages2, 2, "wasmGrow() returns old page count");
  assertEq(memory.buffer.byteLength, 3 * PAGE, "size after wasm-side grow");
  assertEq(beforeWasmGrow.byteLength, 0, "old buffer detached after wasm-side grow");
  assertEq(instance.exports.wasmRead(0), 42, "data survives wasm-side grow");

  // Exceeding the declared maximum (4 pages, currently at 3): the JS method
  // throws, the raw instruction just signals failure with -1.
  let threw = false;
  try {
    memory.grow(2);
  } catch (e) {
    threw = e instanceof RangeError;
  }
  assertEq(threw, true, "grow() past maximum throws RangeError");
  assertEq(memory.buffer.byteLength, 3 * PAGE, "size unchanged after failed JS-side grow");

  assertEq(instance.exports.wasmGrow(2), -1, "wasmGrow() past maximum returns -1");
  assertEq(memory.buffer.byteLength, 3 * PAGE, "size unchanged after failed wasm-side grow");

  globalThis.__wjiOk = true;
}).catch(e => print("uncaught: " + e));
