// reentrant-table-sync.wat: exports table and run(), which sets table[0] to
// a wasm function right before calling an imported JS callback, then
// invokes whatever the callback left at table[1] and returns its result.
// postFnHandle re-exports the imported postFn, giving JS a funcref-legal
// handle to hand back via table.set (see table-mutation.wat). Verifies
// table sync across the OTHER JS/wasm boundary crossing -- not
// WebAssembly.instantiate/an exported function call (covered by
// table-mutation.js), but wasm reentrantly calling an imported JS function
// mid-execution, mirroring reentrant-memory-sync.js/reentrant-global-sync.js
// but for tables instead of linear memory/globals.
const bytes = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x08, 0x02, 0x60,
  0x00, 0x01, 0x7f, 0x60, 0x00, 0x00, 0x02, 0x1d, 0x02, 0x03, 0x65, 0x6e,
  0x76, 0x08, 0x63, 0x61, 0x6c, 0x6c, 0x62, 0x61, 0x63, 0x6b, 0x00, 0x01,
  0x03, 0x65, 0x6e, 0x76, 0x06, 0x70, 0x6f, 0x73, 0x74, 0x46, 0x6e, 0x00,
  0x00, 0x03, 0x03, 0x02, 0x00, 0x00, 0x04, 0x05, 0x01, 0x70, 0x01, 0x02,
  0x02, 0x07, 0x1e, 0x03, 0x0c, 0x70, 0x6f, 0x73, 0x74, 0x46, 0x6e, 0x48,
  0x61, 0x6e, 0x64, 0x6c, 0x65, 0x00, 0x01, 0x05, 0x74, 0x61, 0x62, 0x6c,
  0x65, 0x01, 0x00, 0x03, 0x72, 0x75, 0x6e, 0x00, 0x03, 0x09, 0x05, 0x01,
  0x03, 0x00, 0x01, 0x02, 0x0a, 0x16, 0x02, 0x04, 0x00, 0x41, 0x37, 0x0b,
  0x0f, 0x00, 0x41, 0x00, 0xd2, 0x02, 0x26, 0x00, 0x10, 0x00, 0x41, 0x01,
  0x11, 0x00, 0x00, 0x0b,
]);

function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

let instance = null;
let seenInsideCallback = null;

const importObject = {
  env: {
    callback: () => {
      // wasm -> JS, mid-call: wasm set table[0] to a funcref right before
      // calling us. If the reentrant pull didn't happen, table[0] would
      // still be null here.
      seenInsideCallback = instance.exports.table.get(0)();
      // JS -> wasm, mid-call: hand back a funcref-legal handle to postFn;
      // wasm invokes it through call_indirect right after we return.
      instance.exports.table.set(1, instance.exports.postFnHandle);
    },
    postFn: () => 77,
  },
};

WebAssembly.instantiate(bytes.buffer, importObject).then(result => {
  instance = result.instance;

  const returned = instance.exports.run();

  assertEq(
    seenInsideCallback,
    55,
    "wasm-set table slot before the call not visible inside the JS callback",
  );
  assertEq(
    returned,
    77,
    "JS-set table slot inside the callback not visible to wasm after the call",
  );

  globalThis.__wjiOk = true;
}).catch(e => print("uncaught: " + e));
