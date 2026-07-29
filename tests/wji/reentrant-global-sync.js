// reentrant-global-sync.wat: exports a mutable i32 global `g` and
// run(preVal), which sets g to preVal, calls an imported JS callback, then
// reads and returns g. Verifies global mutation sync across the OTHER
// JS/wasm boundary crossing -- not WebAssembly.instantiate/an exported
// function call (covered by global-mutation.js), but wasm reentrantly
// calling an imported JS function mid-execution, mirroring
// reentrant-memory-sync.js but for globals instead of linear memory.
const bytes = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x09, 0x02, 0x60,
  0x00, 0x00, 0x60, 0x01, 0x7f, 0x01, 0x7f, 0x02, 0x10, 0x01, 0x03, 0x65,
  0x6e, 0x76, 0x08, 0x63, 0x61, 0x6c, 0x6c, 0x62, 0x61, 0x63, 0x6b, 0x00,
  0x00, 0x03, 0x02, 0x01, 0x01, 0x06, 0x06, 0x01, 0x7f, 0x01, 0x41, 0x00,
  0x0b, 0x07, 0x0b, 0x02, 0x01, 0x67, 0x03, 0x00, 0x03, 0x72, 0x75, 0x6e,
  0x00, 0x01, 0x0a, 0x0c, 0x01, 0x0a, 0x00, 0x20, 0x00, 0x24, 0x00, 0x10,
  0x00, 0x23, 0x00, 0x0b,
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
      // wasm -> JS, mid-call: wasm set g to preVal right before calling us.
      seenInsideCallback = instance.exports.g.value;
      // JS -> wasm, mid-call: wasm reads g again via global.get right after
      // we return.
      instance.exports.g.value = 77;
    },
  },
};

WebAssembly.instantiate(bytes.buffer, importObject).then(result => {
  instance = result.instance;

  const returned = instance.exports.run(55);

  assertEq(
    seenInsideCallback,
    55,
    "wasm write to global before the call not visible inside the JS callback",
  );
  assertEq(
    returned,
    77,
    "JS write to global inside the callback not visible to wasm after the call",
  );

  globalThis.__wjiOk = true;
}).catch(e => print("uncaught: " + e));
