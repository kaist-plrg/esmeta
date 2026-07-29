// reentrant-memory-sync.wat: exports memory and run(preOff, preVal, postOff),
// which stores preVal at preOff, calls an imported JS callback, then loads
// and returns whatever is at postOff. Verifies memory sync across the OTHER
// JS/wasm boundary crossing -- not WebAssembly.instantiate/an exported
// function call (covered by memory-mutation.js), but wasm reentrantly
// calling an imported JS function mid-execution (Interpreter.toHostFunc's
// pullMemories/pushMemories path, never exercised until now).
const bytes = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x0b, 0x02, 0x60,
  0x00, 0x00, 0x60, 0x03, 0x7f, 0x7f, 0x7f, 0x01, 0x7f, 0x02, 0x10, 0x01,
  0x03, 0x65, 0x6e, 0x76, 0x08, 0x63, 0x61, 0x6c, 0x6c, 0x62, 0x61, 0x63,
  0x6b, 0x00, 0x00, 0x03, 0x02, 0x01, 0x01, 0x05, 0x03, 0x01, 0x00, 0x01,
  0x07, 0x10, 0x02, 0x06, 0x6d, 0x65, 0x6d, 0x6f, 0x72, 0x79, 0x02, 0x00,
  0x03, 0x72, 0x75, 0x6e, 0x00, 0x01, 0x0a, 0x12, 0x01, 0x10, 0x00, 0x20,
  0x00, 0x20, 0x01, 0x36, 0x02, 0x00, 0x10, 0x00, 0x20, 0x02, 0x28, 0x02,
  0x00, 0x0b,
]);

function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

let memory = null;
let seenInsideCallback = null;

const importObject = {
  env: {
    callback: () => {
      // wasm -> JS, mid-call: wasm wrote preVal at preOff right before
      // calling us. If the reentrant pull didn't happen, this reads stale
      // (zero) bytes instead.
      seenInsideCallback = new Int32Array(memory.buffer)[0];
      // JS -> wasm, mid-call: write directly into the buffer here; wasm
      // reads this back via i32.load right after we return. If the
      // reentrant push didn't happen, wasm sees whatever it had before.
      new Int32Array(memory.buffer)[2] = 77;
    },
  },
};

WebAssembly.instantiate(bytes.buffer, importObject).then(
  ({ module, instance }) => {
    memory = instance.exports.memory;

    const result = instance.exports.run(0, 55, 8);

    assertEq(
      seenInsideCallback,
      55,
      "wasm write before the call not visible inside the JS callback",
    );
    assertEq(
      result,
      77,
      "JS write inside the callback not visible to wasm after the call",
    );

    globalThis.__wjiOk = true;
  },
).catch(e => print("uncaught: " + e));
