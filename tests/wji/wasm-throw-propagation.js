// wasm-throw-propagation.wat: exports tag `t` (single i32 param) and
// throwIt(i32), which unconditionally throws it. Checks that a wasm-native
// exception (the `throw` instruction, uncaught inside wasm) surfaces to JS as
// a WebAssembly.Exception carrying the tag identity and payload -- distinct
// from both a trap (wasm-trap-propagation.js) and a JS exception simply
// passing through a wasm frame (js-throw-propagation.js).
const bytes = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x05, 0x01, 0x60,
  0x01, 0x7f, 0x00, 0x03, 0x02, 0x01, 0x00, 0x0d, 0x03, 0x01, 0x00, 0x00,
  0x07, 0x0f, 0x02, 0x01, 0x74, 0x04, 0x00, 0x07, 0x74, 0x68, 0x72, 0x6f,
  0x77, 0x49, 0x74, 0x00, 0x00, 0x0a, 0x08, 0x01, 0x06, 0x00, 0x20, 0x00,
  0x08, 0x00, 0x0b,
]);

function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

WebAssembly.instantiate(bytes.buffer, {}).then(({module, instance}) => {
  const tag = instance.exports.t;
  if (!(tag instanceof WebAssembly.Tag))
    throw new Error("expected exports.t to be a WebAssembly.Tag, got " + tag);

  let caught = null;
  try {
    instance.exports.throwIt(42);
  } catch (e) {
    caught = e;
  }

  if (caught === null) throw new Error("throwIt() did not throw");
  if (!(caught instanceof WebAssembly.Exception))
    throw new Error("expected a WebAssembly.Exception, got " + caught);
  if (!caught.is(tag))
    throw new Error("caught exception's tag does not match exports.t");
  assertEq(caught.getArg(tag, 0), 42, "exception payload");

  globalThis.__wjiOk = true;
}).catch(e => print("uncaught: " + e));
