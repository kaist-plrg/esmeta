// wasm-trap-propagation.wat: exports a function that unconditionally traps
// (unreachable). Verifies a wasm trap surfaces to JS as a thrown
// WebAssembly.RuntimeError.
const bytes = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x04, 0x01, 0x60,
  0x00, 0x00, 0x03, 0x02, 0x01, 0x00, 0x07, 0x08, 0x01, 0x04, 0x74, 0x72,
  0x61, 0x70, 0x00, 0x00, 0x0a, 0x05, 0x01, 0x03, 0x00, 0x00, 0x0b,
]);

WebAssembly.instantiate(bytes.buffer, {}).then(({module, instance}) => {
  let caught = null;
  try {
    instance.exports.trap();
  } catch (e) {
    caught = e;
  }

  if (caught === null) throw new Error("trap() did not throw");
  if (!(caught instanceof WebAssembly.RuntimeError))
    throw new Error("expected a WebAssembly.RuntimeError, got " + caught);

  globalThis.__wjiOk = true;
}).catch(e => print("uncaught: " + e));
