// js-throw-through-wasm.wat: exports callImport(), which calls an imported
// JS function `cb`. Checks that a JS exception thrown inside `cb` propagates
// back to the original JS caller unwrapped -- the same object, not a generic
// WebAssembly.RuntimeError -- crossing the wasm frame in between.
const bytes = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x04, 0x01, 0x60,
  0x00, 0x00, 0x02, 0x0a, 0x01, 0x03, 0x65, 0x6e, 0x76, 0x02, 0x63, 0x62,
  0x00, 0x00, 0x03, 0x02, 0x01, 0x00, 0x07, 0x0e, 0x01, 0x0a, 0x63, 0x61,
  0x6c, 0x6c, 0x49, 0x6d, 0x70, 0x6f, 0x72, 0x74, 0x00, 0x01, 0x0a, 0x06,
  0x01, 0x04, 0x00, 0x10, 0x00, 0x0b,
]);

class MyError extends TypeError {
  constructor() {
    super("boom from js callback");
    this.name = "MyError";
  }
}
const thrown = new MyError();

WebAssembly.instantiate(bytes.buffer, {
  env: { cb: () => { throw thrown; } },
}).then(({module, instance}) => {
  let caught = null;
  try {
    instance.exports.callImport();
  } catch (e) {
    caught = e;
  }

  if (caught === null) throw new Error("callImport() did not throw");
  if (caught instanceof WebAssembly.RuntimeError)
    throw new Error("JS exception was wrapped into a WebAssembly.RuntimeError instead of passing through: " + caught);
  if (caught !== thrown)
    throw new Error("caught exception is not the same object thrown from the callback: " + caught);

  globalThis.__wjiOk = true;
}).catch(e => print("uncaught: " + e));
