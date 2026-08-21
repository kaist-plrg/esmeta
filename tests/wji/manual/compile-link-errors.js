// compile-link-errors.wat: a module that imports env.needed (a func) and
// does nothing else -- used to trigger LinkError when instantiated without
// that import. The CompileError case reuses the same WebAssembly.instantiate
// entry point with deliberately malformed bytes, so both cases exercise the
// exact call path every other fixture already proves is wired (rather than
// WebAssembly.compile/new WebAssembly.Module, which aren't).
const linkBytes = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x04, 0x01, 0x60,
  0x00, 0x00, 0x02, 0x0e, 0x01, 0x03, 0x65, 0x6e, 0x76, 0x06, 0x6e, 0x65,
  0x65, 0x64, 0x65, 0x64, 0x00, 0x00,
]);
// not a valid wasm module at all (wrong magic number) -- decode must fail.
const invalidBytes = new Uint8Array([0x00, 0x00, 0x00, 0x00]);

function assertInstanceOf(value, ctor, msg) {
  if (!(value instanceof ctor))
    throw new Error(msg + ": expected " + ctor.name + ", got " + value);
}

const compileErrorCheck = WebAssembly.instantiate(
  invalidBytes.buffer,
  {},
).then(
  () => {
    throw new Error("instantiate did not reject on an invalid module");
  },
  e => assertInstanceOf(e, WebAssembly.CompileError, "invalid module rejection"),
);

// env.needed is present as an object property but not callable, so
// "read the imports" (IsCallable check) must throw LinkError.
const linkErrorCheck = WebAssembly.instantiate(linkBytes.buffer, {
  env: {},
}).then(
  () => {
    throw new Error("instantiate did not reject on a missing import");
  },
  e => assertInstanceOf(e, WebAssembly.LinkError, "missing import rejection"),
);

Promise.all([compileErrorCheck, linkErrorCheck])
  .then(() => {
    globalThis.__wjiOk = true;
  })
  .catch(e => print("uncaught: " + e));
