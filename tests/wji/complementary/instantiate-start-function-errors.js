// Targets `instantiate the core of a WebAssembly module`
// (spectec/document/js-api/index.bs:605), whose bulleted list maps a failed
// instantiation to a JS exception: a link failure throws a LinkError, while an
// error "when running the start function" throws a RuntimeError for a Wasm
// trap or propagates the error object thrown by inner ECMAScript code. No
// subtest in spectec/test/js-api runs a start function that fails, so only the
// LinkError branch was reached.
//
// Each module below fails in its start function:
//   trapModule:      (import "m" "mem" (memory 1))
//                    (func $start (i32.store8 (i32.const 0) (i32.const 42)) unreachable)
//                    (start $start)
//   jsThrowModule:   (import "m" "f" (func $f)) (start $f)
//   wasmThrowModule: (import "m" "t" (tag $t (param i32)))
//                    (func $start (throw $t (i32.const 7)))
//                    (start $start)
//
// Also checks that a store write made by the start function before it trapped
// is kept (the store may be modified even in case of an error).
function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

const trapModule = new WebAssembly.Module(new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x04, 0x01, 0x60,
  0x00, 0x00, 0x02, 0x0a, 0x01, 0x01, 0x6d, 0x03, 0x6d, 0x65, 0x6d, 0x02,
  0x00, 0x01, 0x03, 0x02, 0x01, 0x00, 0x08, 0x01, 0x00, 0x0a, 0x0c, 0x01,
  0x0a, 0x00, 0x41, 0x00, 0x41, 0x2a, 0x3a, 0x00, 0x00, 0x00, 0x0b,
]));
const jsThrowModule = new WebAssembly.Module(new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x04, 0x01, 0x60,
  0x00, 0x00, 0x02, 0x07, 0x01, 0x01, 0x6d, 0x01, 0x66, 0x00, 0x00, 0x08,
  0x01, 0x00,
]));
const wasmThrowModule = new WebAssembly.Module(new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x08, 0x02, 0x60,
  0x01, 0x7f, 0x00, 0x60, 0x00, 0x00, 0x02, 0x08, 0x01, 0x01, 0x6d, 0x01,
  0x74, 0x04, 0x00, 0x00, 0x03, 0x02, 0x01, 0x01, 0x08, 0x01, 0x00, 0x0a,
  0x08, 0x01, 0x06, 0x00, 0x41, 0x07, 0x08, 0x00, 0x0b,
]));

const mem = new WebAssembly.Memory({ initial: 1 });
const thrown = { name: "thrown by the start function" };
const tag = new WebAssembly.Tag({ parameters: ["i32"] });
const trapImports = { m: { mem } };
const jsThrowImports = { m: { f: () => { throw thrown; } } };
const wasmThrowImports = { m: { t: tag } };

function checkTrap(e, how) {
  assertEq(e instanceof WebAssembly.RuntimeError, true,
    how + ": a trapping start function throws a RuntimeError");
}
function checkJsThrow(e, how) {
  assertEq(e, thrown,
    how + ": a JS exception from the start function propagates as is");
}
function checkWasmThrow(e, how) {
  assertEq(e instanceof WebAssembly.Exception, true,
    how + ": a Wasm exception from the start function throws an Exception");
  assertEq(e.is(tag), true, how + ": the Exception carries the thrown tag");
  assertEq(e.getArg(tag, 0), 7, how + ": the Exception carries the payload");
}

function syncThrow(module, imports) {
  try {
    new WebAssembly.Instance(module, imports);
  } catch (e) {
    return e;
  }
  throw new Error("new WebAssembly.Instance did not throw");
}

checkTrap(syncThrow(trapModule, trapImports), "Instance");
assertEq(new Uint8Array(mem.buffer)[0], 42,
  "the start function's write before the trap is kept");
checkJsThrow(syncThrow(jsThrowModule, jsThrowImports), "Instance");
checkWasmThrow(syncThrow(wasmThrowModule, wasmThrowImports), "Instance");

function asyncThrow(module, imports) {
  return WebAssembly.instantiate(module, imports).then(
    () => { throw new Error("WebAssembly.instantiate did not reject"); },
    e => e,
  );
}

asyncThrow(trapModule, trapImports)
  .then(e => {
    checkTrap(e, "instantiate");
    return asyncThrow(jsThrowModule, jsThrowImports);
  })
  .then(e => {
    checkJsThrow(e, "instantiate");
    return asyncThrow(wasmThrowModule, wasmThrowImports);
  })
  .then(e => {
    checkWasmThrow(e, "instantiate");
    globalThis.__wjiOk = true;
  })
  .catch(e => print("uncaught: " + e));
