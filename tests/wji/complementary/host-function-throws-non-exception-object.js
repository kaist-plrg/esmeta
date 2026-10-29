// When a host function throws, `create a host function`'s steps
// (spectec/document/js-api/index.bs) check whether the thrown value implements
// Exception: if so its own tag and payload are rethrown, otherwise the value is
// wrapped as a JSTag exception. The official corpus only throws either a real
// WebAssembly.Exception or a non-platform value, never a platform object of
// another interface. Here the host function throws a WebAssembly.Memory, which
// takes the "not an Exception" path and comes back to the JS caller as that
// same object.
//
// Confirmed against real Node 26.12.1: the caught value is the same Memory.
function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

// (module (import "m" "f" (func $f)) (func (export "run") call $f))
const bytes = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x04, 0x01, 0x60,
  0x00, 0x00, 0x02, 0x07, 0x01, 0x01, 0x6d, 0x01, 0x66, 0x00, 0x00, 0x03,
  0x02, 0x01, 0x00, 0x07, 0x07, 0x01, 0x03, 0x72, 0x75, 0x6e, 0x00, 0x01,
  0x0a, 0x06, 0x01, 0x04, 0x00, 0x10, 0x00, 0x0b,
]);

const memory = new WebAssembly.Memory({ initial: 1 });
const instance = new WebAssembly.Instance(new WebAssembly.Module(bytes), {
  m: { f() { throw memory; } },
});

let caught;
try {
  instance.exports.run();
} catch (e) {
  caught = e;
}
assertEq(caught, memory, "thrown Memory reaches the caller unchanged");

globalThis.__wjiOk = true;
