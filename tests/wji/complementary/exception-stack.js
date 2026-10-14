// Targets WebAssembly.Exception.prototype's `stack` getter
// (spectec/document/js-api/index.bs:1778: "Return this.[[Stack]]."), already
// mechanized and already has a real `manuals/intrinsics` slot (unlike
// `Memory.prototype.toFixedLengthBuffer`/`toResizableBuffer`) -- it's simply
// never called by any subtest in spectec/test/js-api/exception/*.any.js.
// Found via esmeta.wji.WjiCoverage's zero-coverage-funcs list. No wasm module
// needed: WebAssembly.Tag/Exception are constructible from pure JS.
//
// The spec leaves [[Stack]]'s value implementation-defined ("either a
// DOMString representation of the current call stack or undefined"), so this
// only checks the getter doesn't throw and returns one of the two allowed
// shapes -- not a specific value.
function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

const tag = new WebAssembly.Tag({ parameters: [] });
const exn = new WebAssembly.Exception(tag, []);
const stack = exn.stack;
assertEq(
  stack === undefined || typeof stack === "string",
  true,
  "Exception.prototype.stack must be undefined or a string",
);

globalThis.__wjiOk = true;
