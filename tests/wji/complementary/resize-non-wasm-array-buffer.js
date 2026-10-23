// Targets the "Otherwise, return unhandled." step of WJI's HostResizeArrayBuffer
// redefinition (spectec/document/js-api/index.bs:979): once the redefinition
// replaces the mainline hook, *every* ArrayBuffer.prototype.resize() call
// goes through it, including ones on plain resizable ArrayBuffers that have
// nothing to do with wasm. For those, buffer.[[ArrayBufferDetachKey]] isn't
// "WebAssembly.Memory", so it must return `unhandled` and let ECMA-262's own
// default resize behavior run. Found via esmeta.wji.WjiCoverage's annotated IR:
// memory-resizable-buffer.js only ever resizes Memory-backed buffers, so this
// branch sat at 0 hits.
//
// Confirmed against real Node 26.12.1.
function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

const ab = new ArrayBuffer(8, { maxByteLength: 16 });
new Uint8Array(ab)[7] = 42;

ab.resize(12);
assertEq(ab.byteLength, 12, "growing a plain resizable ArrayBuffer");
assertEq(new Uint8Array(ab)[7], 42, "existing contents survive a grow");

ab.resize(4);
assertEq(ab.byteLength, 4, "shrinking a plain resizable ArrayBuffer");

globalThis.__wjiOk = true;
