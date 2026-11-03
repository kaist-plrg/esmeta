// JS writes to a Memory's buffer must survive every JS-side operation that
// makes SpecTec read or grow the memory without running wasm in between:
// Memory.prototype.grow, toResizableBuffer, toFixedLengthBuffer, and a
// resizable buffer's resize (HostResizeArrayBuffer). The buffer's data block
// is "identified with" the memory (index.bs), so each write must be visible
// through the new buffer. Node 26.12.1 prints the same values.

function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

const PAGE = 65536;

const m1 = new WebAssembly.Memory({ initial: 1, maximum: 4 });
new Uint8Array(m1.buffer)[0] = 42;
m1.grow(1);
assertEq(new Uint8Array(m1.buffer)[0], 42, "write before grow");

const m2 = new WebAssembly.Memory({ initial: 1, maximum: 4 });
new Uint8Array(m2.buffer)[1] = 43;
const rb2 = m2.toResizableBuffer();
assertEq(new Uint8Array(rb2)[1], 43, "write before toResizableBuffer");

const m3 = new WebAssembly.Memory({ initial: 1, maximum: 4 });
const rb3 = m3.toResizableBuffer();
new Uint8Array(rb3)[2] = 44;
rb3.resize(2 * PAGE);
assertEq(new Uint8Array(rb3)[2], 44, "write before resize");
assertEq(new Uint8Array(m3.buffer)[2], 44, "write before resize, via memory");

const m4 = new WebAssembly.Memory({ initial: 1, maximum: 4 });
const rb4 = m4.toResizableBuffer();
new Uint8Array(rb4)[3] = 45;
const fb4 = m4.toFixedLengthBuffer();
assertEq(new Uint8Array(fb4)[3], 45, "write before toFixedLengthBuffer");

globalThis.__wjiOk = true;
