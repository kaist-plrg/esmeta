// Targets WebAssembly.Memory.prototype.toFixedLengthBuffer/toResizableBuffer
// (spectec/document/js-api/index.bs:933-959) and the abstract operation
// HostResizeArrayBuffer it redefines (index.bs:963-981, invoked by the
// mainline ArrayBuffer.prototype.resize() a resizable buffer returned by
// toResizableBuffer() is backed by) -- all three showed up in
// esmeta.wji.WjiCoverage's zero-coverage-funcs list. Unlike
// exported-gc-object-reflection.js/exception-stack.js in this same
// directory, this one wasn't just "uncalled by the official corpus": both
// methods compiled but had no `manuals/intrinsics` property slot at all (see
// docs/hardcodes.md #18's own trailing note) --
// genuinely unreachable from JS until that slot was added alongside this
// test.
//
// Confirmed against real Node 26.12.1. JSC fails the no-maximum TypeError
// check (docs/engine_bugs.md #5).
function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

const PAGE = 65536;

// toFixedLengthBuffer() step 2: IsFixedLengthArrayBuffer(buffer) is true ->
// return it as-is (a fresh Memory's [[BufferObject]] always starts fixed).
const mem = new WebAssembly.Memory({ initial: 1, maximum: 4 });
const fixed = mem.toFixedLengthBuffer();
assertEq(fixed, mem.buffer, "toFixedLengthBuffer on an already-fixed buffer returns it as-is");
assertEq(fixed.resizable, false, "a fixed buffer must report resizable === false");

// toResizableBuffer() step 4: no declared maximum -> TypeError, before ever
// reaching create_a_resizable_memory_buffer.
const noMax = new WebAssembly.Memory({ initial: 1 });
let threwForNoMax = false;
try {
  noMax.toResizableBuffer();
} catch (e) {
  threwForNoMax = e instanceof TypeError;
}
assertEq(threwForNoMax, true, "toResizableBuffer without a declared maximum must throw TypeError");

// toResizableBuffer() steps 5-8: builds a resizable buffer sized off the
// declared maximum (maxsize = memtype's max * 65536) and installs it as the
// new [[BufferObject]].
const resizable = mem.toResizableBuffer();
assertEq(resizable.resizable, true, "toResizableBuffer's result must report resizable === true");
assertEq(resizable.byteLength, PAGE, "resizable buffer starts at the memory's current size");
assertEq(resizable.maxByteLength, 4 * PAGE, "maxByteLength must be declared maximum * page size");
assertEq(mem.buffer, resizable, "the Memory's own .buffer must now be the resizable one");

// HostResizeArrayBuffer's WebAssembly-specific redefinition (index.bs:963-981)
// grows the underlying linear memory itself, in place: the same buffer object
// grows, and Memory.prototype.grow() then sees the new page count.
resizable.resize(2 * PAGE);
assertEq(resizable.byteLength, 2 * PAGE, "resize() grows byteLength in place");
assertEq(mem.buffer, resizable, "resize() keeps the same buffer object");
assertEq(mem.grow(1), 2, "resize() must grow the real wasm memory, not just the buffer");

// refresh the memory buffer's "Otherwise" branch (index.bs:884-894): growing
// a Memory whose buffer is resizable updates that buffer in place.
assertEq(mem.buffer, resizable, "grow() on a resizable-backed Memory keeps the same buffer");
assertEq(resizable.byteLength, 3 * PAGE, "grow() updates the resizable buffer's byteLength");

// HostResizeArrayBuffer step 1.3.1.3: a shrink or a non-page-multiple delta
// throws RangeError.
function throwsRangeError(f) {
  try {
    f();
  } catch (e) {
    return e instanceof RangeError;
  }
  return false;
}
assertEq(throwsRangeError(() => resizable.resize(2 * PAGE)), true, "shrinking must throw RangeError");
assertEq(throwsRangeError(() => resizable.resize(3 * PAGE + 1)), true, "a non-page-multiple size must throw RangeError");
assertEq(resizable.byteLength, 3 * PAGE, "a rejected resize() leaves byteLength unchanged");

// toResizableBuffer() step 6: the buffer is already resizable -> returned as-is.
assertEq(mem.toResizableBuffer(), resizable, "toResizableBuffer on an already-resizable buffer returns it as-is");

// toFixedLengthBuffer() steps 3-7: converting back detaches the resizable
// buffer and installs a new fixed-length one of the same size.
const fixedAgain = mem.toFixedLengthBuffer();
assertEq(fixedAgain === resizable, false, "toFixedLengthBuffer on a resizable buffer must create a new buffer");
assertEq(fixedAgain.resizable, false, "the new buffer must be fixed-length");
assertEq(fixedAgain.byteLength, 3 * PAGE, "the new fixed buffer keeps the memory's current size");
assertEq(mem.buffer, fixedAgain, "the Memory's own .buffer must now be the fixed one");
assertEq(resizable.detached, true, "the old resizable buffer must be detached");

// Both methods are brand-checked: a non-Memory `this` throws TypeError.
function throwsTypeError(f) {
  try {
    f();
  } catch (e) {
    return e instanceof TypeError;
  }
  return false;
}
const proto = WebAssembly.Memory.prototype;
assertEq(throwsTypeError(() => proto.toFixedLengthBuffer.call({})), true, "toFixedLengthBuffer on a non-Memory must throw TypeError");
assertEq(throwsTypeError(() => proto.toResizableBuffer.call({})), true, "toResizableBuffer on a non-Memory must throw TypeError");

globalThis.__wjiOk = true;
