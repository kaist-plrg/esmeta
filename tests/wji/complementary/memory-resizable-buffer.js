// Targets WebAssembly.Memory.prototype.toFixedLengthBuffer/toResizableBuffer
// (spectec/document/js-api/index.bs:933-959) and the abstract operation
// HostResizeArrayBuffer it redefines (index.bs:963-981, invoked by the
// mainline ArrayBuffer.prototype.resize() a resizable buffer returned by
// toResizableBuffer() is backed by) -- all three showed up in
// esmeta.wji.WjiCoverage's zero-coverage-funcs list. Unlike
// exported-gc-object-reflection.js/exception-stack.js in this same
// directory, this one wasn't just "uncalled by the official corpus": both
// methods compiled but had no `manuals/intrinsics` property slot at all (see
// docs/hardcodes.md #18's own trailing note, and personal/TODO.md #6) --
// genuinely unreachable from JS until that slot was added alongside this
// test.
//
// Can't cross-check against real Node/V8 here the way other complementary
// tests in this directory do -- `toFixedLengthBuffer`/`toResizableBuffer`
// aren't implemented in any shipping engine yet (confirmed: Node 24.2.0
// throws "mem.toFixedLengthBuffer is not a function"). Kept deliberately
// close to the spec algorithms' own literal steps instead, to minimize the
// risk of encoding a wrong expectation with no engine oracle to catch it.
function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

const PAGE = 65536;

// toFixedLengthBuffer() step 2: IsFixedLengthArrayBuffer(buffer) is true ->
// return it as-is (a fresh Memory's [[BufferObject]] always starts fixed).
const mem = new WebAssembly.Memory({ initial: 1, maximum: 3 });
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
assertEq(resizable.maxByteLength, 3 * PAGE, "maxByteLength must be declared maximum * page size");
assertEq(mem.buffer, resizable, "the Memory's own .buffer must now be the resizable one");

// HostResizeArrayBuffer's WebAssembly-specific redefinition: growing the
// resizable buffer by 1 page must actually grow the underlying linear memory
// (not just the JS-visible byteLength) -- observed through this *same*
// object growing in place, unlike a plain Memory.prototype.grow()/the raw
// memory.grow instruction, which both detach-and-replace .buffer instead.
resizable.resize(2 * PAGE);
assertEq(resizable.byteLength, 2 * PAGE, "resize() grows byteLength in place");
assertEq(mem.buffer.byteLength, 2 * PAGE, "the underlying memory actually grew, not just this view");

globalThis.__wjiOk = true;
