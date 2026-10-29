// `read the imports` (spectec/document/js-api/index.bs) checks whether each
// supplied import value implements the interface its import kind needs
// (Global/Memory/Table/Tag). The official corpus only ever supplies either the
// right kind of object or a non-platform value, so the branch where the value
// *is* a platform object, just of the wrong interface, is never taken. Here
// every import gets a WebAssembly object of a different interface:
//
// - global (i32) <- a Memory: not a Global, then not a Number -> LinkError.
// - memory <- a Table, table <- a Memory, tag <- a Memory: "If v does not
//   implement Memory/Table/Tag, throw a LinkError exception."
//
// Confirmed against real Node 26.12.1: all four throw LinkError.
function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

function assertLinkError(bytes, importObject, msg) {
  try {
    new WebAssembly.Instance(new WebAssembly.Module(bytes), importObject);
  } catch (e) {
    assertEq(e instanceof WebAssembly.LinkError, true, msg + ": expected a LinkError, got " + e);
    return;
  }
  throw new Error(msg + ": expected a LinkError, got none");
}

// (module (import "m" "g" (global i32)))
const globalModule = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x02, 0x08, 0x01, 0x01,
  0x6d, 0x01, 0x67, 0x03, 0x7f, 0x00,
]);

// (module (import "m" "mem" (memory 1)))
const memoryModule = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x02, 0x0a, 0x01, 0x01,
  0x6d, 0x03, 0x6d, 0x65, 0x6d, 0x02, 0x00, 0x01,
]);

// (module (import "m" "t" (table 1 funcref)))
const tableModule = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x02, 0x09, 0x01, 0x01,
  0x6d, 0x01, 0x74, 0x01, 0x70, 0x00, 0x01,
]);

// (module (import "m" "t" (tag (param i32))))
const tagModule = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x05, 0x01, 0x60,
  0x01, 0x7f, 0x00, 0x02, 0x08, 0x01, 0x01, 0x6d, 0x01, 0x74, 0x04, 0x00,
  0x00,
]);

const memory = new WebAssembly.Memory({ initial: 1 });
const table = new WebAssembly.Table({ element: "anyfunc", initial: 1 });

assertLinkError(globalModule, { m: { g: memory } }, "i32 global import given a Memory");
assertLinkError(memoryModule, { m: { mem: table } }, "memory import given a Table");
assertLinkError(tableModule, { m: { t: memory } }, "table import given a Memory");
assertLinkError(tagModule, { m: { t: memory } }, "tag import given a Memory");

globalThis.__wjiOk = true;
