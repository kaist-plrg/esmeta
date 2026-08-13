// constructors.wat: exports a single zero-arg function getAnswer() => 42, no
// imports. Verifies every WebAssembly.* constructor works when called
// directly with `new` (Module/Instance/Memory/Table/Global are all
// synchronous per spec -- unlike WebAssembly.instantiate, no .then() needed
// anywhere here), including the omitted-optional-argument path each of
// Instance/Table/Global has (importObject/value/v).
const bytes = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x05, 0x01, 0x60,
  0x00, 0x01, 0x7f, 0x03, 0x02, 0x01, 0x00, 0x07, 0x0d, 0x01, 0x09, 0x67,
  0x65, 0x74, 0x41, 0x6e, 0x73, 0x77, 0x65, 0x72, 0x00, 0x00, 0x0a, 0x06,
  0x01, 0x04, 0x00, 0x41, 0x2a, 0x0b
]);

const PAGE = 65536;

function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

function assertInstanceOf(value, ctor, msg) {
  if (!(value instanceof ctor))
    throw new Error(msg + ": expected " + ctor.name + ", got " + value);
}

function assertThrowsTypeError(fn, msg) {
  try {
    fn();
  } catch (e) {
    if (e instanceof TypeError) return;
    throw new Error(msg + ": expected TypeError, got " + e);
  }
  throw new Error(msg + ": did not throw");
}

// Module
const module = new WebAssembly.Module(bytes.buffer);
assertInstanceOf(module, WebAssembly.Module, "Module constructor");
assertThrowsTypeError(() => new WebAssembly.Module(), "Module() with no bytes");

// Instance -- importObject omitted (module has no imports, so this must
// still work; exercises Instance's own optional-argument path).
const instance = new WebAssembly.Instance(module);
assertInstanceOf(instance, WebAssembly.Instance, "Instance constructor");
assertEq(instance.exports.getAnswer(), 42, "Instance.exports usable");
assertThrowsTypeError(() => new WebAssembly.Instance(), "Instance() with no module");

// Memory
const memory = new WebAssembly.Memory({ initial: 1 });
assertInstanceOf(memory, WebAssembly.Memory, "Memory constructor");
assertEq(memory.buffer.byteLength, PAGE, "Memory initial size");
assertThrowsTypeError(() => new WebAssembly.Memory(), "Memory() with no descriptor");

// Table -- once with an explicit fill value, once with it omitted (default
// null for a funcref table).
const table1 = new WebAssembly.Table(
  { element: "anyfunc", initial: 2 },
  instance.exports.getAnswer,
);
assertInstanceOf(table1, WebAssembly.Table, "Table constructor");
assertEq(table1.length, 2, "Table initial length");
assertEq(table1.get(0)(), 42, "Table explicit fill value applied");

const table2 = new WebAssembly.Table({ element: "anyfunc", initial: 1 });
assertEq(table2.get(0), null, "Table default fill value is null");
assertThrowsTypeError(() => new WebAssembly.Table(), "Table() with no descriptor");

// Global -- once with an explicit value, once with it omitted (default 0 for
// i32).
const global1 = new WebAssembly.Global({ value: "i32", mutable: true }, 42);
assertInstanceOf(global1, WebAssembly.Global, "Global constructor");
assertEq(global1.value, 42, "Global explicit value applied");

const global2 = new WebAssembly.Global({ value: "i32", mutable: false });
assertEq(global2.value, 0, "Global default value is 0");
assertThrowsTypeError(() => new WebAssembly.Global(), "Global() with no descriptor");

globalThis.__wjiOk = true;
