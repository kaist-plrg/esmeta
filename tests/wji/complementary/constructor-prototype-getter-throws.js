// Targets the "GetPrototypeFromConstructor(NewTarget, ...) is abrupt" branch
// shared by all seven WebAssembly interface constructors (Table/Global/
// Memory/Module/Instance/Tag/Exception) -- each reads `NewTarget.prototype`
// (via the real ECMA-262 `GetPrototypeFromConstructor` abstract operation,
// reentrant) to set up the new object's own [[Prototype]] before doing
// anything else, per the "a new/=/new=] {{Iface}} object" WebIDL idiom every
// one of these constructors' spec text uses. A subclass invoking one of these
// via `super(...)` with a `prototype` accessor that itself throws must
// propagate that exception. Found via esmeta.wji.WjiCoverage's annotated IR:
// all seven constructors' own "GetPrototypeFromConstructor result is abrupt"
// branch sat at 0 hits -- the official corpus always constructs with a plain
// (non-Proxied, non-overridden) `new.target`, whose `.prototype` read never
// throws.
//
// A Proxy stands in for `new.target` here (via Reflect.construct's third
// argument) since a plain function's own `.prototype` own-property is
// non-configurable and can't be made to throw directly.
//
// Confirmed against real Node 26.12.1: the thrown error propagates verbatim
// (by reference equality, not just "some TypeError") for all seven.
function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

function makeFakeTarget(thrown) {
  return new Proxy(function () {}, {
    get(target, prop, receiver) {
      if (prop === "prototype") throw thrown;
      return Reflect.get(target, prop, receiver);
    },
  });
}

function assertPropagates(Ctor, args, name) {
  const thrown = new Error("boom-" + name);
  const FakeTarget = makeFakeTarget(thrown);
  let caught;
  try {
    Reflect.construct(Ctor, args, FakeTarget);
  } catch (e) {
    caught = e;
  }
  assertEq(caught, thrown, name + ": a throwing new.target.prototype getter must propagate verbatim");
}

const emptyModule = new Uint8Array([0, 97, 115, 109, 1, 0, 0, 0]);
const moduleObj = new WebAssembly.Module(emptyModule);
const tag = new WebAssembly.Tag({ parameters: [] });

assertPropagates(WebAssembly.Table, [{ element: "anyfunc", initial: 1 }], "Table");
assertPropagates(WebAssembly.Global, [{ value: "i32" }], "Global");
assertPropagates(WebAssembly.Memory, [{ initial: 1 }], "Memory");
assertPropagates(WebAssembly.Module, [emptyModule], "Module");
assertPropagates(WebAssembly.Instance, [moduleObj], "Instance");
assertPropagates(WebAssembly.Tag, [{ parameters: [] }], "Tag");
assertPropagates(WebAssembly.Exception, [tag, []], "Exception");

globalThis.__wjiOk = true;
