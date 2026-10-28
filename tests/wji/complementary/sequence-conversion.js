// Targets WebIDL's ES-to-IDL `sequence<T>` conversion ("create a sequence
// from an iterable"), used by {{Exception}}'s `sequence<any> payload` and
// {{TagType}}'s `sequence<ValueType> parameters`: a non-object or a
// non-iterable object is a TypeError, a throwing @@iterator propagates, and
// any iterable (not just an Array) works. Found via esmeta.wji.WjiCoverage's
// annotated IR: WJI read these as array-likes (`length` plus indices) and
// crashed on `new WebAssembly.Exception(tag, {})`.
//
// Cross-checked with tests/wji/scripts/diff-engines: SpiderMonkey passes. V8
// (including Node 26.12.1) reads them as array-likes instead of iterating
// (docs/engine_bugs.md #17), and JSC accepts a string payload
// (docs/engine_bugs.md #18).
function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}
function thrown(f) {
  try {
    f();
  } catch (e) {
    return e;
  }
  return undefined;
}

const tag = new WebAssembly.Tag({ parameters: ["i32"] });

for (const payload of [5, "a", {}]) {
  assertEq(
    thrown(() => new WebAssembly.Exception(tag, payload)) instanceof TypeError,
    true,
    "a non-iterable payload (" + typeof payload + ") must throw TypeError",
  );
}

const iteratorErr = new Error("@@iterator");
const throwingIterable = {
  [Symbol.iterator]() {
    throw iteratorErr;
  },
};
assertEq(
  thrown(() => new WebAssembly.Exception(tag, throwingIterable)),
  iteratorErr,
  "a throwing @@iterator must propagate",
);

assertEq(
  new WebAssembly.Exception(tag, new Set([7])).getArg(tag, 0),
  7,
  "any iterable payload works, not just an Array",
);

new WebAssembly.Tag({ parameters: new Set(["i32"]) });
assertEq(
  thrown(() => new WebAssembly.Tag({ parameters: 5 })) instanceof TypeError,
  true,
  "non-iterable tag parameters must throw TypeError",
);

globalThis.__wjiOk = true;
