// Targets `read the imports`'s (spectec/document/js-api/index.bs) plain
// (non-builtin) import lookup: "Else, Let o be ?Get(importObject,
// moduleName)." -- when the requested module name isn't one of the enabled
// builtin sets, o comes straight from the user-supplied importObject, and
// that Get can itself throw (e.g. importObject is a plain object with a
// throwing getter for the module name). Found via esmeta.wji.WjiCoverage's
// annotated IR: this branch sat at 0 hits -- no existing test's importObject
// has a trapping getter for an ordinary (non-builtin) module name.
//
// Also covers the next lookup, "Let v be ?Get(o, componentName).": a getter
// for the component name on the module object throwing, which no test
// reached either.
//
// Confirmed against real Node 26.12.1: the thrown error propagates verbatim
// (by reference equality) as the Instance constructor's own thrown error.
function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

// (module (import "regularModule" "x" (func (param i32))))
const bytes = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x05, 0x01, 0x60, 0x01, 0x7f, 0x00, 0x02, 0x13, 0x01, 0x0d,
  0x72, 0x65, 0x67, 0x75, 0x6c, 0x61, 0x72, 0x4d, 0x6f, 0x64, 0x75, 0x6c, 0x65, 0x01, 0x78, 0x00, 0x00,
]);
const mod = new WebAssembly.Module(bytes);

const thrown = new TypeError("boom from importObject's own regularModule getter");
const importObject = {
  get regularModule() {
    throw thrown;
  },
};

let caught;
try {
  new WebAssembly.Instance(mod, importObject);
} catch (e) {
  caught = e;
}
assertEq(caught, thrown, "a throwing importObject getter for a plain module name must propagate verbatim");

const componentThrown = new TypeError("boom from regularModule's own x getter");
caught = undefined;
try {
  new WebAssembly.Instance(mod, {
    regularModule: {
      get x() {
        throw componentThrown;
      },
    },
  });
} catch (e) {
  caught = e;
}
assertEq(caught, componentThrown, "a throwing getter for a component name must propagate verbatim");

globalThis.__wjiOk = true;
