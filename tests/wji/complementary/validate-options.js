// Targets `WebAssembly.validate`'s own `options` argument
// (spectec/document/js-api/index.bs): an explicitly passed
// WebAssemblyCompileOptions dictionary is converted like any other WebIDL
// dictionary, so a throwing member getter must propagate; and
// `importedStringConstants`, when given, is read off it. Found via
// esmeta.wji.WjiCoverage's annotated IR: both the conversion's abrupt branch
// and the "options has importedStringConstants" branch sat at 0 hits.
//
// Cross-checked with tests/wji/scripts/diff-engines: SpiderMonkey and JSC
// pass; V8 (including Node 24.2.0) never reads validate's options at all, so
// the getter never runs (docs/engine_bugs.md #15).
function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

const emptyModule = new Uint8Array([0, 97, 115, 109, 1, 0, 0, 0]);
const err = new Error("builtins getter");
let caught;
try {
  WebAssembly.validate(emptyModule, {
    get builtins() {
      throw err;
    },
  });
} catch (e) {
  caught = e;
}
assertEq(caught, err, "an options dictionary whose conversion throws must propagate that error");

// imported-strings-with-other-imports.wat: imports one string constant from
// the "'" module, plus an ordinary i32 global.
const bytes = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x09, 0x02, 0x60,
  0x00, 0x01, 0x6f, 0x60, 0x00, 0x01, 0x7f, 0x02, 0x15, 0x02, 0x01, 0x27,
  0x05, 0x68, 0x65, 0x6c, 0x6c, 0x6f, 0x03, 0x6f, 0x00, 0x03, 0x65, 0x6e,
  0x76, 0x01, 0x6e, 0x03, 0x7f, 0x00, 0x03, 0x03, 0x02, 0x00, 0x01, 0x07,
  0x09, 0x02, 0x01, 0x73, 0x00, 0x00, 0x01, 0x6e, 0x00, 0x01, 0x0a, 0x0b,
  0x02, 0x04, 0x00, 0x23, 0x00, 0x0b, 0x04, 0x00, 0x23, 0x01, 0x0b,
]);
assertEq(
  WebAssembly.validate(bytes, { importedStringConstants: "'" }),
  true,
  "validate accepts a module using the given importedStringConstants module",
);

globalThis.__wjiOk = true;
