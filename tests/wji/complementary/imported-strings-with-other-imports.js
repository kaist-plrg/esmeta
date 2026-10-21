// Targets `instantiate imported strings`'s "If moduleName does not equal
// importedStringModule, then continue." step (spectec/document/js-api/
// index.bs:475): a module compiled with `importedStringConstants` can still
// have ordinary imports from other module names, which this loop must skip
// (and which `read the imports` must then resolve from the real importObject
// as usual). Found via esmeta.wji.WjiCoverage's annotated IR: every official
// corpus module using importedStringConstants imports *only* string
// constants, so the skip branch sat at 0 hits.
//
// Confirmed against real Node 24.2.0.
function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + expected + ", got " + actual);
}

// imported-strings-with-other-imports.wat
const bytes = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x09, 0x02, 0x60,
  0x00, 0x01, 0x6f, 0x60, 0x00, 0x01, 0x7f, 0x02, 0x15, 0x02, 0x01, 0x27,
  0x05, 0x68, 0x65, 0x6c, 0x6c, 0x6f, 0x03, 0x6f, 0x00, 0x03, 0x65, 0x6e,
  0x76, 0x01, 0x6e, 0x03, 0x7f, 0x00, 0x03, 0x03, 0x02, 0x00, 0x01, 0x07,
  0x09, 0x02, 0x01, 0x73, 0x00, 0x00, 0x01, 0x6e, 0x00, 0x01, 0x0a, 0x0b,
  0x02, 0x04, 0x00, 0x23, 0x00, 0x0b, 0x04, 0x00, 0x23, 0x01, 0x0b,
]);

const mod = new WebAssembly.Module(bytes, { importedStringConstants: "'" });
const { s, n } = new WebAssembly.Instance(mod, { env: { n: 7 } }).exports;

assertEq(s(), "hello", "the string-constant import resolves to its own name");
assertEq(n(), 7, "the ordinary import still resolves from importObject");

globalThis.__wjiOk = true;
