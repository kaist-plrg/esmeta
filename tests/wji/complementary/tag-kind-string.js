// Targets the "tag" case of `string_value_of_the_extern_type`
// (spectec/document/js-api/index.bs, shared helper behind
// `Module.prototype.exports`/`.imports`'s own `ModuleExportDescriptor`/
// `ModuleImportDescriptor` -- maps a core-wasm externtype into the kind
// string "function"/"table"/"memory"/"global"/"tag"). Found via
// esmeta.wji.WjiCoverage's annotated IR: every other case was already
// exercised, but "tag" sat at 0/2 nodes -- the official corpus never
// imports or exports a tag by itself (`tag/constructor.tentative.any.js`
// only ever constructs a `WebAssembly.Tag` directly, never through a wasm
// module's own import/export table).
//
// Built the .wat by hand (see tag-kind-string.wat):
//   (module
//     (import "m" "t" (tag $imported (param i32)))
//     (tag $exported (param i32))
//     (export "t" (tag $exported)))
// Confirmed against real Node 24.2.0: both Module.imports/exports report
// `kind: "tag"` for these.
function assertEq(actual, expected, msg) {
  if (actual !== expected)
    throw new Error(msg + ": expected " + JSON.stringify(expected) + ", got " + JSON.stringify(actual));
}

const bytes = new Uint8Array([
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00, 0x01, 0x05, 0x01, 0x60,
  0x01, 0x7f, 0x00, 0x02, 0x08, 0x01, 0x01, 0x6d, 0x01, 0x74, 0x04, 0x00,
  0x00, 0x0d, 0x03, 0x01, 0x00, 0x00, 0x07, 0x05, 0x01, 0x01, 0x74, 0x04,
  0x01,
]);

const module_ = new WebAssembly.Module(bytes);

const imports = WebAssembly.Module.imports(module_);
assertEq(imports.length, 1, "one import");
assertEq(imports[0].module, "m", "import module name");
assertEq(imports[0].name, "t", "import field name");
assertEq(imports[0].kind, "tag", "imported tag's kind string");

const exports = WebAssembly.Module.exports(module_);
assertEq(exports.length, 1, "one export");
assertEq(exports[0].name, "t", "export field name");
assertEq(exports[0].kind, "tag", "exported tag's kind string");

globalThis.__wjiOk = true;
