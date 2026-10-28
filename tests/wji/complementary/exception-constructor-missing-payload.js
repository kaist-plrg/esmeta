// Targets the "too few arguments" branch of WebAssembly.Exception's
// constructor (spectec/document/js-api/index.bs's `Exception(Tag
// exceptionTag, sequence<any> payload, optional ExceptionOptions options =
// {})`): `payload` has no `optional` keyword, so calling the constructor
// with only the required `exceptionTag` argument (no `payload` at all) must
// throw a TypeError before ever reaching the rest of the algorithm. Found
// via esmeta.wji.WjiCoverage's annotated IR: the argument-count check inside
// `INTRINSICS.WebAssembly.Exception` has 0 hits on its "fewer than 2
// arguments" side -- every exception/*.any.js subtest always supplies a
// payload (even an empty array).
//
// Confirmed against real Node 26.12.1: `new WebAssembly.Exception(tag)`
// throws a TypeError (message differs -- V8 treats the omitted `payload` as
// `undefined` and reports "must be an iterable object" rather than WebIDL's
// own "not enough arguments", but the observable TypeError is the same
// either way, which is all this checks).
function assertThrowsTypeError(fn, msg) {
  try {
    fn();
  } catch (e) {
    if (e instanceof TypeError) return;
    throw new Error(msg + ": expected TypeError, got " + e);
  }
  throw new Error(msg + ": expected a throw, got none");
}

const tag = new WebAssembly.Tag({ parameters: ["i32"] });
assertThrowsTypeError(
  () => new WebAssembly.Exception(tag),
  "Exception constructor with only the exceptionTag argument (no payload)",
);

globalThis.__wjiOk = true;
