// Targets two branches of WebAssembly.Exception.prototype.is/getArg never hit
// by the official corpus, found via esmeta.wji.WjiCoverage:
//
// 1. The "implements this Exception" brand check (spectec/document/
//    js-api/index.bs) on both methods -- the corpus only ever calls them on
//    a real Exception instance, never via Function.prototype.call with an
//    unrelated receiver.
// 2. getArg()'s own "the supplied tag doesn't match this exception's own
//    tag" check -- the corpus never calls getArg with a *different* Tag than
//    the one the Exception was actually constructed with.
//
// Confirmed against real Node 26.12.1 (messages differ, but all three throw
// a TypeError either way, which is all this checks).
function assertThrowsTypeError(fn, msg) {
  try {
    fn();
  } catch (e) {
    if (e instanceof TypeError) return;
    throw new Error(msg + ": expected TypeError, got " + e);
  }
  throw new Error(msg + ": expected a throw, got none");
}

const tag1 = new WebAssembly.Tag({ parameters: ["i32"] });
const tag2 = new WebAssembly.Tag({ parameters: ["i32"] });
const exn = new WebAssembly.Exception(tag1, [42]);

assertThrowsTypeError(
  () => WebAssembly.Exception.prototype.is.call({}, tag1),
  "Exception.prototype.is called on a non-Exception receiver",
);
assertThrowsTypeError(
  () => WebAssembly.Exception.prototype.getArg.call({}, tag1, 0),
  "Exception.prototype.getArg called on a non-Exception receiver",
);
assertThrowsTypeError(
  () => exn.getArg(tag2, 0),
  "getArg with a tag that doesn't match the exception's own tag",
);

globalThis.__wjiOk = true;
