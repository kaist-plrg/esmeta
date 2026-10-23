// Targets {{Exception}}'s constructor (spectec/document/js-api/index.bs)
// beyond the missing-payload case (exception-constructor-missing-payload.js):
//   - "If |types|'s size is not |payload|'s size, throw a TypeError."
//   - "If |resultType| matches v128 or exnref, throw a TypeError."
//   - the payload value's own `ToWebAssemblyValue` conversion throwing,
//   - an explicitly passed `options` dictionary, including one whose
//     conversion itself throws.
// Found via esmeta.wji.WjiCoverage's annotated IR: all of these sat at 0
// hits.
//
// Confirmed against real Node 26.12.1.
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

const i32Tag = new WebAssembly.Tag({ parameters: ["i32"] });

assertEq(
  thrown(() => new WebAssembly.Exception(i32Tag, [])) instanceof TypeError,
  true,
  "a payload shorter than the tag's parameters must throw TypeError",
);

const v128Tag = new WebAssembly.Tag({ parameters: ["v128"] });
assertEq(
  thrown(() => new WebAssembly.Exception(v128Tag, [0])) instanceof TypeError,
  true,
  "a v128 tag parameter must throw TypeError",
);

const err = new Error("valueOf");
assertEq(
  thrown(() => new WebAssembly.Exception(i32Tag, [{ valueOf() { throw err; } }])),
  err,
  "a payload value whose conversion throws must propagate that error",
);

const exn = new WebAssembly.Exception(i32Tag, [7], { traceStack: false });
assertEq(exn.getArg(i32Tag, 0), 7, "an explicit options dictionary is accepted");

const optionsErr = new Error("traceStack");
assertEq(
  thrown(() =>
    new WebAssembly.Exception(i32Tag, [7], {
      get traceStack() {
        throw optionsErr;
      },
    }),
  ),
  optionsErr,
  "an options dictionary whose conversion throws must propagate that error",
);

globalThis.__wjiOk = true;
