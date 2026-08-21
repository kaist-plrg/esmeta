// spectec/test/harness/testharness.js assumes a browser/worker global
// (`self`). Load this before testharness.js so it runs standalone in a bare
// JS shell (like WJI).
var self = globalThis;

// A from-scratch, minimal reimplementation of the slice of
// spectec/test/harness/testharness.js that tests/wji/js-api actually uses.
//
// The real testharness.js can't run under WJI: it uses a regex literal
// unconditionally in its own bottom-of-file setup (e.g. resolving its own
// script URL), and ESMeta's ECMA-262 mechanization doesn't implement RegExp
// literal evaluation at all (`Interpreter.scala`'s `RegularExpressionLiteral`
// case unconditionally throws `NotSupported`). A survey of every .any.js
// test, the shared per-category assertions.js helpers, and
// wasm-module-builder.js found zero regex usage outside testharness.js
// itself, and found only 12 of its ~30 assert_* primitives (plus
// test/promise_test/setup/format_value) actually called anywhere in the
// corpus -- so porting just those, algorithm-for-algorithm, is both
// necessary and sufficient. Load this right after shell-shim.js, before any
// META script or test file content.
(function () {
  const tests = [];
  let queue = Promise.resolve();

  function record(name, status, message) {
    tests.push({ name, status, message: message || null });
  }

  // mirrors testharness.js's format_value: same switch structure (string /
  // boolean+undefined / number incl. -0 / bigint / null, then a shared
  // `typeof val + ' "' + String(val) + '"'` fallback for object/function/
  // symbol/etc). Skips the seen-object cycle tracking, Node-specific cases,
  // and 1000-char truncation -- nothing in this corpus needs them.
  function format_value(val) {
    if (Array.isArray(val)) return "[" + val.map(format_value).join(", ") + "]";
    switch (typeof val) {
      case "string":
        return '"' + val + '"';
      case "boolean":
      case "undefined":
        return String(val);
      case "number":
        if (val === 0 && 1 / val === -Infinity) return "-0";
        return String(val);
      case "bigint":
        return String(val) + "n";
      case "object":
        if (val === null) return "null";
      /* falls through */
      default:
        try {
          return typeof val + ' "' + String(val) + '"';
        } catch (e) {
          return "[stringifying " + typeof val + " threw " + String(e) + "]";
        }
    }
  }
  globalThis.format_value = format_value;

  // mirrors testharness.js's make_message, without its regex-based
  // surrogate-sanitizing step (irrelevant for message text).
  function make_message(functionName, description, template, substitutions) {
    let msg = "" + template;
    for (const key in substitutions || {}) {
      msg = msg.split("${" + key + "}").join(format_value(substitutions[key]));
    }
    return (description ? description + ": " : "") + functionName + ": " + msg;
  }

  function AssertionError(message) {
    this.message = message;
  }
  AssertionError.prototype = Object.create(Error.prototype);

  function assert(expectedTrue, functionName, description, template, substitutions) {
    if (expectedTrue !== true) {
      throw new AssertionError(
        make_message(functionName, description, template, substitutions),
      );
    }
  }

  // mirrors testharness.js's same_value: SameValue semantics (distinguishes
  // +0/-0, treats NaN as equal to itself).
  function same_value(x, y) {
    if (y !== y) return x !== x;
    if (x === 0 && y === 0) return 1 / x === 1 / y;
    return x === y;
  }

  globalThis.assert_true = function (actual, description) {
    assert(actual === true, "assert_true", description, "expected true got ${actual}", { actual });
  };

  globalThis.assert_false = function (actual, description) {
    assert(actual === false, "assert_false", description, "expected false got ${actual}", { actual });
  };

  globalThis.assert_equals = function (actual, expected, description) {
    if (typeof actual !== typeof expected) {
      assert(
        false, "assert_equals", description,
        "expected (${expectedType}) ${expected} but got (${actualType}) ${actual}",
        { expected, actual, expectedType: typeof expected, actualType: typeof actual },
      );
      return;
    }
    assert(same_value(actual, expected), "assert_equals", description,
      "expected ${expected} but got ${actual}", { expected, actual });
  };

  globalThis.assert_not_equals = function (actual, expected, description) {
    assert(!same_value(actual, expected), "assert_not_equals", description,
      "got disallowed value ${actual}", { actual });
  };

  globalThis.assert_array_equals = function (actual, expected, description) {
    assert(
      typeof actual === "object" && actual !== null && "length" in actual,
      "assert_array_equals", description, "value is ${actual}, expected array", { actual },
    );
    assert(actual.length === expected.length, "assert_array_equals", description,
      "lengths differ, expected length ${expectedLength}, got length ${actualLength}",
      { expectedLength: expected.length, actualLength: actual.length });
    for (let i = 0; i < actual.length; i++) {
      assert(actual.hasOwnProperty(i) === expected.hasOwnProperty(i), "assert_array_equals", description,
        "property ${i} expected to be ${expected} but was ${actual}",
        { i, expected: expected.hasOwnProperty(i) ? "present" : "missing", actual: actual.hasOwnProperty(i) ? "present" : "missing" });
      assert(same_value(expected[i], actual[i]), "assert_array_equals", description,
        "property ${i} expected ${expected} but got ${actual}", { i, expected: expected[i], actual: actual[i] });
    }
  };

  globalThis.assert_class_string = function (object, class_string, description) {
    const actual = {}.toString.call(object);
    const expected = "[object " + class_string + "]";
    assert(same_value(actual, expected), "assert_class_string", description,
      "expected ${expected} but got ${actual}", { expected, actual });
  };

  globalThis.assert_own_property = function (object, property_name, description) {
    assert(object.hasOwnProperty(property_name), "assert_own_property", description,
      "expected property ${p} missing", { p: property_name });
  };

  globalThis.assert_not_own_property = function (object, property_name, description) {
    assert(!object.hasOwnProperty(property_name), "assert_not_own_property", description,
      "unexpected property ${p} is found on object", { p: property_name });
  };

  globalThis.assert_unreached = function (description) {
    assert(false, "assert_unreached", description, "Reached unreachable code");
  };

  // mirrors testharness.js's assert_throws_js_impl, minus its diagnostic-only
  // "is constructor an Error subtype" walk (doesn't affect pass/fail).
  function assert_throws_js_impl(constructor, func, description, assertionType) {
    try {
      func.call(undefined);
      assert(false, assertionType, description, "${func} did not throw", { func });
    } catch (e) {
      if (e instanceof AssertionError) throw e;
      assert(typeof e === "object" && e !== null, assertionType, description,
        "${func} threw a non-object value ${e}", { func, e });
      assert(e.constructor === constructor && e.name === constructor.name, assertionType, description,
        "${func} threw ${actual} (${actualName}) expected instance of ${expected} (${expectedName})",
        { func, actual: e, actualName: e.name, expected: constructor, expectedName: constructor.name });
    }
  }

  globalThis.assert_throws_js = function (constructor, func, description) {
    assert_throws_js_impl(constructor, func, description, "assert_throws_js");
  };

  globalThis.assert_throws_exactly = function (exception, func, description) {
    try {
      func.call(undefined);
      assert(false, "assert_throws_exactly", description, "${func} did not throw", { func });
    } catch (e) {
      if (e instanceof AssertionError) throw e;
      assert(same_value(e, exception), "assert_throws_exactly", description,
        "${func} threw ${e} but we expected it to throw ${exception}", { func, e, exception });
    }
  };

  globalThis.promise_rejects_js = function (test, constructor, promise, description) {
    return Promise.resolve(promise).then(
      (v) => {
        assert(false, "promise_rejects_js", description,
          "Should have rejected: promise resolved with ${v}", { v });
      },
      (e) => {
        assert_throws_js_impl(constructor, () => { throw e; }, description, "promise_rejects_js");
      },
    );
  };

  globalThis.setup = function (fn) {
    fn();
  };

  // mirrors testharness.js's Test-name-omitted fallback ("Untitled",
  // "Untitled 1", ...), but without its regex-based fallback of deriving a
  // name from a simple `() => expr` function's own source text -- every
  // unnamed test/promise_test in this corpus is multi-line, which real
  // testharness.js's own derivation regex rejects anyway (it requires no
  // line breaks in the source), so this fallback alone already matches it.
  let untitledCounter = 0;
  function nextDefaultTestName() {
    const suffix = untitledCounter > 0 ? " " + untitledCounter : "";
    untitledCounter++;
    return "Untitled" + suffix;
  }

  // only add_cleanup and unreached_func are used anywhere in this corpus --
  // real testharness.js's Test object has far more (step, done, timeout, ...)
  // that nothing here calls.
  function makeTestHandle() {
    const cleanups = [];
    return {
      add_cleanup: (fn) => cleanups.push(fn),
      unreached_func: (description) => () => {
        assert(false, "unreached_func", description, "Reached unreachable code");
      },
      cleanups,
    };
  }

  globalThis.test = function (fn, name) {
    const testName = name || nextDefaultTestName();
    const t = makeTestHandle();
    try {
      fn(t);
      record(testName, 0, null);
    } catch (e) {
      record(testName, 1, e instanceof AssertionError ? e.message : String((e && e.message) || e));
    } finally {
      for (const cleanup of t.cleanups) {
        try {
          cleanup();
        } catch (e) {}
      }
    }
  };

  // promise_test entries run strictly sequentially (each awaited before the
  // next starts), matching testharness.js -- some tests (e.g.
  // instance/constructor-caching.any.js) depend on that ordering.
  globalThis.promise_test = function (fn, name) {
    const testName = name || nextDefaultTestName();
    queue = queue.then(() => {
      const t = makeTestHandle();
      return Promise.resolve()
        .then(() => fn(t))
        .then(
          () => record(testName, 0, null),
          (e) => record(testName, 1, e instanceof AssertionError ? e.message : String((e && e.message) || e)),
        )
        .then(() => Promise.all(t.cleanups.map((c) => c())))
        .catch((e) => record(testName, 1, "cleanup failed: " + String((e && e.message) || e)));
    });
  };

  // real testharness.js has no jsshell output of its own -- see
  // report-shim.js, which should load LAST (after the test file's own
  // content): add_completion_callback captures the *current* `queue` chain,
  // so anything registering it must run after every promise_test() call.
  globalThis.add_completion_callback = function (cb) {
    queue.then(() => cb(tests, { status: 0, message: null }));
  };

  // wasm-module-builder.js's debug logging (`if (debug) print(...)`) is the
  // only other consumer of `print` in this corpus (debug is off by default).
  if (typeof globalThis.print !== "function") globalThis.print = console.log.bind(console);
})();



test(() => {
  const thisValues = [
    undefined,
    null,
    true,
    "",
    Symbol(),
    1,
    {},
    WebAssembly.Table,
    WebAssembly.Table.prototype,
  ];

  const desc = Object.getOwnPropertyDescriptor(WebAssembly.Table.prototype, "length");
  assert_equals(typeof desc, "object");

  const getter = desc.get;
  assert_equals(typeof getter, "function");

  assert_equals(typeof desc.set, "undefined");

  for (const thisValue of thisValues) {
    assert_throws_js(TypeError, () => getter.call(thisValue), `this=${format_value(thisValue)}`);
  }
}, "Branding");

test(() => {
  const argument = { "element": "anyfunc", "initial": 2 };
  const table = new WebAssembly.Table(argument);
  assert_equals(table.length, 2, "Initial length");

  const desc = Object.getOwnPropertyDescriptor(WebAssembly.Table.prototype, "length");
  assert_equals(typeof desc, "object");

  const getter = desc.get;
  assert_equals(typeof getter, "function");

  assert_equals(getter.call(table, {}), 2);
}, "Stray argument");

test(() => {
  const argument = { "element": "anyfunc", "initial": 2 };
  const table = new WebAssembly.Table(argument);
  assert_equals(table.length, 2, "Initial length");
  table.length = 4;
  assert_equals(table.length, 2, "Should not change the length");
}, "Setting (sloppy mode)");

test(() => {
  const argument = { "element": "anyfunc", "initial": 2 };
  const table = new WebAssembly.Table(argument);
  assert_equals(table.length, 2, "Initial length");
  assert_throws_js(TypeError, () => {
    "use strict";
    table.length = 4;
  });
  assert_equals(table.length, 2, "Should not change the length");
}, "Setting (strict mode)");

// Prints each subtest's pass/fail once the whole file's tests finish, and
// sets globalThis.__wjiOk to whether every subtest passed -- reusing the same
// convention tests/wji/manual/*.js fixtures use (see EvalSpec/WjiTest), so a
// whole js-api file is judged as one WJI eval test: pass iff every subtest
// in it passed. Load this LAST -- after shell-shim.js, testharness-lite.js,
// any META scripts, and the test file's own content. testharness-lite.js's
// add_completion_callback captures the *current* promise_test queue when
// called, so registering it before every promise_test() call has run would
// miss the later ones.
add_completion_callback((tests, harness_status) => {
  for (const t of tests) {
    print((t.status === 0 ? "PASS" : "FAIL") + " " + t.name + (t.message ? " - " + t.message : ""));
  }
  globalThis.__wjiOk = tests.length > 0 && tests.every((t) => t.status === 0);
});
