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

function assert_ArrayBuffer(actual, { size=0, shared=false, detached=false }, message) {
  // https://github.com/WebAssembly/spec/issues/840
  // See https://github.com/whatwg/html/issues/5380 for why not `self.SharedArrayBuffer`
  const isShared = !("isView" in actual.constructor);
  assert_equals(isShared, shared, `${message}: constructor`);
  const sharedString = shared ? "Shared" : "";
  assert_equals(actual.toString(), `[object ${sharedString}ArrayBuffer]`, `${message}: toString()`);
  assert_equals(Object.getPrototypeOf(actual).toString(), `[object ${sharedString}ArrayBuffer]`, `${message}: prototype toString()`);
  if (detached) {
    // https://github.com/tc39/ecma262/issues/678
    let byteLength;
    try {
      byteLength = actual.byteLength;
    } catch (e) {
      byteLength = 0;
    }
    assert_equals(byteLength, 0, `${message}: detached size`);
  } else {
    assert_equals(actual.byteLength, 0x10000 * size, `${message}: size`);
    if (size > 0) {
      const array = new Uint8Array(actual);
      assert_equals(array[0], 0, `${message}: first element`);
      assert_equals(array[array.byteLength - 1], 0, `${message}: last element`);
    }
  }
  assert_equals(Object.isFrozen(actual), shared, "buffer frozen");
  assert_equals(Object.isExtensible(actual), !shared, "buffer extensibility");
}

function assert_Memory(memory, { size=0, shared=false, address="i32" }) {
  assert_equals(Object.getPrototypeOf(memory), WebAssembly.Memory.prototype,
                "prototype");
  assert_true(Object.isExtensible(memory), "extensible");

  // https://github.com/WebAssembly/spec/issues/840
  assert_equals(memory.buffer, memory.buffer, "buffer should be idempotent");
  assert_ArrayBuffer(memory.buffer, { size, shared });

  // this depends on js-types proposal implementation
  if (typeof memory.type == "function") {
    assert_equals(memory.type().address, address, "memory address type");
  }
}


test(() => {
  const argument = { "initial": 0 };
  const memory = new WebAssembly.Memory(argument);
  assert_throws_js(TypeError, () => memory.grow());
}, "Missing arguments");

test(t => {
  const thisValues = [
    undefined,
    null,
    true,
    "",
    Symbol(),
    1,
    {},
    WebAssembly.Memory,
    WebAssembly.Memory.prototype,
  ];

  const argument = {
    valueOf: t.unreached_func("Should not touch the argument (valueOf)"),
    toString: t.unreached_func("Should not touch the argument (toString)"),
  };

  const fn = WebAssembly.Memory.prototype.grow;

  for (const thisValue of thisValues) {
    assert_throws_js(TypeError, () => fn.call(thisValue, argument), `this=${format_value(thisValue)}`);
  }
}, "Branding");

test(() => {
  const argument = { "initial": 0 };
  const memory = new WebAssembly.Memory(argument);
  const oldMemory = memory.buffer;
  assert_ArrayBuffer(oldMemory, { "size": 0 }, "Buffer before growing");

  const result = memory.grow(2);
  assert_equals(result, 0);

  const newMemory = memory.buffer;
  assert_not_equals(oldMemory, newMemory);
  assert_ArrayBuffer(oldMemory, { "detached": true }, "Old buffer after growing");
  assert_ArrayBuffer(newMemory, { "size": 2 }, "New buffer after growing");
}, "Zero initial");

test(() => {
  const argument = { "initial": { valueOf() { return 0 } } };
  const memory = new WebAssembly.Memory(argument);
  const oldMemory = memory.buffer;
  assert_ArrayBuffer(oldMemory, { "size": 0 }, "Buffer before growing");

  const result = memory.grow({ valueOf() { return 2 } });
  assert_equals(result, 0);

  const newMemory = memory.buffer;
  assert_not_equals(oldMemory, newMemory);
  assert_ArrayBuffer(oldMemory, { "detached": true }, "Old buffer after growing");
  assert_ArrayBuffer(newMemory, { "size": 2 }, "New buffer after growing");
}, "Zero initial with valueOf");

test(() => {
  const argument = { "initial": 3 };
  const memory = new WebAssembly.Memory(argument);
  const oldMemory = memory.buffer;
  assert_ArrayBuffer(oldMemory, { "size": 3 }, "Buffer before growing");

  const result = memory.grow(2);
  assert_equals(result, 3);

  const newMemory = memory.buffer;
  assert_not_equals(oldMemory, newMemory);
  assert_ArrayBuffer(oldMemory, { "detached": true }, "Old buffer after growing");
  assert_ArrayBuffer(newMemory, { "size": 5 }, "New buffer after growing");
}, "Non-zero initial");

test(() => {
  const argument = { "initial": 0, "maximum": 2 };
  const memory = new WebAssembly.Memory(argument);
  const oldMemory = memory.buffer;
  assert_ArrayBuffer(oldMemory, { "size": 0 }, "Buffer before growing");

  const result = memory.grow(2);
  assert_equals(result, 0);

  const newMemory = memory.buffer;
  assert_not_equals(oldMemory, newMemory);
  assert_ArrayBuffer(oldMemory, { "detached": true }, "Old buffer after growing");
  assert_ArrayBuffer(newMemory, { "size": 2 }, "New buffer after growing");
}, "Zero initial with respected maximum");

test(() => {
  const argument = { "initial": 0, "maximum": 2 };
  const memory = new WebAssembly.Memory(argument);
  const oldMemory = memory.buffer;
  assert_ArrayBuffer(oldMemory, { "size": 0 }, "Buffer before growing");

  const result = memory.grow(1);
  assert_equals(result, 0);

  const newMemory = memory.buffer;
  assert_not_equals(oldMemory, newMemory);
  assert_ArrayBuffer(oldMemory, { "detached": true }, "Old buffer after growing once");
  assert_ArrayBuffer(newMemory, { "size": 1 }, "New buffer after growing once");

  const result2 = memory.grow(1);
  assert_equals(result2, 1);

  const newestMemory = memory.buffer;
  assert_not_equals(newMemory, newestMemory);
  assert_ArrayBuffer(oldMemory, { "detached": true }, "New buffer after growing twice");
  assert_ArrayBuffer(newMemory, { "detached": true }, "New buffer after growing twice");
  assert_ArrayBuffer(newestMemory, { "size": 2 }, "Newest buffer after growing twice");
}, "Zero initial with respected maximum grown twice");

test(() => {
  const argument = { "initial": 1, "maximum": 2 };
  const memory = new WebAssembly.Memory(argument);
  const oldMemory = memory.buffer;
  assert_ArrayBuffer(oldMemory, { "size": 1 }, "Buffer before growing");

  assert_throws_js(RangeError, () => memory.grow(2));
  assert_equals(memory.buffer, oldMemory);
  assert_ArrayBuffer(memory.buffer, { "size": 1 }, "Buffer before trying to grow");
}, "Zero initial growing too much");

const outOfRangeValues = [
  undefined,
  NaN,
  Infinity,
  -Infinity,
  -1,
  0x100000000,
  0x1000000000,
  "0x100000000",
  { valueOf() { return 0x100000000; } },
];

for (const value of outOfRangeValues) {
  test(() => {
    const argument = { "initial": 0 };
    const memory = new WebAssembly.Memory(argument);
    assert_throws_js(TypeError, () => memory.grow(value));
  }, `Out-of-range argument: ${format_value(value)}`);
}

test(() => {
  const argument = { "initial": 0 };
  const memory = new WebAssembly.Memory(argument);
  const oldMemory = memory.buffer;
  assert_ArrayBuffer(oldMemory, { "size": 0 }, "Buffer before growing");

  const result = memory.grow(2, {});
  assert_equals(result, 0);

  const newMemory = memory.buffer;
  assert_not_equals(oldMemory, newMemory);
  assert_ArrayBuffer(oldMemory, { "detached": true }, "Old buffer after growing");
  assert_ArrayBuffer(newMemory, { "size": 2 }, "New buffer after growing");
}, "Stray argument");

test(() => {
  const argument = { "initial": 1, "maximum": 2, "shared": true };
  const memory = new WebAssembly.Memory(argument);
  const oldMemory = memory.buffer;
  assert_ArrayBuffer(oldMemory, { "size": 1, "shared": true }, "Buffer before growing");

  const result = memory.grow(1);
  assert_equals(result, 1);

  const newMemory = memory.buffer;
  assert_not_equals(oldMemory, newMemory);
  assert_ArrayBuffer(oldMemory, { "size": 1, "shared": true }, "Old buffer after growing");
  assert_ArrayBuffer(newMemory, { "size": 2, "shared": true }, "New buffer after growing");

  // The old and new buffers must have the same value for the
  // [[ArrayBufferData]] internal slot.
  const oldArray = new Uint8Array(oldMemory);
  const newArray = new Uint8Array(newMemory);
  assert_equals(oldArray[0], 0, "old first element");
  assert_equals(newArray[0], 0, "new first element");
  oldArray[0] = 1;
  assert_equals(oldArray[0], 1, "old first element");
  assert_equals(newArray[0], 1, "new first element");

}, "Growing shared memory does not detach old buffer");

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
