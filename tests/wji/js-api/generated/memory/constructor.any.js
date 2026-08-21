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

function assert_function_name(fn, name, description) {
  const propdesc = Object.getOwnPropertyDescriptor(fn, "name");
  assert_equals(typeof propdesc, "object", `${description} should have name property`);
  assert_false(propdesc.writable, "writable", `${description} name should not be writable`);
  assert_false(propdesc.enumerable, "enumerable", `${description} name should not be enumerable`);
  assert_true(propdesc.configurable, "configurable", `${description} name should be configurable`);
  assert_equals(propdesc.value, name, `${description} name should be ${name}`);
}

function assert_function_length(fn, length, description) {
  const propdesc = Object.getOwnPropertyDescriptor(fn, "length");
  assert_equals(typeof propdesc, "object", `${description} should have length property`);
  assert_false(propdesc.writable, "writable", `${description} length should not be writable`);
  assert_false(propdesc.enumerable, "enumerable", `${description} length should not be enumerable`);
  assert_true(propdesc.configurable, "configurable", `${description} length should be configurable`);
  assert_equals(propdesc.value, length, `${description} length should be ${length}`);
}

function assert_exported_function(fn, { name, length }, description) {
  if (WebAssembly.Function === undefined) {
    assert_equals(Object.getPrototypeOf(fn), Function.prototype,
                  `${description}: prototype`);
  } else {
    assert_equals(Object.getPrototypeOf(fn), WebAssembly.Function.prototype,
                  `${description}: prototype`);
  }

  assert_function_name(fn, name, description);
  assert_function_length(fn, length, description);
}

function assert_Instance(instance, expected_exports) {
  assert_equals(Object.getPrototypeOf(instance), WebAssembly.Instance.prototype,
                "prototype");
  assert_true(Object.isExtensible(instance), "extensible");

  assert_equals(instance.exports, instance.exports, "exports should be idempotent");
  const exports = instance.exports;

  assert_equals(Object.getPrototypeOf(exports), null, "exports prototype");
  assert_false(Object.isExtensible(exports), "extensible exports");
  assert_array_equals(Object.keys(exports), Object.keys(expected_exports), "matching export keys");
  for (const [key, expected] of Object.entries(expected_exports)) {
    const property = Object.getOwnPropertyDescriptor(exports, key);
    assert_equals(typeof property, "object", `${key} should be present`);
    assert_false(property.writable, `${key}: writable`);
    assert_true(property.enumerable, `${key}: enumerable`);
    assert_false(property.configurable, `${key}: configurable`);
    const actual = property.value;
    assert_true(Object.isExtensible(actual), `${key}: extensible`);

    switch (expected.kind) {
    case "function":
      assert_exported_function(actual, expected, `value of ${key}`);
      break;
    case "global":
      assert_equals(Object.getPrototypeOf(actual), WebAssembly.Global.prototype,
                    `value of ${key}: prototype`);
      assert_equals(actual.value, expected.value, `value of ${key}: value`);
      assert_equals(actual.valueOf(), expected.value, `value of ${key}: valueOf()`);
      break;
    case "memory":
      assert_equals(Object.getPrototypeOf(actual), WebAssembly.Memory.prototype,
                    `value of ${key}: prototype`);
      assert_equals(Object.getPrototypeOf(actual.buffer), ArrayBuffer.prototype,
                    `value of ${key}: prototype of buffer`);
      assert_equals(actual.buffer.byteLength, 0x10000 * expected.size, `value of ${key}: size of buffer`);
      const array = new Uint8Array(actual.buffer);
      assert_equals(array[0], 0, `value of ${key}: first element of buffer`);
      assert_equals(array[array.byteLength - 1], 0, `value of ${key}: last element of buffer`);
      break;
    case "table":
      assert_equals(Object.getPrototypeOf(actual), WebAssembly.Table.prototype,
                    `value of ${key}: prototype`);
      assert_equals(actual.length, expected.length, `value of ${key}: length of table`);
      break;
    }
  }
}

function assert_WebAssemblyInstantiatedSource(actual, expected_exports={}) {
  assert_equals(Object.getPrototypeOf(actual), Object.prototype,
                "Prototype");
  assert_true(Object.isExtensible(actual), "Extensibility");

  const module = Object.getOwnPropertyDescriptor(actual, "module");
  assert_equals(typeof module, "object", "module: type of descriptor");
  assert_true(module.writable, "module: writable");
  assert_true(module.enumerable, "module: enumerable");
  assert_true(module.configurable, "module: configurable");
  assert_equals(Object.getPrototypeOf(module.value), WebAssembly.Module.prototype,
                "module: prototype");

  const instance = Object.getOwnPropertyDescriptor(actual, "instance");
  assert_equals(typeof instance, "object", "instance: type of descriptor");
  assert_true(instance.writable, "instance: writable");
  assert_true(instance.enumerable, "instance: enumerable");
  assert_true(instance.configurable, "instance: configurable");
  assert_Instance(instance.value, expected_exports);
}

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
  assert_function_name(WebAssembly.Memory, "Memory", "WebAssembly.Memory");
}, "name");

test(() => {
  assert_function_length(WebAssembly.Memory, 1, "WebAssembly.Memory");
}, "length");

test(() => {
  assert_throws_js(TypeError, () => new WebAssembly.Memory());
}, "No arguments");

test(() => {
  const argument = { "initial": 0 };
  assert_throws_js(TypeError, () => WebAssembly.Memory(argument));
}, "Calling");

test(() => {
  const invalidArguments = [
    undefined,
    null,
    false,
    true,
    "",
    "test",
    Symbol(),
    1,
    NaN,
    {},
  ];
  for (const invalidArgument of invalidArguments) {
    assert_throws_js(TypeError,
                     () => new WebAssembly.Memory(invalidArgument),
                     `new Memory(${format_value(invalidArgument)})`);
  }
}, "Invalid descriptor argument");

test(() => {
  assert_throws_js(TypeError, () => new WebAssembly.Memory({ "initial": undefined }));
}, "Undefined initial value in descriptor");

const outOfRangeValues = [
  NaN,
  Infinity,
  -Infinity,
  -1,
  0x100000000,
  0x1000000000,
];

for (const value of outOfRangeValues) {
  test(() => {
    assert_throws_js(TypeError, () => new WebAssembly.Memory({ "initial": value }));
  }, `Out-of-range initial value in descriptor: ${format_value(value)}`);

  test(() => {
    assert_throws_js(TypeError, () => new WebAssembly.Memory({ "initial": 0, "maximum": value }));
  }, `Out-of-range maximum value in descriptor: ${format_value(value)}`);
}

test(() => {
  assert_throws_js(RangeError, () => new WebAssembly.Memory({ "initial": 10, "maximum": 9 }));
}, "Initial value exceeds maximum");

test(() => {
  const proxy = new Proxy({}, {
    has(o, x) {
      assert_unreached(`Should not call [[HasProperty]] with ${x}`);
    },
    get(o, x) {
      // Due to the requirement not to supply both minimum and initial, we need to ignore one of them.
      switch (x) {
        case "shared":
          return false;
        case "initial":
        case "maximum":
          return 0;
        case "address":
          return "i32";
        default:
          return undefined;
      }
    },
  });
  new WebAssembly.Memory(proxy);
}, "Proxy descriptor");

test(() => {
  const order = [];

  new WebAssembly.Memory({
    get maximum() {
      order.push("maximum");
      return {
        valueOf() {
          order.push("maximum valueOf");
          return 1;
        },
      };
    },

    get initial() {
      order.push("initial");
      return {
        valueOf() {
          order.push("initial valueOf");
          return 1;
        },
      };
    },

    get address() {
      order.push("address");
      return {
        toString() {
          order.push("address toString");
          return "i32";
        },
      };
    },
  });

  assert_array_equals(order, [
    "address",
    "address toString",
    "initial",
    "initial valueOf",
    "maximum",
    "maximum valueOf",
  ]);
}, "Order of evaluation for descriptor");

test(() => {
  const argument = { "initial": 0 };
  const memory = new WebAssembly.Memory(argument);
  assert_Memory(memory, { "size": 0 });
}, "Zero initial");

test(() => {
  const argument = { "initial": 4 };
  const memory = new WebAssembly.Memory(argument);
  assert_Memory(memory, { "size": 4 });
}, "Non-zero initial");

test(() => {
  const argument = { "initial": 0 };
  const memory = new WebAssembly.Memory(argument, {});
  assert_Memory(memory, { "size": 0 });
}, "Stray argument");

test(() => {
  const argument = { "initial": 1 };
  const memory = new WebAssembly.Memory(argument);
  assert_Memory(memory, { "size": 1, "address": "i32" });
}, "Memory with address parameter omitted");

test(() => {
  const argument = { "initial": 1, "address": "i32" };
  const memory = new WebAssembly.Memory(argument);
  assert_Memory(memory, { "size": 1, "address": "i32" });
}, "Memory with i32 address constructor");

test(() => {
  const argument = { "initial": "3" };
  const memory = new WebAssembly.Memory(argument);
  assert_Memory(memory, { "size": 3 });
}, "Memory with string value for initial");


test(() => {
  const argument = { "initial": true };
  const memory = new WebAssembly.Memory(argument);
  assert_Memory(memory, { "size": 1 });
}, "Memory with boolean value for initial");

test(() => {
  assert_throws_js(TypeError, () => new WebAssembly.Memory({ "initial": 1, "address": "none" }));
}, "Unknown memory address");

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
