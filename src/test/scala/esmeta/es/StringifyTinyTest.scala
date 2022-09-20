package esmeta.es

import esmeta.es.util.injector.*
import esmeta.state.*

/** stringify test */
class StringifyTinyTest extends ESTest {
  val name = "esStringifyTest"

  def init: Unit = {
    lazy val addr = NamedAddr("foo")
    lazy val path = "path"
    lazy val origPath = "origPath"

    checkStringify("SameValue")(
      hasValue1 -> "$assert.sameValue(x, 1.0);",
      hasValue2 -> "$assert.sameValue(x, 1n);",
      hasValue3 -> "$assert.sameValue(x, \"bar\");",
      hasValue4 -> "$assert.sameValue(x, true);",
      hasValue5 -> "$assert.sameValue(x, undefined);",
      hasValue6 -> "$assert.sameValue(x, null);",
      hasValue7 -> "$assert.sameValue(x, absent);",
    )

    checkStringify("IsExtensible")(
      isExtensible1 -> "$assert.sameValue(Object.isExtensible(path), true);",
      isExtensible2 -> "$assert.sameValue(Object.isExtensible(path), false);",
    )

    checkStringify("IsCallable")(
      isCallable1 -> "$assert.callable(path);",
      isCallable2 -> "$assert.notCallable(path);",
    )

    checkStringify("IsConstructable")(
      isConstructable1 -> "$assert.constructable(path);",
      isConstructable2 -> "$assert.notConstructable(path);",
    )

    checkStringify("CompareArray")(
      compareArray1 -> "$assert.compareArray(Reflect.ownKeys(path), [0, 1, 2], path);",
    )

    checkStringify("SameObject")(
      sameObject1 -> "$assert.sameValue(path, origPath);",
    )

    checkStringify("VerifyProperty")(
      verifyProperty1 ->
      """$verifyProperty(path, baz, {
          |  foo: "bar",
          |});""".stripMargin,
    )

    checkStringify("ConformTest")(
      conformTest1 -> """// [EXIT] normal
                        |"use strict";
                        |
                        |// hidden constructors
                        |var AsyncArrowFunction = Object.getPrototypeOf(async () => {}).constructor;
                        |var AsyncFunction = Object.getPrototypeOf(async function () {}).constructor;
                        |var AsyncGeneratorFunction = Object.getPrototypeOf(
                        |  async function* () {}
                        |).constructor;
                        |var GeneratorFunction = Object.getPrototypeOf(function* () {}).constructor;
                        |
                        |// logging errors
                        |var $error = (globalThis.console && globalThis.console.log) || globalThis.print;
                        |
                        |// conversion to string
                        |function $toString(value) {
                        |  if (value === 0 && 1 / value === -Infinity) return "«-0»";
                        |  if (value instanceof Error) return "a " + value.constructor.name;
                        |  if (typeof value === "string") return '"' + value + '"';
                        |  return String(value);
                        |}
                        |
                        |function $isSameValue(x, y) {
                        |  if (x === y) return x !== 0 || 1 / x === 1 / y;
                        |  return x !== x && y !== y;
                        |}
                        |
                        |// assertion
                        |function $assert(mustBeTrue) {
                        |  if (mustBeTrue === true) return;
                        |  $error("Expected true but got " + $toString(mustBeTrue));
                        |}
                        |
                        |// assertion for comparing two thrown values
                        |$assert.sameThrows = function (thrown, expected) {
                        |  var thrownStr = $toString(thrown);
                        |  if (typeof expected !== "function") {
                        |    if (thrown !== expected)
                        |      $error("Expected " + $toString(expected) + " but got " + thrownStr);
                        |  } else if (!(thrown instanceof expected)) {
                        |    $error("Expected a " + expected.name + " but got " + thrownStr);
                        |  }
                        |};
                        |
                        |// assertion for thrown values that were expected to be thrown but not thrown
                        |$assert.shouldveThrown = function (expected) {
                        |  $error(
                        |    "Expected a " +
                        |      expected.name +
                        |      " to be thrown but no exception was thrown at all"
                        |  );
                        |};
                        |
                        |// assertion for no exception
                        |$assert.notThrows = function (func) {
                        |  if (typeof func !== "function") {
                        |    $error("$assert.notThrows requires a function.");
                        |    return;
                        |  }
                        |  try {
                        |    func();
                        |  } catch (thrown) {
                        |    $error("Expected no exception but " + $toString(thrown) + " is thrown.");
                        |    return;
                        |  }
                        |};
                        |
                        |// assertion for same values
                        |$assert.sameValue = function (actual, expected) {
                        |  if ($isSameValue(actual, expected)) return;
                        |  $error(
                        |    "Expected " + $toString(expected) + " but got " + $toString(actual) + "."
                        |  );
                        |};
                        |
                        |// assertion for same values
                        |$assert.notSameValue = function (actual, unexpected) {
                        |  if (!$isSameValue(actual, unexpected)) return;
                        |  $error(
                        |    "Not expected " +
                        |      $toString(unexpected) +
                        |      " but got " +
                        |      $toString(actual) +
                        |      "."
                        |  );
                        |};
                        |
                        |// assertion for [[Call]]
                        |$assert.isCallable = function (f) {
                        |  return typeof f === "function";
                        |};
                        |$assert.callable = function (f) {
                        |  if (!$assert.isCallable(f))
                        |    $error("Expected " + $toString(f) + " has [[Call]] but does not.");
                        |};
                        |$assert.notCallable = function (f) {
                        |  if ($assert.isCallable(f))
                        |    $error("Expected " + $toString(f) + " does not have [[Call]] but does.");
                        |};
                        |
                        |// assertion for [[Construct]]
                        |$assert.isConstructable = function (f) {
                        |  try {
                        |    Reflect.construct(function () {}, [], f);
                        |    return true;
                        |  } catch (e) {
                        |    return false;
                        |  }
                        |};
                        |$assert.constructable = function (f) {
                        |  if (!$assert.isConstructable(f))
                        |    $error("Expected " + $toString(f) + " has [[Construct]] but does not.");
                        |};
                        |$assert.notConstructable = function (f) {
                        |  if ($assert.isConstructable(f))
                        |    $error(
                        |      "Expected " + $toString(f) + " does not have [[Construct]] but does."
                        |    );
                        |};
                        |
                        |// assertion to compare arrays
                        |function $compareArray(a, b) {
                        |  if (b.length !== a.length) return false;
                        |  for (var i = 0; i < a.length; i++) {
                        |    if (!$isSameValue(a[i], b[i])) return false;
                        |  }
                        |  return true;
                        |}
                        |
                        |$assert.compareArray = function (actual, expected, obj) {
                        |  function format(array) {
                        |    return "[" + array.map($toString).join(", ") + "]";
                        |  }
                        |  function getObjDesc(obj) {
                        |    var ty = Object.prototype.toString.call(obj);
                        |    return ty.substring("[object ".length, ty.length - "]".length);
                        |  }
                        |  if ($compareArray(actual, expected)) return;
                        |  $error(
                        |    "Expected " +
                        |      format(expected) +
                        |      " but got " +
                        |      format(actual) +
                        |      " for " +
                        |      getObjDesc(obj) +
                        |      "."
                        |  );
                        |};
                        |
                        |// assertion to compare iterators
                        |$assert.compareIterator = function (iter, validators) {
                        |  var i, result;
                        |  for (i = 0; i < validators.length; i++) {
                        |    result = iter.next();
                        |    $error(
                        |      !result.done,
                        |      "Expected " +
                        |        i +
                        |        " values(s). Instead iterator only produced " +
                        |        (i - 1) +
                        |        " value(s)."
                        |    );
                        |    validators[i](result.value);
                        |  }
                        |  result = iter.next();
                        |  $error(
                        |    result.done,
                        |    "Expected only " + i + " values(s). Instead iterator produced more."
                        |  );
                        |  $assert.sameValue(
                        |    result.value,
                        |    undefined,
                        |    "Expected value of `undefined` when iterator completes."
                        |  );
                        |};
                        |
                        |// verify properties
                        |function $verifyProperty(obj, prop, desc) {
                        |  // check property type
                        |  var propType = typeof prop;
                        |  if (propType !== "string" && propType !== "symbol") {
                        |    $error(
                        |      "$verifyProperty requires a string or symbol property but " +
                        |        $toString(prop) +
                        |        " given."
                        |    );
                        |    return;
                        |  }
                        |
                        |  var originalDesc = Object.getOwnPropertyDescriptor(obj, prop);
                        |
                        |  // Allows checking for undefined descriptor if it's explicitly given.
                        |  if (desc === undefined) {
                        |    $assert.sameValue(originalDesc, undefined);
                        |    return;
                        |  }
                        |
                        |  var hasOwnProperty = Object.prototype.hasOwnProperty;
                        |  $assert(hasOwnProperty.call(obj, prop));
                        |  $assert.notSameValue(desc, null);
                        |  $assert.sameValue(typeof desc, "object");
                        |
                        |  function check(name) {
                        |    try {
                        |      if (!hasOwnProperty.call(desc, name)) return;
                        |      if ($isSameValue(desc[name], originalDesc[name])) return;
                        |      var message;
                        |      if (name === "value")
                        |        message =
                        |          "descriptor value of " + prop + " should be " +
                        |          $toString(desc.value) +
                        |          " but " +
                        |          $toString(originalDesc.value);
                        |      else
                        |        message =
                        |          "descriptor should " + (desc[name] ? "" : "not ") + "be " + name;
                        |      $error(message);
                        |    } catch (e) {}
                        |  }
                        |  check("value");
                        |  check("writable");
                        |  check("enumerable");
                        |  check("configurable");
                        |}
                        |
                        |// delay checking assertions
                        |function $delay(f) {
                        |  var DELAY = 100;
                        |  var setTimeout = globalThis.setTimeout;
                        |  import("os")
                        |    .then((os) => {
                        |      // qjs
                        |      if (!setTimeout) setTimeout = os?.setTimeout;
                        |    })
                        |    .catch(() => {})
                        |    .finally(() => {
                        |      setTimeout(f, DELAY);
                        |    });
                        |}
                        |
                        |// Script
                        |$delay(() => {
                        |$assert.sameValue(x, 1.0);
                        |$assert.sameValue(Object.isExtensible(path), true);
                        |$assert.callable(path);
                        |});""".stripMargin,
      conformTest2 -> """// [EXIT] normal
                        |// Script
                        |$delay(() => {
                        |$assert.sameValue(x, 1.0);
                        |$assert.sameValue(Object.isExtensible(path), true);
                        |$assert.callable(path);
                        |});""".stripMargin,
      conformTest3 -> """// [EXIT] normal
                        |// Script
                        |$assert.sameValue(x, 1.0);""".stripMargin,
    )

    // -------------------------------------------------------------------------
    // Assertions
    // -------------------------------------------------------------------------
    lazy val hasValue1 = HasValue("x", Number(1.0))
    lazy val hasValue2 = HasValue("x", BigInt(1))
    lazy val hasValue3 = HasValue("x", Str("bar"))
    lazy val hasValue4 = HasValue("x", Bool(true))
    lazy val hasValue5 = HasValue("x", Undef)
    lazy val hasValue6 = HasValue("x", Null)
    lazy val hasValue7 = HasValue("x", Absent)
    lazy val isExtensible1 = IsExtensible(addr, path, true)
    lazy val isExtensible2 = IsExtensible(addr, path, false)
    lazy val isCallable1 = IsCallable(addr, path, true)
    lazy val isCallable2 = IsCallable(addr, path, false)
    lazy val isConstructable1 = IsConstructable(addr, path, true)
    lazy val isConstructable2 = IsConstructable(addr, path, false)
    lazy val compareArray1 = CompareArray(addr, path, List(0, 1, 2))
    lazy val sameObject1 = SameObject(addr, path, origPath)
    lazy val verifyProperty1 =
      VerifyProperty(addr, path, "baz", Map("foo" -> Str("bar")))

    // -------------------------------------------------------------------------
    // ConformTests
    // -------------------------------------------------------------------------
    lazy val conformTest1 = ConformTest(
      0,
      "// Script",
      NormalTag,
      true,
      true,
      Vector(hasValue1, isExtensible1, isCallable1),
    )
    lazy val conformTest2 = ConformTest(
      0,
      "// Script",
      NormalTag,
      false,
      true,
      Vector(hasValue1, isExtensible1, isCallable1),
    )
    lazy val conformTest3 = ConformTest(
      0,
      "// Script",
      NormalTag,
      false,
      false,
      Vector(hasValue1),
    )
  }

  init
}
