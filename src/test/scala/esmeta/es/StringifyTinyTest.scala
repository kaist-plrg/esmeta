package esmeta.es

import esmeta.*
import esmeta.es.util.injector.*
import esmeta.es.util.injector.Injector.*
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
      conformTest1 -> s"""// [EXIT] normal
                          |${template
        .replace(
          scriptPlaceholder,
          "// Script",
        )
        .replace(
          libPlaceholder,
          lib,
        )
        .replace(
          assertionPlaceholder,
          """$delay(() => {
            |$assert.sameValue(x, 1.0);
            |$assert.sameValue(Object.isExtensible(path), true);
            |$assert.callable(path);
            |});""",
        )}""".stripMargin,
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
