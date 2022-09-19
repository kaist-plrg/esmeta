package esmeta.fuzzer.util

import esmeta.fuzzer.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

/** stringifier for fuzzer */
object Stringifier {

  // elements
  given elemRule: Rule[FuzzerElem] = (app, elem) =>
    elem match
      case elem: Test      => testRule(app, elem)
      case elem: Assertion => assertRule(app, elem)

  // conformance tests
  given testRule: Rule[Test] = (app, test) => ???

  // assertions
  given assertRule: Rule[Assertion] = (app, assert) =>
    assert match
      case _ => ???
}
