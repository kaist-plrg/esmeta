package esmeta.test262

import esmeta.ESMetaTest.cfg
import esmeta.es.util.withCFG

class EvalLargeTest extends Test262Test {
  val name: String = "test262EvalTest"

  // registration
  def init: Unit = check(name) {
    withCFG(cfg) {
      val summary = Test262Test.test262.evalTest(
        log = true,
        useProgress = true,
        useCoverage = true,
        timeLimit = Some(10),
      )
      val f = summary.fail
      if (f > 0) fail(s"$f tests are failed (See `$logDir/fail.log`).")
    }
  }

  init
}
