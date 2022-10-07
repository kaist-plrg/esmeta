package esmeta.test262

import esmeta.*
import esmeta.ESMetaTest.cfg
import esmeta.es.util.withCFG
import esmeta.util.BaseUtils.*

class ParseLargeTest extends Test262Test {
  val name: String = "test262ParseTest"

  // log directory
  lazy val logDir = s"$TEST262TEST_LOG_DIR/parse-$dateStr"

  // registration
  def init: Unit = check(name) {
    withCFG(cfg) {
      val summary = Test262Test.test262.parseTest(
        log = true,
        useProgress = true,
        timeLimit = Some(100),
      )
      val f = summary.fail
      if (f > 0) fail(s"$f tests are failed (See `$logDir/fail.log`).")
    }
  }
  init
}
