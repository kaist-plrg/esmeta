package esmeta.es

import esmeta.{ESMetaTest, ES_TEST_DIR}
import esmeta.util.SystemUtils.*

class TransCheckSmallTest extends ESTest {
  import ESTest.*

  val name: String = "esTransCheckTest"

  // registration
  def init: Unit = for (src <- tests) {
    check(s"transcheck: $src") { transCheckTest(src) }
  }

  def testFile: Unit = for (file <- walkTree(ES_TEST_DIR)) {
    val filename = file.getName
    if (jsFilter(filename))
      check(filename) { transCheckTestFile(file.toString) }
  }

  val tests = List(
    "var x = 1 + 2;",
    "let $ = 'dollar'",
    "REF_ERROR",
    "1(2)",
  )

  init
}
