package esmeta.js

import esmeta.JS_TEST_DIR
import esmeta.util.SystemUtils.*
import io.circe.*, io.circe.syntax.*, io.circe.parser.*

class DumpSmallTest extends JSTest {
  val name: String = "jsDumpTest"

  // registration
  def init: Unit = {
    import esmeta.js.util.JsonProtocol.given

    // check parser and stringifier from files
    for (file <- walkTree(JS_TEST_DIR)) {
      val filename = file.getName
      if (jsFilter(filename)) check(filename) {
        val name = file.toString
        val parsed = JSTest.parseFile(name)
        val loaded = parsed.asJson.as[Ast].getOrElse(fail("fail to decode"))
        assert(parsed == loaded)
      }
    }
  }

  init
}
