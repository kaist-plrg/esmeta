package esmeta.util

import esmeta.*
import esmeta.util.SystemUtils.readFile

/** JavaScript Babel utilities */
object Babel {

  val BABEL_FILE = s"$RESOURCE_DIR/babel/babel@7.19.1.min.js"
  val RUNNER_FILE = s"$RESOURCE_DIR/babel/transpile.js"
  val runner = readFile(RUNNER_FILE)

  lazy val init: Unit =
    JSEngine.runInContext("babel", readFile(BABEL_FILE))

  def transpile(src: String): String =
    init
    val escaped =
      src.replace("\\", "\\\\").replace("`", "\\`").replace("$", "\\$")
    JSEngine.runInContext("babel", s"let orig = `$escaped`").get
    JSEngine.runInContext("babel", runner).get.toString
}
