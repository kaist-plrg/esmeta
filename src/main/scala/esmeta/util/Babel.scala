package esmeta.util

import esmeta.*
import esmeta.util.SystemUtils.readFile

/** JavaScript Babel utilities */
object Babel {

  val BABEL_FILE = s"$RESOURCE_DIR/babel/babel@7.19.1.min.js"
  val RUNNER_FILE = s"$RESOURCE_DIR/babel/transpile.js"
  lazy val babel = readFile(BABEL_FILE)
  lazy val runner = readFile(RUNNER_FILE)

  lazy val doInit: Unit =
    JSEngine.runInContext("babel", babel)

  def transpile(src: String): String =
    doInit
    val escaped =
      src.replace("\\", "\\\\").replace("`", "\\`").replace("$", "\\$")
    JSEngine.runInContext("babel", s"orig = `$escaped`").get
    JSEngine.runInContext("babel", runner).get.toString
}
