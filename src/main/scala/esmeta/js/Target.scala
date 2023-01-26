package esmeta.js

import esmeta.LINE_SEP
import esmeta.util.BaseUtils.*
import esmeta.es.Script
import esmeta.error.{TimeoutException, NoCommandError}
import esmeta.es.util.injector.*
import esmeta.es.util.Coverage.Interp

case class Target(
  val name: String,
  val isTrans: Boolean,
  val _cmd: String = "",
) {
  lazy val cmd = optional {
    if (!isTrans)
      JSEngine.defaultCmd(name)
    else
      JSTrans.defaultCmd(name)
  }.getOrElse(_cmd)

  override def toString = name

  private val errorPattern = "[\\w]*Error(?=: )".r

  def doConformTest(test: ConformTest): Boolean = {
    val exitTag = test.exitTag
    val isNormal = exitTag == NormalTag

    val (concreteExitTag, stdout) =
      if (!isTrans) {
        JSEngine
          .runUsingBinary(_cmd, test.script)
          .map((NormalTag, _))
          .recover(engineErrorResolver _)
          .get
      } else {
        JSEngine
          .run(test.script)
          .map((NormalTag, _))
          .recover(engineErrorResolver _)
          .get
      }
    val sameExitTag = exitTag.equivalent(concreteExitTag)
    val pass = sameExitTag && stdout.isEmpty

    pass
  }

  private def engineErrorResolver(engineError: Throwable): (ExitTag, String) =
    engineError match {
      case e: TimeoutException     => (TimeoutTag, "")
      case e @ NoCommandError(cmd) => throw e
      case e =>
        val msg = e.getMessage
        val tag = errorPattern.findFirstIn(msg) match {
          case Some(name) =>
            ThrowErrorTag(name, msg.split(LINE_SEP).toList.headOption)
          case _ if msg.contains("TRANSPILE_FAILURE") => TranspileFailTag
          case _ => ThrowValueTag(esmeta.state.Str(msg))
        }
        (tag, "")
    }

  def minimize(code: String): (String, Interp) = ???
}
