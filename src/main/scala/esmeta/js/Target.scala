package esmeta.js

import esmeta.util.BaseUtils.*
import esmeta.es.util.injector.*

case class Target(
                   name: String,
                   isTrans: Boolean,
                   _cmd: String = "",
                 ) {
  lazy val cmd = optional {
    if (!isTrans)
      JSEngine.defaultCmd(name)
    else
      JSTrans.defaultCmd(name)
  }.getOrElse(_cmd)

  override def toString = name

  type CResult = (ExitTag, ExitTag, String)

  def isPass(result: CResult): Boolean =
    val (expected, actual, stdout) = result
    expected.equivalent(actual) && stdout.isEmpty

  def doConformTest(code: String): Boolean = isPass(runConformTest(code))

  def doConformTest(test: ConformTest): Boolean = isPass(runConformTest(test))

  private def runConformTest(code: String): CResult = runConformTest(
    Injector(code),
  )

  private def runConformTest(test: ConformTest): CResult = {
    val assertion = test.core
    val code = test.script
    val exitTag = test.exitTag
    val isNormal = exitTag == NormalTag

    def testMaker(code: String) = {
      if (isNormal)
        List(
          USE_STRICT,
          code,
          libHead,
          Injector.assertionLib,
          delayHead,
          assertion,
          delayTail,
          libTail,
        ).mkString(LINE_SEP)
      else
        List(
          USE_STRICT,
          code,
          assertion,
        ).mkString(LINE_SEP)
    }

    val (concreteExitTag, stdout) =
      if (!isTrans) {
        // engine
        val etest = testMaker(code)
        JSEngine
          .runUsingBinary(cmd, etest)
          .map((NormalTag, _))
          .recover(engineErrorResolver _)
          .get
      } else {
        val transpiledCode = JSTrans.transpileUsingRepl(repl.get, code)
        val ttest = testMaker(transpiledCode)
        JSEngine
          .run(ttest)
          .map((NormalTag, _))
          .recover(engineErrorResolver _)
          .get
      }
    (exitTag, concreteExitTag, stdout)
  }
}
