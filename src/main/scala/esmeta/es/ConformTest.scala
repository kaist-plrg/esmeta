package esmeta.es

import esmeta.*
import esmeta.util.*
import esmeta.es.util.injector.*
import esmeta.cfg.CFG
import esmeta.state.State

/** conformance test */
case class ConformTest(
  id: Int,
  script: String,
  exitTag: ExitTag,
  defs: Boolean,
  isAsync: Boolean,
  assertions: Vector[Assertion],
) extends ESElem
  with UId {

  /** replace script */
  def replaceScript(newScript: String) =
    ConformTest(id, newScript, exitTag, defs, isAsync, assertions)

  /** indicates if the test should exit normally */
  lazy val isNormal = exitTag == NormalTag

  /** Execute test and get result */
  lazy val (concreteExitTag, passedAssertions, failedAssertions)
    : (ExitTag, Vector[Assertion], Vector[(Assertion, String)]) =
    val src = s"${Injector.assertions}$LINE_SEP$script"
    JSEngine
      .runAndGetStdout(src :: (assertions.toList.map(_.toString)))
      .map(stdouts =>
        val (p, f) = assertions.zip(stdouts.tail).partition(_._2.isEmpty)
        (NormalTag, p.map(_._1), f),
      )
      .recover(e =>
        // TODO handle ThrowValueTag more carefully
        val msg = e.getMessage
        val tag =
          if msg.contains("Error:") then ThrowErrorTag(msg.split(":")(0))
          else ThrowValueTag(esmeta.state.Str(msg))
        (tag, Vector(), Vector()),
      )
      .get

  /** Indicates if the test is passed */
  lazy val isPass = exitTag == concreteExitTag && failedAssertions.length == 0

  /** human readable message indication the reason of test fail */
  lazy val msg =
    if isPass then ""
    else if (exitTag != concreteExitTag) then
      s"[Exit Tag Mismatch]$LINE_SEP > Expected $exitTag but got $concreteExitTag"
    else failedAssertions.map((a, m) => s"$a$LINE_SEP > $m").mkString(LINE_SEP)

  /** retain only passed assertions */
  def filterAssertion: ConformTest =
    ConformTest(id, script, concreteExitTag, defs, isAsync, passedAssertions)
}

object ConformTest {
  def createTestPair(script: String, cfg: CFG): (ConformTest, ConformTest) =
    // run babel to get transpiled program
    val transpiled = Babel.transpile(script)

    // inject assertions to original program
    val injectedTest = Injector(cfg, script, true)

    // replace test's script with transpiled script
    val transpiledTest =
      injectedTest.filterAssertion.replaceScript(transpiled)

    (injectedTest, transpiledTest)

  def createTestPair(initSt: State, exitSt: State): (ConformTest, ConformTest) =
    val script = initSt.sourceText.get

    // run babel to get transpiled program
    val transpiled = Babel.transpile(script)

    // inject assertions to original program
    val injectedTest = new Injector(initSt, exitSt, true, false).conformTest

    // replace test's script with transpiled script
    val transpiledTest =
      injectedTest.filterAssertion.replaceScript(transpiled)

    (injectedTest, transpiledTest)
}
