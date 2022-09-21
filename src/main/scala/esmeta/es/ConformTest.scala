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
    JSEngine
      .runAndGetStdout(
        script :: Injector.lib :: (assertions.toList.map(_.toString)),
      )
      .map(stdouts =>
        val (p, f) = assertions.zip(stdouts.tail.tail).partition(_._2.isEmpty)
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

  lazy val sameExitTag =
    exitTag == concreteExitTag ||
    // TODO handle ThrowValueTag more carefully
    exitTag.isInstanceOf[ThrowValueTag] && concreteExitTag
      .isInstanceOf[ThrowValueTag]

  /** Indicates if the test is passed */
  lazy val isPass = sameExitTag && failedAssertions.length == 0

  /** human readable message indication the reason of test fail */
  lazy val msg =
    if isPass then ""
    else if (!sameExitTag) then
      s"[Exit Tag Mismatch]$LINE_SEP > Expected $exitTag but got $concreteExitTag"
    else failedAssertions.map((a, m) => s"$a$LINE_SEP > $m").mkString(LINE_SEP)

  /** retain only passed assertions */
  def filterAssertion: ConformTest =
    ConformTest(id, script, concreteExitTag, defs, isAsync, passedAssertions)
}

object ConformTest {

  /** Create a pair of tests using code string */
  def createTestPair(script: String, cfg: CFG): (ConformTest, ConformTest) =
    val transpiled = Babel.transpile(script)
    val injectedTest = Injector(cfg, script, true)
    val transpiledTest =
      injectedTest.filterAssertion.replaceScript(transpiled)
    (injectedTest, transpiledTest)

  /** Create a pair of tests using init state and exit state */
  def createTestPair(initSt: State, exitSt: State): (ConformTest, ConformTest) =
    val script = initSt.sourceText.get
    val transpiled = Babel.transpile(script)
    val injectedTest = new Injector(initSt, exitSt, true, false).conformTest
    val transpiledTest =
      injectedTest.filterAssertion.replaceScript(transpiled)
    (injectedTest, transpiledTest)
}
