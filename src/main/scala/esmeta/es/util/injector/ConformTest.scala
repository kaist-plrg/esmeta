package esmeta.es.util.injector

import esmeta.*
import esmeta.cfg.CFG
import esmeta.error.{NoGraal, TimeoutException}
import esmeta.es.*
import esmeta.es.util.*
import esmeta.state.State
import esmeta.util.*
import esmeta.util.SystemUtils.*
import scala.util.*

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

  /** retain only passed assertions */
  def filterAssertion: ConformTest =
    ConformTest(id, script, exitTag, defs, isAsync, passedAssertions)

  /** indicates if the test should exit normally */
  val isNormal = exitTag == NormalTag

  /** An optional comment to this test */
  var comment = ""

  /** Execute test and get result */
  lazy val (
    concreteExitTag: ExitTag,
    passedAssertions: Vector[Assertion],
    failedAssertions: Vector[(Assertion, String)],
  ) = JSEngine
    .usingContext((context, out) => {
      JSEngine.runUsingContext(script, context, Some(1000))
      JSEngine.runUsingContext(Injector.assertionLib, context)
      val (passes, fails) = assertions
        .map(assertion =>
          (
            assertion,
            try JSEngine.runAndGetStdout(assertion.toString, context, out)
            catch {
              case e =>
                s"An exception occured while checking this assertion.$LINE_SEP"
            },
          ),
        )
        .partition(_._2.isEmpty)
      (NormalTag, passes.map(_._1), fails)
    })
    .recoverWith(_ match {
      case e: JSEngine.JSException =>
        // TODO handle ThrowValueTag more carefully
        val msg = e.getMessage
        val tag =
          if msg.contains("Error:") then ThrowErrorTag(msg.split(":").head)
          else if msg.contains(Babel.failTag) then TranspileFailTag
          else ThrowValueTag(esmeta.state.Str(msg))
        Success((tag, Vector(), Vector()))
      case e: TimeoutException =>
        Success((TimeoutTag, Vector(), Vector()))
      case e =>
        Failure(e)
    })
    .get

  /** Indicates if the expected exit tag mathces with the concrete exit tag */
  lazy val sameExitTag = exitTag equivalent concreteExitTag

  /** Indicates if the test is passed */
  lazy val isPass =
    try sameExitTag && failedAssertions.length == 0
    catch { case NoGraal => true }

  /** human readable message indication the reason of test fail */
  lazy val msg =
    if isPass then ""
    else if (!sameExitTag) then
      s"[Exit Tag Mismatch]$LINE_SEP > Expected $exitTag but got $concreteExitTag$LINE_SEP"
    else failedAssertions.map((a, m) => s"$a$LINE_SEP > $m").mkString("")

  /** result of manual categorization */
  lazy val category: String =
    if isPass then ""
    else {
      ConformTest.manualRule
        .foldLeft("YET")((cur, rule) =>
          cur match {
            case "YET" =>
              val Array(tag, codePattern, msgPattern) = rule
              if (script.contains(codePattern) && msg.contains(msgPattern))
                tag
              else
                cur
            case _ => cur
          },
        )
    }

  def dumpTest(dir: String, name: String): Unit =
    dumpFile(this.toString, s"$dir/$name")
    if (!isPass)
      dumpFile(s"TAG: $category${LINE_SEP}$msg", s"$dir/$name.msg")
}

object ConformTest {

  /** Create a pair of tests using code string */
  def createTestPair(script: String): (ConformTest, ConformTest) =
    val engineScript = USE_STRICT + script
    val transpiledScript = Babel.transpile(engineScript)
    val injectedTest = Injector(script, true)
    val engineTest =
      injectedTest.replaceScript(engineScript)
    val transpiledTest =
      engineTest.filterAssertion.replaceScript(transpiledScript)
    (engineTest, transpiledTest)

  /** Create a test using init state and exit state */
  def createTest(initSt: State, exitSt: State): ConformTest =
    val script = initSt.sourceText.get
    new Injector(initSt, exitSt, true, false).conformTest
      .replaceScript(placeholder)

  /** Manually written rule to categorize bugs kind */
  lazy val manualRule =
    readFile(f"$RESOURCE_DIR/injector/manual-categorize.csv")
      .split(LINE_SEP)
      .drop(1) // drop header
      .map(l => l.split(",", -1))

  /** placeholder for script */
  val placeholder = "//SCRIPT_PLACEHOLDER"
}
