package esmeta.es

import esmeta.*
import esmeta.util.*
import esmeta.es.util.injector.*

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

  /** List of passed and failed assertions */
  lazy val (passedAssertions, failedAssertions)
    : (Vector[Assertion], Vector[(Assertion, String)]) =
    val src = s"${Injector.assertions}$LINE_SEP$script"
    JSEngine
      .runAndGetStdout(src :: (assertions.toList.map(_.toString)))
      .map(stdouts =>
        val (p, f) = assertions.zip(stdouts.tail).partition(_._2.isEmpty)
        (p.map(_._1), f),
      )
      .getOrElse(
        // TODO inspect exit tag more carefully
        if isNormal then (Vector(), assertions.map((_, "")))
        else (assertions, Vector()),
      )

  /** retain only passed assertions */
  def filterAssertion: ConformTest =
    ConformTest(id, script, exitTag, defs, isAsync, passedAssertions),
}
