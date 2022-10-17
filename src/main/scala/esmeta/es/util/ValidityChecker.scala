package esmeta.es.util

import esmeta.*
import esmeta.es.*
import esmeta.spec.*
import esmeta.util.JSEngine
import scala.util.Try

/** ECMAScript program validity checker */
object ValidityChecker {
  def apply(grammar: Grammar, ast: Ast): Boolean = apply(ast.toString(grammar))
  def apply(code: String): Boolean =
    val src = s"${USE_STRICT}throw \"$MESSAGE\";$LINE_SEP;$LINE_SEP$code"
    (!useD8 || _valid(JSEngine.runUsingD8(src))) &&
    (!useGraal || _valid(JSEngine.runUsingGraal(src)))

  val MESSAGE = "VALIDITY_CHECKER_EXPECTED_EXCEPTION"

  private def _valid(result: Try[Any]): Boolean =
    result.failed.filter(_.getMessage contains MESSAGE).isSuccess

  import esmeta.util.BaseUtils.warn
  lazy val useD8: Boolean =
    val use = JSEngine.runUsingD8(";").isSuccess;
    if (!use) warn("No D8")
    use
  lazy val useGraal: Boolean =
    val use = JSEngine.runUsingGraal(";").isSuccess;
    if (!use) warn("No Graal")
    use
}
