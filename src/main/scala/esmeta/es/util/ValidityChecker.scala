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
    _valid(JSEngine.runUsingD8(src)) &&
    _valid(JSEngine.runUsingGraal(src))

  val MESSAGE = "VALIDITY_CHECKER_EXPECTED_EXCEPTION"

  private def _valid(result: Try[Any]): Boolean =
    result.failed.filter(_.getMessage contains MESSAGE).isSuccess
}
