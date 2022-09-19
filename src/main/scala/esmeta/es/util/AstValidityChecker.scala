package esmeta.es.util

import esmeta.es.*
import esmeta.spec.*
import esmeta.util.JSEngine

/** TODO AST validity checker */
object AstValidityChecker {
  def apply(grammar: Grammar, ast: Ast): Boolean =
    val MESSAGE = "VALIDITY_CHECKER_EXPECTED_EXCEPTION"
    val src = s"\"use strict\";\nthrow $MESSAGE;\n${ast.toString(grammar)}"

    JSEngine.run(src).failed.filter(_.getMessage contains MESSAGE).isSuccess
}
