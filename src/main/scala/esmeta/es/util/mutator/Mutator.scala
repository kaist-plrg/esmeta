package esmeta.es.util.mutator

import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.Coverage.*
import esmeta.spec.Grammar
import esmeta.util.*

/** ECMAScript AST mutator */
trait Mutator {

  /** mutate string */
  def apply(code: String, n: Int): (String, Iterable[Ast]) =
    apply(code, n, None)
  def apply(
    code: String,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): (String, Iterable[Ast]) = apply(cfg.scriptParser.from(code), n, target)

  /** mutate asts */
  def apply(ast: Ast, n: Int): (String, Iterable[Ast]) = apply(ast, n, None)
  def apply(
    ast: Ast,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): (String, Iterable[Ast])

  /** Possible names of underlying mutators */
  val names: List[String]
}
