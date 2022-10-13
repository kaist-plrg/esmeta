package esmeta.es.util.mutator

import esmeta.es.*
import esmeta.es.util.synthesizer.*
import esmeta.es.util.{Walker => AstWalker, *}
import esmeta.es.util.Coverage.*
import esmeta.spec.Grammar
import esmeta.util.*
import esmeta.util.BaseUtils.*

/** A random ECMAScript AST mutator */
class RandomMutator(
  val synthesizer: Synthesizer = RandomSynthesizer,
) extends Mutator {

  /** mutate programs */
  def apply(
    ast: Ast,
    target: Option[(CondView, Coverage)],
  ): (String, Ast) = (names.head, Walker.walk(ast))

  /** internal walker */
  object Walker extends AstWalker {
    override def walk(ast: Syntactic): Syntactic = ast.name match
      case "AssignmentExpression" | "PrimaryExpression" | "Statement" |
          "Declaration" if randBool =>
        synthesizer(ast)
      case _ =>
        super.walk(ast)
  }

  val names = List("RandomMutator")
}
