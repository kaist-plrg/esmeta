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
  import RandomMutator.*
  val names = List("RandomMutator")

  /** mutate programs */
  def apply(
    ast: Ast,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): Iterable[(String, Ast)] =
    val k = targetAstCounter(ast)
    if (k == 0)
      List.fill(n)(ast)
    else
      c = (n - 1) / k + 1
    shuffle(Walker.walk(ast)).take(n).map((name, _))

  /* number of new candidates to make for each target */
  var c = 0

  /** internal walker */
  object Walker extends Util.AdditiveListWalker {
    override def walk(ast: Syntactic): List[Syntactic] =
      val mutants = super.walk(ast)
      if isTarget(ast) then List.tabulate(c)(_ => synthesizer(ast)) ++ mutants
      else mutants
  }

}

object RandomMutator {
  // true if the given ast is target ast
  def isTarget = (ast: Syntactic) =>
    List(
      "AssignmentExpression",
      "PrimaryExpression",
      "Statement",
      "Declaration",
    )
      .contains(ast.name)

  // count the number of target sub-ast
  val targetAstCounter = new Util.AstCounter(isTarget)

  // default random mutator
  val default = RandomMutator()
}
