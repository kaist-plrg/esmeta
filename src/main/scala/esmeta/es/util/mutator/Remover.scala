package esmeta.es.util.mutator

import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.synthesizer.*
import esmeta.es.util.{UnitWalker => AstWalker, *}
import esmeta.es.util.Coverage.*
import esmeta.spec.Grammar
import esmeta.util.*
import esmeta.util.BaseUtils.*

/** A mutator that removes nodes of ECMAScript AST */
class Remover(
  val synthesizer: Synthesizer = RandomSynthesizer,
) extends Mutator
  with Util.MultiplicativeListWalker {

  val names = "Remover" :: RandomMutator.default.names

  import Remover.*

  /** mutate a program */
  def apply(
    ast: Ast,
    n: Int,
    _target: Option[(CondView, Coverage)],
  ): Iterable[(String, Ast)] = {
    // count the number of potential victims
    val k = victimCounter(ast)
    k1 = 0
    k2 = k
    if (k == 0) RandomMutator.default(ast, n, _target)
    else if (Math.pow(2, k) < n)
      walk(ast)
        .map((name, _)) ++ RandomMutator.default(ast, n - (1 << k), _target)
    else {
      // calculate the most efficient parameters
      while (Math.pow(2, k2 - 1) >= n)
        k1 = k1 + 1
        k2 = k2 - 1
      sample(ast, n)
    }
  }

  /** parameter for sampler */
  private var (k1, k2) = (0, 0)

  private def sample(ast: Ast, n: Int) =
    shuffle(walk(ast)).take(n).map((name, _))

  private def doDrop: Boolean =
    if k1 > 0 && randBool(k1 / (k1 + k2 + 0.0)) then
      k1 -= 1; false
    else if k2 > 0 then
      k2 -= 1; true
    else throw new Error("This is a bug in Remover")

  /** ast walker */
  override def walk(ast: Syntactic): List[Syntactic] =
    val mutants = super.walk(ast)
    val i = findSameChild(ast)
    if i >= 0 && doDrop then
      mutants ++ mutants.map(_.children(i).get.asInstanceOf[Syntactic])
    else mutants
}

object Remover {
  def findSameChild(ast: Syntactic): Int =
    val Syntactic(name, args, rhsIdx, children) = ast

    children.indexWhere(_ match {
      case Some(Syntactic(`name`, `args`, _, _)) => true
      case _                                     => false
    })

  // count the number of asts that have same child
  val victimCounter = Util.AstCounter(ast => findSameChild(ast) >= 0)
}