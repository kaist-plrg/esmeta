package esmeta.es.util.mutator

import esmeta.cfg.CFG
import esmeta.es.*
import esmeta.es.util.synthesizer.*
import esmeta.es.util.{Walker => AstWalker, *}
import esmeta.es.util.Coverage.*
import esmeta.spec.Grammar
import esmeta.state.Nearest
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.ty.AstSingleTy

/** A nearest ECMAScript AST mutator */
class NearestMutator(
  val synthesizer: Synthesizer = RandomSynthesizer,
) extends Mutator {

  /** mutate programs */
  def apply(
    ast: Ast,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): (String, Iterable[Ast]) = (for {
    (condView, cov) <- target
    nearest <- cov.targetCondViews.getOrElse(condView, None)
  } yield (names.head, List.tabulate(n)(_ => Walker(nearest).walk(ast))))
    .getOrElse(randomMutator(ast, n, target))

  /** internal walker */
  class Walker(nearest: Nearest) extends AstWalker {
    val AstSingleTy(name, rhsIdx, subIdx) = nearest.ty
    override def walk(ast: Syntactic): Syntactic = if (
      ast.name == name &&
      ast.rhsIdx == rhsIdx &&
      ast.subIdx == subIdx &&
      ast.loc == Some(nearest.loc)
    ) synthesizer(ast)
    else super.walk(ast)
  }

  /** internal random mutator */
  private val randomMutator = RandomMutator(synthesizer)

  val names = "NearestMutator" :: randomMutator.names
}
