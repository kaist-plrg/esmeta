package esmeta.es.util.mutator

import esmeta.cfg.CFG
import esmeta.es.*
import esmeta.es.util.synthesizer.*
import esmeta.es.util.{Walker => AstWalker}
import esmeta.es.util.Coverage.*
import esmeta.spec.Grammar
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
    condView: Option[CondView],
    nearest: Option[Nearest],
  ): (String, Ast) = nearest.fold {
    RandomMutator(synthesizer)(ast, condView, nearest)
  } { case (ty, loc) => ("NearestMutator", Walker(ty, loc).walk(ast)) }

  /** internal walker */
  class Walker(ty: AstSingleTy, loc: Loc) extends AstWalker {
    val AstSingleTy(name, rhsIdx, subIdx) = ty
    override def walk(ast: Syntactic): Syntactic = ast match
      case ast @ Syntactic(`name`, _, `rhsIdx`, _) if ast.subIdx == subIdx =>
        synthesizer(ast)
      case _ =>
        super.walk(ast)
  }
}
