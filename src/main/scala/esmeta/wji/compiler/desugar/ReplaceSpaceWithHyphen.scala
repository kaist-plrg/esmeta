package esmeta.wji.compiler.desugar

import esmeta.wji.lang.{Algorithm, Instr}

/** Replaces every space in each algorithm's `name` with a hyphen, so it can be
  * used as a valid function identifier, and does the same in every
  * [[Instr.Perform]]'s `func` link so calls keep matching their (renamed)
  * target.
  *
  * This isn't a desugaring (it changes no control-flow/expression shape) but
  * lives alongside the other passes since it needs to run over the same
  * `List[Algorithm]` before/after which the rest of the pipeline runs.
  */
object ReplaceSpaceWithHyphen extends DesugarPass:
  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map { a =>
      a.copy(name = a.name.map(dashify), body = a.body.map(rewriteInstr))
    }

  private def dashify(s: String): String = s.replace(' ', '-')

  private def rewriteInstr(instr: Instr): Instr =
    val rewritten = instr match
      case i: Instr.Perform => i.copy(func = dashify(i.func))
      case other            => other
    rewritten.mapBody(_.map(rewriteInstr))
