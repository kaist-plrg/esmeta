package esmeta.wji.util

import esmeta.cfg.CFG
import esmeta.cfgBuilder.CFGBuilder
import esmeta.error.ESMetaError
import esmeta.ir.Program

/** Merges a compiled WJI [[Program]] into the mainline ECMA-262 one, so both
  * run on the same `CFG` and `esmeta.interpreter.Interpreter`.
  *
  * `redefined` names the mainline functions WJI replaces (see
  * `esmeta.wji.lang.AlgorithmKind.Redefinition`); any other name clash is an
  * error rather than a silent override.
  */
object MergeWithMainline:
  def apply(
    mainline: Program,
    wji: Program,
    redefined: Set[String] = Set(),
  ): CFG =
    val mainlineNames = mainline.funcs.map(_.name).toSet
    val missing = redefined -- mainlineNames
    if missing.nonEmpty then
      throw ESMetaError(s"no mainline function to redefine: $missing")
    val clashes = (wji.funcs.map(_.name).toSet & mainlineNames) -- redefined
    if clashes.nonEmpty then
      throw ESMetaError(s"WJI function clashes with mainline: $clashes")
    val kept = mainline.funcs.filterNot(f => redefined.contains(f.name))
    CFGBuilder(Program(kept ++ wji.funcs, mainline.spec))
