package esmeta.wji.util

import esmeta.cfg.CFG
import esmeta.cfgBuilder.CFGBuilder
import esmeta.ir.Program

/** Merges a compiled WJI [[Program]] into the mainline ECMA-262 one, so both
  * run on the same `CFG` and `esmeta.interpreter.Interpreter`.
  */
object MergeWithMainline:
  def apply(mainline: Program, wji: Program): CFG =
    CFGBuilder(Program(mainline.funcs ++ wji.funcs, mainline.spec))
