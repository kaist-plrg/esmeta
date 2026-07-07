package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Instr}

/** Removes all [[Instr.Note]] nodes from every algorithm body. */
object DropNotesPass extends LoweringPass:
  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = transform(a.body)))

  private def transform(instrs: List[Instr]): List[Instr] =
    instrs.flatMap {
      case _: Instr.Note => Nil
      case instr         => List(instr.mapBody(transform))
    }
