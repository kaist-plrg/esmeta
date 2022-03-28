package esmeta.editor

import esmeta.spec.Algorithm
import esmeta.cfg.CFG
import esmeta.editor.analyzer.AbsSemantics
import esmeta.editor.sview.*
import esmeta.ir.{Func => IRFunc}

// partial evaluator for IR functions with a given syntactic view
class PartialEval(cfg: CFG) {

  def apply(view: SyntacticView): List[IRFunc] = {
    //
    val abs = new AbsSemantics(cfg)
    // Mockup, TODO
    abs.getSDO(view, "Evaluation").map(_._2.irFunc).toList
  }
}
