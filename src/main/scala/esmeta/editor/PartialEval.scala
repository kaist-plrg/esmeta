package esmeta.editor

import esmeta.spec.Algorithm
import esmeta.cfg.CFG
import esmeta.editor.analyzer.{Initialize, AbsSemantics}
import esmeta.editor.sview.*
import esmeta.ir.{Func => IRFunc}
import esmeta.editor.util.CFGHelper

// partial evaluator for IR functions with a given syntactic view
class PartialEval(cfgHelper: CFGHelper) {

  def apply(view: SyntacticView): List[IRFunc] = {
    //
    val abs =
      new AbsSemantics[Any, Any](
        cfgHelper,
        npMap = Initialize[Any](cfgHelper)(view),
      ).fixpoint
    // Mockup, TODO
    cfgHelper.getSDO(view, "Evaluation").map(_._2.irFunc).toList
  }
}
