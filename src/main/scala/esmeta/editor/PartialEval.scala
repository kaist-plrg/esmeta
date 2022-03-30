package esmeta.editor

import esmeta.spec.Algorithm
import esmeta.cfg.CFG
import esmeta.editor.analyzer.AbsSemantics
import esmeta.editor.sview.*
import esmeta.ir.{Func => IRFunc}
import esmeta.editor.util.CFGHelper
import esmeta.editor.analyzer.*
import esmeta.editor.analyzer.AbsStateDomain
import esmeta.editor.analyzer.AbsValueDomain
import esmeta.cfg.Node

// partial evaluator for IR functions with a given syntactic view
class PartialEval(cfgHelper: CFGHelper) {

  def apply(view: SyntacticView): List[IRFunc] = {
    //
    val avd: AbsValueDomain = BasicValueDomain()
    val aod = BasicObjDomain(avd)
    val asd = BasicStateDomain(aod, cfgHelper.cfg)
    val ard = RetDomain(asd)

    val absinit =
      new AbsSemantics(ard)(
        cfgHelper,
      )

    absinit.npMap = absinit.initialize(view)
    val absfin = absinit.fixpoint

    // Mockup, TODO
    cfgHelper.getSDO(view, "Evaluation").map(_._2.irFunc).toList
  }
}
