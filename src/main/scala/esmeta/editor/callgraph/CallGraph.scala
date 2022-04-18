package esmeta.editor

import esmeta.cfg.CFG
import esmeta.cfg.Call

trait CallGraph:
  def funcs: Set[String]
  // def calls: Map[String, Set[Call]]
  // def call_targets: Map[Call, Set[String]]
  def func_targets: Map[String, Set[String]]
end CallGraph
