package esmeta.editor

import esmeta.cfg.CFG
import esmeta.cfg.Call

trait CallGraph:
  def funcs: Set[String]
  // def calls: Map[String, Set[Call]]
  // def call_targets: Map[Call, Set[String]]
  def func_targets: Map[String, Set[String]]

  def toDot: String =
    val mlist = (funcs ++ func_targets.values.flatten).toList.zipWithIndex.toMap
    s"""
digraph {
${mlist.keySet.map((s) => s"\"X${mlist(s)}\" [label=\"$s\"]").mkString("\n")}

${funcs.map((s) => func_targets.getOrElse(s, Set()).map((t) => s"X${mlist(s)} -> X${mlist(t)}").mkString("\n")).mkString("\n")}
}
"""
end CallGraph
