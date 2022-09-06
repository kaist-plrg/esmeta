package esmeta.editor

import esmeta.cfg.CFG
import esmeta.cfg.Call

trait CallGraph:
  def funcs: Set[String]
  // def calls: Map[String, Set[Call]]
  // def call_targets: Map[Call, Set[String]]
  def func_targets: Map[String, Set[String]]
  def draw_func_targets: Map[String, Set[String]] = func_targets

  def toDot: String =
    val mlist =
      (funcs ++ draw_func_targets.values.flatten).toList.zipWithIndex.toMap
    s"""
digraph {
${mlist.keySet
      .map((s) => s"\"X${s.hashCode()}\" [label=\"$s\"]")
      .toList
      .sorted
      .mkString("\n")}

${funcs
      .map((s) =>
        draw_func_targets
          .getOrElse(s, Set())
          .map((t) => s"X${s.hashCode()} -> X${t.hashCode()}")
          .mkString("\n"),
      )
      .mkString("\n")
      .split("\n")
      .sorted
      .mkString("\n")}
}
"""
end CallGraph
