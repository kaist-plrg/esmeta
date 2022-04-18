package esmeta.editor

import esmeta.ir.*
import esmeta.ir.util.UnitWalker
import esmeta.editor.util.CFGHelper

class SyntacticSearch(cfgHelper: CFGHelper) extends UnitWalker:

  val target = collection.mutable.Set[String]()
  override def walk(i: Inst): Unit = i match
    case ICall(_, EClo(name, _), _) if name != "GetValue" =>
      if (cfgHelper.cfg.fnameMap contains name) target += name else ()
    case ICall(_, ERef(Prop(_, EStr(something))), _)
        if something != "Evaluation" =>
      if (cfgHelper.nameCloMap contains something)
        target ++= cfgHelper.nameCloMap(something)
      else if (cfgHelper.topCloNameSet contains something) ()
      else
        throw new Error()
    case _ => super.walk(i)
end SyntacticSearch

class SyntacticCallGraph(cfgHelper: CFGHelper, f: Func) extends CallGraph:
  val search = SyntacticSearch(cfgHelper)
  search.walk(f)
  val funcs = Set(f.name)
  val func_targets = Map(f.name -> search.target.toSet)
end SyntacticCallGraph

class SyntacticMergedCallGraph(
  mcgs: Map[String, CallGraph],
  cfgHelper: CFGHelper,
) extends CallGraph:
  val funcs = mcgs.values.map(_.funcs).flatten.toSet
  val func_targets =
    mcgs.values
      .map((cg) => cg.funcs.head -> cg.func_targets(cg.funcs.head))
      .toMap
end SyntacticMergedCallGraph

class SyntacticTransitiveClosedCallGraph(
  mcgs: Map[String, CallGraph],
  f: String,
) extends CallGraph:
  def aux(s: Set[String]): Set[String] =
    val ns = s ++ s.flatMap((n) => mcgs(n).func_targets(n))
    if ns == s then ns else aux(ns)
  val funcs = aux(Set(f))
  val func_targets =
    funcs.toList.map((i) => (i -> mcgs(i).func_targets(i))).toMap

end SyntacticTransitiveClosedCallGraph
