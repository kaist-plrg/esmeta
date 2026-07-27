package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Cond, Expr, Instr}
import esmeta.wji.lang.Instr.PerformOutcome
import esmeta.wji.lang.util.UnitWalker
import scala.collection.mutable

/** Free-variable analysis for a block of already-lowered [[Instr]]s, shared by
  * lowering passes that split a nested instruction body off into its own
  * top-level [[esmeta.wji.lang.Algorithm]] and need to know which
  * enclosing-scope variables the split-off body still references, so they can
  * be passed in explicitly as a closure's captured variables (see
  * [[ExpandFollowingStepsPass]]).
  */
object FreeVarAnalysis:

  private def stripPipes(s: String): String =
    s.stripPrefix("|").stripSuffix("|")

  /** Every `Var` name `instrs` references that isn't bound somewhere inside
    * `instrs` itself (by a `Let`, a `ForEach` element, or a `Perform`'s
    * `BindResult`). Position-insensitive (a name bound anywhere in `instrs` is
    * treated as bound throughout, not just after its binding site) — spec prose
    * never shadows an outer variable with a same-named local, so this is a safe
    * simplification.
    */
  def freeVars(instrs: List[Instr]): Set[String] =
    referencedVars(instrs) -- boundVars(instrs)

  private def boundVars(instrs: List[Instr]): Set[String] =
    instrs.flatMap(boundVarsInstr).toSet

  private def bindableNames(expr: Expr): Set[String] = expr match
    case Expr.Var(name)    => Set(name)
    case Expr.Tuple(elems) => elems.flatMap(bindableNames).toSet
    case _                 => Set.empty

  private def boundVarsInstr(instr: Instr): Set[String] =
    val own: Set[String] = instr match
      case Instr.Let(lhs, _, _)      => bindableNames(lhs)
      case Instr.ForEach(elem, _, _) => bindableNames(elem)
      case p: Instr.Perform =>
        p.outcome match
          case PerformOutcome.BindResult(v) => Set(stripPipes(v))
          case _                            => Set.empty
      case p: Instr.PerformClosure =>
        p.outcome match
          case PerformOutcome.BindResult(v) => Set(stripPipes(v))
          case _                            => Set.empty
      case _ => Set.empty
    val nested: Set[String] = instr match
      case Instr.IfChain(branches, fallback) =>
        branches.flatMap((_, b) => boundVars(b)).toSet ++ boundVars(fallback)
      case other => boundVars(other.body)
    own ++ nested

  private def referencedVars(instrs: List[Instr]): Set[String] =
    instrs.flatMap(referencedVarsInstr).toSet

  private def referencedVarsInstr(instr: Instr): Set[String] =
    val here: Set[String] = instr match
      case Instr.Let(_, rhs, _)            => varsOf(rhs)
      case Instr.Set(lhs, rhs, _)          => varsOf(lhs) ++ varsOf(rhs)
      case Instr.If(cond, _)               => varsOfCond(cond)
      case Instr.ElseIf(cond, _)           => varsOfCond(cond)
      case Instr.Return(exprOpt, _)        => exprOpt.toSet.flatMap(varsOf)
      case Instr.Assert(cond, _)           => varsOfCond(cond)
      case Instr.ForEach(_, collection, _) => varsOf(collection)
      case Instr.While(cond, _)            => varsOfCond(cond)
      case Instr.Append(item, collection, _) =>
        varsOf(item) ++ varsOf(collection)
      case p: Instr.Perform => p.args.flatMap(varsOf).toSet
      case p: Instr.PerformClosure =>
        varsOf(p.closure) ++ p.args.flatMap(varsOf).toSet
      case Instr.IfChain(branches, _) =>
        branches.flatMap((c, _) => varsOfCond(c)).toSet
      case _ => Set.empty
    val nested: Set[String] = instr match
      case Instr.IfChain(branches, fallback) =>
        branches.flatMap((_, b) => referencedVars(b)).toSet ++ referencedVars(
          fallback,
        )
      case other => referencedVars(other.body)
    here ++ nested

  /** Every `Var` name reachable from an `Expr`/`Cond`, at any depth. Only
    * overrides [[Expr.Var]] itself and [[Expr.Closure]] (whose `captured:
    * List[String]` isn't reachable via generic `Expr` recursion — it's plain
    * names, not nested `Expr`s, same reason `Expr.children` leaves it a leaf
    * too) — every other node is reached by [[UnitWalker]]'s own exhaustive
    * default recursion.
    */
  private class VarCollector extends UnitWalker:
    val vars = mutable.Set.empty[String]
    override def walk(expr: Expr): Unit = expr match
      case Expr.Var(name)            => vars += name
      case Expr.Closure(_, captured) => vars ++= captured
      case other                     => super.walk(other)

  private def varsOf(expr: Expr): Set[String] =
    val w = VarCollector()
    w.walk(expr)
    w.vars.toSet

  private def varsOfCond(cond: Cond): Set[String] =
    val w = VarCollector()
    w.walk(cond)
    w.vars.toSet
