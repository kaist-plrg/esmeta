package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Cond, Expr, Instr}
import esmeta.wji.lang.Instr.PerformOutcome
import esmeta.wji.lang.walker.UnitWalker
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

  private def bindableNames(expr: Expr): Set[String] = expr match
    case Expr.Var(name)    => Set(name)
    case Expr.Tuple(elems) => elems.flatMap(bindableNames).toSet
    case _                 => Set.empty

  /** Every `Var` name `instrs` references that isn't bound somewhere inside
    * `instrs` itself (by a `Let`, a `ForEach`/`For` element, or a `Perform`'s
    * `BindResult`). Position-insensitive (a name bound anywhere in `instrs` is
    * treated as bound throughout, not just after its binding site) — spec prose
    * never shadows an outer variable with a same-named local, so this is a safe
    * simplification.
    */
  def freeVars(instrs: List[Instr]): Set[String] =
    val w = FreeVarWalker()
    instrs.foreach(w.walk)
    w.referenced.toSet -- w.bound.toSet

  /** Walks an already-lowered instruction list once, sorting every `Var` name
    * it sees into `referenced` or `bound`. Overrides only the handful of shapes
    * that actually introduce a binding — `Let`, `ForEach`/`For`'s element, and
    * a `Perform`/`PerformClosure`'s `BindResult` — carefully *not* recursing
    * into the binder-side `Expr` itself (so e.g. `Let`'s `lhs` doesn't get
    * miscounted as a reference), then walks everything else
    * (RHS/collection/args/nested body) as usual. Every other node shape —
    * including which `Var`s inside a `Cond`/`Expr` count as "referenced" —
    * falls through to [[UnitWalker]]'s ordinary exhaustive recursion, so a new
    * `Instr`/`Expr` constructor is covered automatically rather than needing
    * this analysis hand-updated for it.
    */
  private class FreeVarWalker extends UnitWalker:
    val referenced = mutable.Set.empty[String]
    val bound = mutable.Set.empty[String]

    override def walk(cond: Cond): Unit = cond match
      // Unreachable today (ExpandThrowsPass desugars Cond.Throws away before
      // freeVars's only caller runs) — kept for callers earlier in the pipeline,
      // where a Throws catch branch implicitly binds "exception" pre-desugaring.
      case Cond.Throws(_) => bound += "exception"
      case other          => super.walk(other)

    override def walk(expr: Expr): Unit = expr match
      case Expr.Var(name)            => referenced += name
      case Expr.Closure(_, captured) => referenced ++= captured
      case other                     => super.walk(other)

    override def walk(instr: Instr): Unit = instr match
      case i: Instr.Let =>
        bound ++= bindableNames(i.lhs)
        walk(i.expr)
        i.body.foreach(walk)
      case i: Instr.ForEach =>
        bound ++= bindableNames(i.elem)
        walk(i.collection)
        i.body.foreach(walk)
      case i: Instr.For =>
        bound ++= bindableNames(i.elem)
        walk(i.collection)
        i.body.foreach(walk)
      case i: Instr.Perform =>
        bindResultOf(i.outcome).foreach(bound += _)
        i.args.foreach(walk)
        i.body.foreach(walk)
      case i: Instr.PerformClosure =>
        bindResultOf(i.outcome).foreach(bound += _)
        walk(i.closure)
        i.args.foreach(walk)
        i.body.foreach(walk)
      case other => super.walk(other)

    private def bindResultOf(outcome: PerformOutcome): Option[String] =
      outcome match
        case PerformOutcome.BindResult(v) => Some(stripPipes(v))
        case _                            => None
