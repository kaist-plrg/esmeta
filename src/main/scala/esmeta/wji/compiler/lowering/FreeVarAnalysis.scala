package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Cond, Expr, Instr}
import esmeta.wji.lang.Instr.PerformOutcome

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

  private def varsOf(expr: Expr): Set[String] = expr match
    case Expr.Var(name)         => Set(name)
    case Expr.Field(base, _)    => varsOf(base)
    case Expr.Index(base, key)  => varsOf(base) ++ varsOf(key)
    case Expr.Link(_, args)     => args.flatMap(varsOf).toSet
    case Expr.AlgoCall(_, args) => args.flatMap(varsOf).toSet
    case Expr.Case(_, args)     => args.flatMap(varsOf).toSet
    case Expr.JSCall(_, args)   => args.flatMap(varsOf).toSet
    case Expr.Abrupt(_, e)      => varsOf(e)
    case Expr.List_(elems)      => elems.flatMap(varsOf).toSet
    case Expr.Map_(entries) =>
      entries.flatMap((k, v) => varsOf(k) ++ varsOf(v)).toSet
    case Expr.Length(e)            => varsOf(e)
    case Expr.BinOp(l, _, r)       => varsOf(l) ++ varsOf(r)
    case Expr.Pow(base, exp)       => varsOf(base) ++ varsOf(exp)
    case Expr.Neg(e)               => varsOf(e)
    case Expr.AsMath(e)            => varsOf(e)
    case Expr.Tuple(elems)         => elems.flatMap(varsOf).toSet
    case Expr.Closure(_, captured) => captured.toSet
    case Expr.CaseTag(base)        => varsOf(base)
    case _                         => Set.empty

  private def varsOfCond(cond: Cond): Set[String] = cond match
    case Cond.Eq(l, r, _)         => varsOf(l) ++ varsOf(r)
    case Cond.Compare(l, _, r)    => varsOf(l) ++ varsOf(r)
    case Cond.HasField(e, _)      => varsOf(e)
    case Cond.Implements(e, _, _) => varsOf(e)
    case Cond.IsOfForm(e, f, condOpt, _) =>
      varsOf(e) ++ varsOf(f) ++ condOpt.toSet.flatMap(varsOfCond)
    case Cond.Matches(l, _, r, _) => varsOf(l) ++ varsOf(r)
    case Cond.IsMissing(e, _)     => varsOf(e)
    case Cond.HasSlot(e, _, _)    => varsOf(e)
    case Cond.IsType(e, _, _)     => varsOf(e)
    case Cond.And(l, r)           => varsOfCond(l) ++ varsOfCond(r)
    case Cond.Or(l, r)            => varsOfCond(l) ++ varsOfCond(r)
    case Cond.Abbreviated(e)      => varsOf(e)
    case _                        => Set.empty
