package esmeta.wji.lang.walker

import esmeta.wji.lang.{Cond, Expr, Instr}

/** The read-only, `Unit`-returning sibling of [[Walker]] — mirrors
  * `esmeta.ir.util.UnitWalker`. A concrete subclass overrides `walk` for the
  * node type(s) it cares about, typically accumulating into a mutable field as
  * a side effect, and calls `super.walk(...)` to keep recursing (see
  * `esmeta.ir.util.YetCollector` for the canonical shape this mirrors). Same
  * exhaustive default recursion as [[Walker]], just without reconstructing
  * anything.
  */
trait UnitWalker:
  def walk(expr: Expr): Unit = expr match
    case Expr.Field(base, _)    => walk(base)
    case Expr.Index(base, key)  => walk(base); walk(key)
    case Expr.Link(_, args)     => args.foreach(walk)
    case Expr.AlgoCall(_, args) => args.foreach(walk)
    case Expr.Case(_, args)     => args.foreach(walk)
    case Expr.JSCall(_, args)   => args.foreach(walk)
    case Expr.Abrupt(_, e)      => walk(e)
    case Expr.List_(elems)      => elems.foreach(walk)
    case Expr.Map_(entries)  => entries.foreach((k, v) => { walk(k); walk(v) })
    case Expr.Length(e)      => walk(e)
    case Expr.BinOp(l, _, r) => walk(l); walk(r)
    case Expr.Pow(base, exp) => walk(base); walk(exp)
    case Expr.Neg(e)         => walk(e)
    case Expr.AsMath(e)      => walk(e)
    case Expr.AsNumber(e)    => walk(e)
    case Expr.AsBigInt(e)    => walk(e)
    case Expr.Tuple(elems)   => elems.foreach(walk)
    case Expr.NewByteSequence(len) => walk(len)
    case Expr.DataBlockOf(memaddr) => walk(memaddr)
    case Expr.Range(low, high)     => walk(low); walk(high)
    case Expr.IndexOf(list, elem)  => walk(list); walk(elem)
    case Expr.ClosureCall(closure, args) =>
      walk(closure); args.foreach(walk)
    case Expr.TupleProj(base, _) => walk(base)
    case Expr.CaseTag(base)      => walk(base)
    case Expr.Opt(inner)         => inner.foreach(walk)
    case Expr.SuchThat(_, cond)  => walk(cond)
    // leaves with no nested Expr: Var, This, GivenValue, Num, Bool, Str,
    // Byte, SpecTerm, New, UnknownNew, Described, Unknown, Closure,
    // FollowingSteps.
    case _ =>

  def walk(cond: Cond): Unit = cond match
    case Cond.Eq(l, r, _)         => walk(l); walk(r)
    case Cond.Compare(l, _, r)    => walk(l); walk(r)
    case Cond.HasField(e, _)      => walk(e)
    case Cond.Implements(e, _, _) => walk(e)
    case Cond.IsOfForm(e, form, condOpt, _) =>
      walk(e); walk(form); condOpt.foreach(walk)
    case Cond.Matches(l, _, r, _)     => walk(l); walk(r)
    case Cond.IsMissing(e, _)         => walk(e)
    case Cond.HasSlot(e, _, _)        => walk(e)
    case Cond.HasDuplicates(list, _)  => walk(list)
    case Cond.Contains(elem, list, _) => walk(elem); walk(list)
    case Cond.IsType(e, _, _)         => walk(e)
    case Cond.And(l, r)               => walk(l); walk(r)
    case Cond.Or(l, r)                => walk(l); walk(r)
    case Cond.Abbreviated(e)          => walk(e)
    case Cond.Any(_, collections, body) =>
      collections.foreach(walk); walk(body)
    case Cond.Exists(_, body) => walk(body)
    // leaves: Unreachable, Throws, Unknown.
    case _ =>

  def walk(instr: Instr): Unit = instr match
    case i: Instr.Let    => walk(i.lhs); walk(i.expr); i.body.foreach(walk)
    case i: Instr.Set    => walk(i.lhs); walk(i.expr); i.body.foreach(walk)
    case i: Instr.If     => walk(i.cond); i.body.foreach(walk)
    case i: Instr.ElseIf => walk(i.cond); i.body.foreach(walk)
    case i: Instr.Else   => i.body.foreach(walk)
    case i: Instr.Return => i.expr.foreach(walk); i.body.foreach(walk)
    case i: Instr.Assert => walk(i.cond); i.body.foreach(walk)
    case i: Instr.Throw  => i.body.foreach(walk)
    case i: Instr.ForEach =>
      walk(i.elem); walk(i.collection); i.body.foreach(walk)
    case i: Instr.For =>
      walk(i.elem); walk(i.collection); i.body.foreach(walk)
    case i: Instr.While         => walk(i.cond); i.body.foreach(walk)
    case i: Instr.RunInParallel => i.body.foreach(walk)
    case i: Instr.Append =>
      walk(i.item); walk(i.collection); i.body.foreach(walk)
    case i: Instr.Remove   => walk(i.list); i.body.foreach(walk)
    case i: Instr.Continue => i.body.foreach(walk)
    case i: Instr.Perform  => i.args.foreach(walk); i.body.foreach(walk)
    case i: Instr.PerformClosure =>
      walk(i.closure); i.args.foreach(walk); i.body.foreach(walk)
    case i: Instr.Note    => i.body.foreach(walk)
    case i: Instr.Unknown => i.body.foreach(walk)
    case i: Instr.IfChain =>
      i.branches.foreach((c, b) => { walk(c); b.foreach(walk) })
      i.fallback.foreach(walk)
