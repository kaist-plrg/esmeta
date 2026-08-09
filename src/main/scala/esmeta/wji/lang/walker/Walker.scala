package esmeta.wji.lang.walker

import esmeta.wji.lang.{Cond, Expr, Instr}

/** A structural rewriter for metalang ASTs, mirroring `esmeta.ir.util.Walker`:
  * a concrete pass extends this and overrides only the `walk` overload for the
  * node type(s) it actually transforms, calling `super.walk(...)` for
  * everything else. Every other node type recurses through its children
  * unchanged by default — a pass never needs to hand-write the full exhaustive
  * `Expr`/`Cond`/`Instr` case list itself just to reach whichever one or two
  * constructors it actually cares about, and dynamic dispatch means an override
  * on one node type is picked up wherever the default recursion for any *other*
  * node type reaches it (see `esmeta.ir.util. YetCollector` for the same
  * pattern already proven in mainline ESMeta).
  */
trait Walker:
  def walk(expr: Expr): Expr = expr match
    case Expr.Field(base, name)    => Expr.Field(walk(base), name)
    case Expr.Index(base, key)     => Expr.Index(walk(base), walk(key))
    case Expr.Link(link, args)     => Expr.Link(link, args.map(walk))
    case Expr.AlgoCall(link, args) => Expr.AlgoCall(link, args.map(walk))
    case Expr.Case(tag, args)      => Expr.Case(tag, args.map(walk))
    case Expr.JSCall(name, args)   => Expr.JSCall(name, args.map(walk))
    case Expr.Abrupt(check, e)     => Expr.Abrupt(check, walk(e))
    case Expr.List_(elems)         => Expr.List_(elems.map(walk))
    case Expr.Map_(entries) =>
      Expr.Map_(entries.map((k, v) => (walk(k), walk(v))))
    case Expr.Length(e)               => Expr.Length(walk(e))
    case Expr.BinOp(l, op, r)         => Expr.BinOp(walk(l), op, walk(r))
    case Expr.Pow(base, exp)          => Expr.Pow(walk(base), walk(exp))
    case Expr.Neg(e)                  => Expr.Neg(walk(e))
    case Expr.AsMath(e)               => Expr.AsMath(walk(e))
    case Expr.AsNumber(e)             => Expr.AsNumber(walk(e))
    case Expr.AsBigInt(e)             => Expr.AsBigInt(walk(e))
    case Expr.Tuple(elems)            => Expr.Tuple(elems.map(walk))
    case Expr.NewByteSequence(length) => Expr.NewByteSequence(walk(length))
    case Expr.DataBlockOf(memaddr)    => Expr.DataBlockOf(walk(memaddr))
    case Expr.Range(low, high)        => Expr.Range(walk(low), walk(high))
    case Expr.IndexOf(list, elem)     => Expr.IndexOf(walk(list), walk(elem))
    case Expr.ClosureCall(closure, args) =>
      Expr.ClosureCall(walk(closure), args.map(walk))
    case Expr.TupleProj(base, idx) => Expr.TupleProj(walk(base), idx)
    case Expr.CaseTag(base)        => Expr.CaseTag(walk(base))
    case Expr.Opt(inner)           => Expr.Opt(inner.map(walk))
    case Expr.SuchThat(desc, cond) => Expr.SuchThat(desc, walk(cond))
    // leaves with no nested Expr: Var, This, GivenValue, Num, Bool, Str,
    // Byte, SpecTerm, New, UnknownNew, Described, Unknown, Closure,
    // FollowingSteps.
    case other => other

  def walk(cond: Cond): Cond = cond match
    case Cond.Eq(l, r, neg)     => Cond.Eq(walk(l), walk(r), neg)
    case Cond.Compare(l, op, r) => Cond.Compare(walk(l), op, walk(r))
    case Cond.HasField(e, neg)  => Cond.HasField(walk(e), neg)
    case Cond.Implements(e, iface, neg) =>
      Cond.Implements(walk(e), iface, neg)
    case Cond.IsOfForm(e, form, condOpt, neg) =>
      Cond.IsOfForm(walk(e), walk(form), condOpt.map(walk), neg)
    case Cond.Matches(l, t, r, neg)    => Cond.Matches(walk(l), t, walk(r), neg)
    case Cond.IsMissing(e, neg)        => Cond.IsMissing(walk(e), neg)
    case Cond.HasSlot(e, slot, neg)    => Cond.HasSlot(walk(e), slot, neg)
    case Cond.HasDuplicates(list, neg) => Cond.HasDuplicates(walk(list), neg)
    case Cond.Contains(elem, list, neg) =>
      Cond.Contains(walk(elem), walk(list), neg)
    case Cond.IsType(e, t, neg) => Cond.IsType(walk(e), t, neg)
    case Cond.Exposed(subject, realm, neg) =>
      Cond.Exposed(walk(subject), walk(realm), neg)
    case Cond.And(l, r)      => Cond.And(walk(l), walk(r))
    case Cond.Or(l, r)       => Cond.Or(walk(l), walk(r))
    case Cond.Abbreviated(e) => Cond.Abbreviated(walk(e))
    case Cond.Any(binder, collections, body) =>
      Cond.Any(binder, collections.map(walk), walk(body))
    case Cond.Exists(binder, body) => Cond.Exists(binder, walk(body))
    // leaves: Unreachable, Throws, Unknown.
    case other => other

  def walk(instr: Instr): Instr = instr match
    case i: Instr.Let => Instr.Let(walk(i.lhs), walk(i.expr), i.body.map(walk))
    case i: Instr.Set => Instr.Set(walk(i.lhs), walk(i.expr), i.body.map(walk))
    case i: Instr.If  => Instr.If(walk(i.cond), i.body.map(walk))
    case i: Instr.ElseIf => Instr.ElseIf(walk(i.cond), i.body.map(walk))
    case i: Instr.Else   => Instr.Else(i.body.map(walk))
    case i: Instr.Return => Instr.Return(i.expr.map(walk), i.body.map(walk))
    case i: Instr.Assert => Instr.Assert(walk(i.cond), i.body.map(walk))
    case i: Instr.Throw  => Instr.Throw(walk(i.target), i.body.map(walk))
    case i: Instr.ForEach =>
      Instr.ForEach(walk(i.elem), walk(i.collection), i.body.map(walk))
    case i: Instr.For =>
      Instr.For(walk(i.elem), walk(i.collection), i.body.map(walk))
    case i: Instr.While         => Instr.While(walk(i.cond), i.body.map(walk))
    case i: Instr.RunInParallel => Instr.RunInParallel(i.body.map(walk))
    case i: Instr.Append =>
      Instr.Append(walk(i.item), walk(i.collection), i.body.map(walk))
    case i: Instr.Remove =>
      Instr.Remove(
        walk(i.list),
        i.elemKind,
        i.property,
        i.body.map(walk),
      )
    case i: Instr.Pop =>
      Instr.Pop(walk(i.lhs), walk(i.list), i.body.map(walk))
    case i: Instr.Continue => Instr.Continue(i.body.map(walk))
    case i: Instr.Perform =>
      Instr.Perform(i.func, i.args.map(walk), i.outcome, i.body.map(walk))
    case i: Instr.PerformClosure =>
      Instr.PerformClosure(
        walk(i.closure),
        i.args.map(walk),
        i.outcome,
        i.body.map(walk),
      )
    case i: Instr.Note    => Instr.Note(i.text, i.body.map(walk))
    case i: Instr.Unknown => Instr.Unknown(i.text, i.body.map(walk))
    case i: Instr.IfChain =>
      Instr.IfChain(
        i.branches.map((c, b) => (walk(c), b.map(walk))),
        i.fallback.map(walk),
      )
