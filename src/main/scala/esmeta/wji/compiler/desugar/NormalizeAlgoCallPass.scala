package esmeta.wji.compiler.desugar

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}

/** ANF-style normalization: extracts every `AlgoCall`-with-args that appears in
  * a non-trivial expression position into a preceding `Let` binding.
  *
  * After this pass every `AlgoCall(f, args)` (args.nonEmpty) is either directly
  * the RHS of a `Let` or `Return` (handled by [[ExtractInlineAlgoCallPass]]) or
  * fully eliminated by substitution.
  *
  * Examples:
  * {{{
  *   Append(AlgoCall(f, args), coll)
  *   →  Let(_callN, AlgoCall(f, args))
  *       Append(Var("_callN"), coll)
  *
  *   IfChain([(Eq(AlgoCall(f, args), Bool(false)), body), ...], fb)
  *   →  Let(_callN, AlgoCall(f, args))
  *       IfChain([(Eq(Var("_callN"), Bool(false)), body), ...], fb)
  *
  *   Set(lhs, BinOp(AlgoCall(f, args), "+", Num("1")))
  *   →  Let(_callN, AlgoCall(f, args))
  *       Set(lhs, BinOp(Var("_callN"), "+", Num("1")))
  * }}}
  *
  * [[Instr.Let]] RHS and [[Instr.Return]] value are intentionally skipped at
  * the top level — [[ExtractInlineAlgoCallPass]] converts those to `Perform`
  * directly without an intermediate variable.
  *
  * `!`-wrapped calls (`Abrupt("!", AlgoCall(…))`) are stripped of the wrapper
  * before extraction.
  *
  * For [[Cond.And]] / [[Cond.Or]], only the left (unconditionally-evaluated)
  * side is normalized to preserve short-circuit semantics.
  *
  * [[Instr.While]] conditions are never extracted (they are re-evaluated each
  * iteration; extraction would change semantics).
  */
object NormalizeAlgoCallPass extends DesugarPass:
  private var counter = 0
  private def fresh(): String = { counter += 1; s"_call$counter" }

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = transform(a.body)))

  // ── Instr traversal ──────────────────────────────────────────────────────────

  private def transform(instrs: List[Instr]): List[Instr] =
    instrs.flatMap(normalizeInstr)

  private def normalizeInstr(instr: Instr): List[Instr] = instr match

    case Instr.Let(lhs, rhs, body) =>
      val (bindings, newRhs) = skipTopAlgoCall(rhs)
      bindings ++ List(Instr.Let(lhs, newRhs, transform(body)))

    case Instr.Return(Some(rhs), body) =>
      val (bindings, newRhs) = skipTopAlgoCall(rhs)
      bindings ++ List(Instr.Return(Some(newRhs), transform(body)))

    case Instr.Set(lhs, rhs, body) =>
      val (bindings, newRhs) = extractFromExpr(rhs)
      bindings ++ List(Instr.Set(lhs, newRhs, transform(body)))

    case Instr.Append(item, coll, body) =>
      val (ib, ie) = extractFromExpr(item)
      val (cb, ce) = extractFromExpr(coll)
      ib ++ cb ++ List(Instr.Append(ie, ce, transform(body)))

    case Instr.Perform(func, args, outcome, body) =>
      val (bs, newArgs) = args.map(extractFromExpr).unzip
      bs.flatten ++ List(Instr.Perform(func, newArgs, outcome, transform(body)))

    case Instr.Assert(cond, body) =>
      val (bindings, newCond) = extractFromCond(cond)
      bindings ++ List(Instr.Assert(newCond, transform(body)))

    case Instr.IfChain(branches, fallback) =>
      branches match
        case (cond, body) :: rest =>
          val (bindings, newCond) = extractFromCond(cond)
          val newChain = Instr.IfChain(
            (newCond, transform(body)) :: rest.map((c, b) => (c, transform(b))),
            transform(fallback),
          )
          bindings ++ List(newChain)
        case Nil =>
          List(Instr.IfChain(Nil, transform(fallback)))

    case Instr.While(cond, body) =>
      List(Instr.While(cond, transform(body)))

    case _ =>
      List(instr.mapBody(transform))

  /** Like [[extractFromExpr]] but returns `(Nil, expr)` when the top-level
    * expression is already a bare AlgoCall/JSCall (or `!`-wrapped) — those are
    * left for [[ExtractInlineAlgoCallPass]] to convert directly to `Perform`
    * without an intermediate variable.
    */
  private def skipTopAlgoCall(expr: Expr): (List[Instr.Let], Expr) = expr match
    case Expr.AlgoCall(_, _)                   => (Nil, expr)
    case Expr.Abrupt("!", Expr.AlgoCall(_, _)) => (Nil, expr)
    case Expr.JSCall(_, _)                     => (Nil, expr)
    case Expr.Abrupt("!", Expr.JSCall(_, _))   => (Nil, expr)
    case _                                     => extractFromExpr(expr)

  // ── Expr normalization ───────────────────────────────────────────────────────

  private def extractFromExpr(expr: Expr): (List[Instr.Let], Expr) = expr match

    case Expr.AlgoCall(link, args) if args.nonEmpty =>
      val tmp = fresh()
      (List(Instr.Let(Expr.Var(tmp), expr)), Expr.Var(tmp))

    case Expr.Abrupt("!", Expr.AlgoCall(link, args)) if args.nonEmpty =>
      val tmp = fresh()
      (List(Instr.Let(Expr.Var(tmp), Expr.AlgoCall(link, args))), Expr.Var(tmp))

    // unlike AlgoCall, JSCall's `[$name$](...)` syntax always carries a
    // (possibly empty) argument list, so there is no zero-arg/bare-reference
    // ambiguity to preserve — every JSCall is a call.
    case Expr.JSCall(_, _) =>
      val tmp = fresh()
      (List(Instr.Let(Expr.Var(tmp), expr)), Expr.Var(tmp))

    case Expr.Abrupt("!", Expr.JSCall(name, args)) =>
      val tmp = fresh()
      (List(Instr.Let(Expr.Var(tmp), Expr.JSCall(name, args))), Expr.Var(tmp))

    case Expr.BinOp(l, op, r) =>
      val (lb, le) = extractFromExpr(l)
      val (rb, re) = extractFromExpr(r)
      (lb ++ rb, Expr.BinOp(le, op, re))

    case Expr.Pow(base, exp) =>
      val (bb, be) = extractFromExpr(base)
      val (eb, ee) = extractFromExpr(exp)
      (bb ++ eb, Expr.Pow(be, ee))

    case Expr.Neg(e) => val (b, ne) = extractFromExpr(e); (b, Expr.Neg(ne))
    case Expr.AsMath(e) =>
      val (b, ne) = extractFromExpr(e); (b, Expr.AsMath(ne))
    case Expr.Length(e) =>
      val (b, ne) = extractFromExpr(e); (b, Expr.Length(ne))

    case Expr.Abrupt(check, e) =>
      val (b, ne) = extractFromExpr(e)
      (b, Expr.Abrupt(check, ne))

    case Expr.Field(base, name) =>
      val (b, be) = extractFromExpr(base)
      (b, Expr.Field(be, name))

    case Expr.Index(base, key) =>
      val (bb, be) = extractFromExpr(base)
      val (kb, ke) = extractFromExpr(key)
      (bb ++ kb, Expr.Index(be, ke))

    case Expr.List_(elems) =>
      val (bs, es) = elems.map(extractFromExpr).unzip
      (bs.flatten, Expr.List_(es))

    case Expr.Tuple(elems) =>
      val (bs, es) = elems.map(extractFromExpr).unzip
      (bs.flatten, Expr.Tuple(es))

    case Expr.Map_(entries) =>
      val (bs, es) = entries.map { (k, v) =>
        val (kb, ke) = extractFromExpr(k)
        val (vb, ve) = extractFromExpr(v)
        (kb ++ vb, (ke, ve))
      }.unzip
      (bs.flatten, Expr.Map_(es))

    case _ => (Nil, expr)

  // ── Cond normalization ───────────────────────────────────────────────────────

  private def extractFromCond(cond: Cond): (List[Instr.Let], Cond) = cond match

    case Cond.Eq(l, r, neg) =>
      val (lb, le) = extractFromExpr(l)
      val (rb, re) = extractFromExpr(r)
      (lb ++ rb, Cond.Eq(le, re, neg))

    case Cond.Compare(l, op, r) =>
      val (lb, le) = extractFromExpr(l)
      val (rb, re) = extractFromExpr(r)
      (lb ++ rb, Cond.Compare(le, op, re))

    case Cond.IsType(e, t, neg) =>
      val (b, ne) = extractFromExpr(e); (b, Cond.IsType(ne, t, neg))

    case Cond.HasField(e, neg) =>
      val (b, ne) = extractFromExpr(e); (b, Cond.HasField(ne, neg))

    case Cond.Implements(e, iface, neg) =>
      val (b, ne) = extractFromExpr(e); (b, Cond.Implements(ne, iface, neg))

    case Cond.IsMissing(e, neg) =>
      val (b, ne) = extractFromExpr(e); (b, Cond.IsMissing(ne, neg))

    case Cond.IsOfForm(e, form, condOpt, neg) =>
      val (b, ne) = extractFromExpr(e);
      (b, Cond.IsOfForm(ne, form, condOpt, neg))

    case Cond.And(l, r) =>
      val (lb, lc) = extractFromCond(l)
      (lb, Cond.And(lc, r))

    case Cond.Or(l, r) =>
      val (lb, lc) = extractFromCond(l)
      (lb, Cond.Or(lc, r))

    case other => (Nil, other)
