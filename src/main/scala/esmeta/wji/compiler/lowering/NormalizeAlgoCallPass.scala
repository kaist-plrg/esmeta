package esmeta.wji.compiler.lowering

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
object NormalizeAlgoCallPass extends LoweringPass:
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
    // the call itself is left in place for ExtractInlineAlgoCallPass to
    // convert directly to Perform, but its own args can still hide a nested
    // call (e.g. `Let x be F(G(...))`) that needs hoisting first — args.map
    // is applied at the same level as extractFromExpr's own AlgoCall/JSCall
    // cases below, so both paths agree on what "already extracted" means.
    case Expr.AlgoCall(link, args) =>
      val (bs, newArgs) = args.map(extractFromExpr).unzip
      (bs.flatten, Expr.AlgoCall(link, newArgs))
    case Expr.Abrupt("!", Expr.AlgoCall(link, args)) =>
      val (bs, newArgs) = args.map(extractFromExpr).unzip
      (bs.flatten, Expr.Abrupt("!", Expr.AlgoCall(link, newArgs)))
    case Expr.JSCall(name, args) =>
      val (bs, newArgs) = args.map(extractFromExpr).unzip
      (bs.flatten, Expr.JSCall(name, newArgs))
    case Expr.Abrupt("!", Expr.JSCall(name, args)) =>
      val (bs, newArgs) = args.map(extractFromExpr).unzip
      (bs.flatten, Expr.Abrupt("!", Expr.JSCall(name, newArgs)))
    case _ => extractFromExpr(expr)

  // ── Expr normalization ───────────────────────────────────────────────────────

  private def extractFromExpr(expr: Expr): (List[Instr.Let], Expr) = expr match

    case Expr.AlgoCall(link, args) if args.nonEmpty =>
      // hoist any call nested in this call's own args first (e.g. `F(G(...))`)
      // — otherwise G survives, unextracted, as a nonempty-arg AlgoCall inside
      // the Let this produces, which compileExpr can't handle either.
      val (argBs, newArgs) = args.map(extractFromExpr).unzip
      val tmp = fresh()
      (
        argBs.flatten :+ Instr.Let(Expr.Var(tmp), Expr.AlgoCall(link, newArgs)),
        Expr.Var(tmp),
      )

    case Expr.Abrupt("!", Expr.AlgoCall(link, args)) if args.nonEmpty =>
      val (argBs, newArgs) = args.map(extractFromExpr).unzip
      val tmp = fresh()
      (
        argBs.flatten :+ Instr.Let(Expr.Var(tmp), Expr.AlgoCall(link, newArgs)),
        Expr.Var(tmp),
      )

    // unlike AlgoCall, JSCall's `[$name$](...)` syntax always carries a
    // (possibly empty) argument list, so there is no zero-arg/bare-reference
    // ambiguity to preserve — every JSCall is a call.
    case Expr.JSCall(name, args) =>
      val (argBs, newArgs) = args.map(extractFromExpr).unzip
      val tmp = fresh()
      (
        argBs.flatten :+ Instr.Let(Expr.Var(tmp), Expr.JSCall(name, newArgs)),
        Expr.Var(tmp),
      )

    case Expr.Abrupt("!", Expr.JSCall(name, args)) =>
      val (argBs, newArgs) = args.map(extractFromExpr).unzip
      val tmp = fresh()
      (
        argBs.flatten :+ Instr.Let(Expr.Var(tmp), Expr.JSCall(name, newArgs)),
        Expr.Var(tmp),
      )

    // a Case is a constructor/pattern, not a call — never itself hoisted the
    // way an AlgoCall/JSCall is — but its args may still contain calls
    case Expr.Case(tag, args) =>
      val (bs, es) = args.map(extractFromExpr).unzip
      (bs.flatten, Expr.Case(tag, es))

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

    // `collections` is evaluated once, before ExpandMatchesExistsPass's
    // generated loop even starts, so hoisting a call out of it here is safe
    // — unlike `body`, which runs once per iteration and references the
    // loop-bound `binder`; extracting from `body` at this stage (before that
    // loop exists) would evaluate it in the wrong scope, so it's deliberately
    // left alone for ExpandMatchesExistsPass's own (loop-aware) handling.
    case Cond.Exists(binder, collections, body) =>
      val (bs, es) = collections.map(extractFromExpr).unzip
      (bs.flatten, Cond.Exists(binder, es, body))

    case other => (Nil, other)
