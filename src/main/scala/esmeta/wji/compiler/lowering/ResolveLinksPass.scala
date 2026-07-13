package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}

/** Resolves every [[Expr.Link]] — a raw `[=...=]` Bikeshed autolink, parsed
  * before it's known whether it names a callable algorithm — into an
  * [[Expr.AlgoCall]], an [[Expr.Case]], or an [[Expr.SpecTerm]].
  *
  * A Bikeshed `[=...=]` autolink can point to a `<div algorithm>` (an actually
  * callable operation, e.g. `[=module_decode=]`) or to a plain `<dfn>`/prose
  * definition (e.g. `[=current Realm=]`, which links to ECMA-262's "Execution
  * Contexts" section, not an algorithm). [[ExprParser]] can't tell these apart
  * — it only ever sees one link string at a time — so it parses every
  * bracket-link into a neutral `Link` rather than presupposing it's a call.
  * This pass runs once every algorithm has been extracted, when the full set of
  * real algorithm names is known, and is the only place that decides:
  *
  *   - a `Link` whose name matches a known algorithm becomes an `AlgoCall`,
  *     regardless of args.
  *   - a `Link` used with args that doesn't match a known algorithm is either a
  *     Wasm embedding function / ECMA-262 AO (resolved elsewhere at compile
  *     time — an `AlgoCall`) or a SpecTec Wasm Core Spec constructor/variant
  *     application (an `Expr.Case`, e.g. `[=i32.const=] |u32|`,
  *     `[=external-type/func=] |functype|`) — syntactically indistinguishable
  *     here in general, so this pass falls back to a heuristic: a genuine
  *     SpecTec notation term is always a single dot/slash/hyphen-joined token,
  *     never a multi-word phrase, so a link whose text contains a space (e.g.
  *     `converted to a JavaScript value`, filtered out of `known` by
  *     `SpecFile.webidlFilter`) is assumed to be prose referring to an
  *     algorithm and stays `AlgoCall`; anything else becomes `Case`. This is
  *     imperfect — a handful of genuine multi-word SpecTec constructor names
  *     (`external value`, `memory type`) are misclassified as `AlgoCall` too —
  *     but it's a deliberate, simple tradeoff over a larger lookup table.
  *   - a zero-arg `Link` that doesn't match a known algorithm becomes a
  *     `SpecTerm` — a bare reference to something else.
  */
object ResolveLinksPass extends LoweringPass:
  def run(algos: List[Algorithm]): List[Algorithm] =
    // lower-cased: Bikeshed link matching is case-insensitive (e.g. a
    // sentence-initial "Read the imports" links to a dfn written "read the
    // imports"), so names must be compared case-insensitively here too.
    val known = algos.flatMap(_.name).map(_.toLowerCase).toSet
    algos.map(a => a.copy(body = a.body.map(rewriteInstr(known))))

  private def rewriteInstr(known: Set[String])(instr: Instr): Instr =
    val e = rewriteExpr(known)
    val c = rewriteCond(known)
    val rewritten: Instr = instr match
      case i: Instr.Let    => i.copy(lhs = e(i.lhs), expr = e(i.expr))
      case i: Instr.Set    => i.copy(lhs = e(i.lhs), expr = e(i.expr))
      case i: Instr.If     => i.copy(cond = c(i.cond))
      case i: Instr.ElseIf => i.copy(cond = c(i.cond))
      case i: Instr.Return => i.copy(expr = i.expr.map(e))
      case i: Instr.Assert => i.copy(cond = c(i.cond))
      case i: Instr.While  => i.copy(cond = c(i.cond))
      case i: Instr.Append =>
        i.copy(item = e(i.item), collection = e(i.collection))
      case i: Instr.ForEach =>
        i.copy(elem = e(i.elem), collection = e(i.collection))
      case i: Instr.Perform => i.copy(args = i.args.map(e))
      case i: Instr.IfChain =>
        i.copy(branches = i.branches.map((cond, body) => (c(cond), body)))
      case other => other
    rewritten.mapBody(_.map(rewriteInstr(known)))

  /** `Link`/`AlgoCall`'s `link` field is stored with its `[=...=]` delimiters
    * (see `Compiler.nameFromLink`); `Algorithm.name` is not, so the two must be
    * normalized to the same form before comparing.
    */
  private def stripLink(link: String): String =
    link.stripPrefix("[=").stripSuffix("=]").trim

  private def rewriteExpr(known: Set[String])(expr: Expr): Expr =
    val go = rewriteExpr(known)
    expr match
      case Expr.Link(link, args) =>
        val resolvedArgs = args.map(go)
        if known.contains(stripLink(link).toLowerCase) then
          Expr.AlgoCall(link, resolvedArgs)
        else if args.nonEmpty && !stripLink(link).contains(" ") then
          // a link with args that doesn't name a known algorithm is either a
          // Wasm embedding function / ECMA-262 AO (resolved elsewhere at
          // compile time — still AlgoCall) or a SpecTec Wasm Core Spec
          // constructor/variant application (e.g. `[=i32.const=] |u32|`,
          // `[=external-type/func=] |functype|`) — syntactically
          // indistinguishable here in general, so both would start as `Case`
          // and `esmeta.wji.compiler.Compiler` treats an unresolved `Case`
          // tag as a call the same way it does an unresolved `AlgoCall` link
          // — except a genuine SpecTec notation term is always a single
          // dot/slash/hyphen-joined token (`i32.const`, `external-type/func`,
          // `ref.null`), never a multi-word phrase, so a `link` containing a
          // space (`converted to a JavaScript value`, `a promise rejected
          // with`) is assumed to be prose referring to an algorithm — usually
          // one filtered out of `known` (see `SpecFile.webidlFilter`) rather
          // than genuinely unrecognizable — and stays `AlgoCall`.
          Expr.Case(link, resolvedArgs)
        else if args.nonEmpty then Expr.AlgoCall(link, resolvedArgs)
        else Expr.SpecTerm(stripLink(link))
      // ExprParser's `[=link=](args)` form (unambiguous call syntax) already
      // parses straight into AlgoCall; still need to recurse into its args
      // in case one of them is itself an unresolved Link.
      case Expr.AlgoCall(link, args) => Expr.AlgoCall(link, args.map(go))
      case Expr.Case(tag, args)      => Expr.Case(tag, args.map(go))
      case Expr.JSCall(name, args)   => Expr.JSCall(name, args.map(go))
      case Expr.Field(base, name)    => Expr.Field(go(base), name)
      case Expr.Index(base, key)     => Expr.Index(go(base), go(key))
      case Expr.Abrupt(check, e)     => Expr.Abrupt(check, go(e))
      case Expr.List_(elems)         => Expr.List_(elems.map(go))
      case Expr.Map_(entries) =>
        Expr.Map_(entries.map((k, v) => (go(k), go(v))))
      case Expr.Length(e)       => Expr.Length(go(e))
      case Expr.BinOp(l, op, r) => Expr.BinOp(go(l), op, go(r))
      case Expr.Pow(base, exp)  => Expr.Pow(go(base), go(exp))
      case Expr.Neg(e)          => Expr.Neg(go(e))
      case Expr.AsMath(e)       => Expr.AsMath(go(e))
      case Expr.Tuple(elems)    => Expr.Tuple(elems.map(go))
      case other                => other

  private def rewriteCond(known: Set[String])(cond: Cond): Cond =
    val e = rewriteExpr(known)
    val go = rewriteCond(known)
    cond match
      case Cond.Eq(l, r, neg)     => Cond.Eq(e(l), e(r), neg)
      case Cond.Compare(l, op, r) => Cond.Compare(e(l), op, e(r))
      case Cond.HasField(ex, neg) => Cond.HasField(e(ex), neg)
      case Cond.Implements(ex, iface, neg) =>
        Cond.Implements(e(ex), iface, neg)
      case Cond.IsOfForm(ex, form, condOpt, neg) =>
        Cond.IsOfForm(e(ex), e(form), condOpt.map(go), neg)
      case Cond.Matches(l, t, r, neg) => Cond.Matches(e(l), t, e(r), neg)
      case Cond.IsMissing(ex, neg)    => Cond.IsMissing(e(ex), neg)
      case Cond.IsType(ex, t, neg)    => Cond.IsType(e(ex), t, neg)
      case Cond.And(l, r)             => Cond.And(go(l), go(r))
      case Cond.Or(l, r)              => Cond.Or(go(l), go(r))
      case Cond.Abbreviated(ex)       => Cond.Abbreviated(e(ex))
      case other                      => other
