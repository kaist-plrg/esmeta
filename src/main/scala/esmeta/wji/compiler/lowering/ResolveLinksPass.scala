package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}

/** Resolves every [[Expr.Link]] — a raw `[=...=]` Bikeshed autolink, parsed
  * before it's known whether it names a callable algorithm — into either an
  * [[Expr.AlgoCall]] or an [[Expr.SpecTerm]].
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
  *   - a `Link` whose name matches a known algorithm becomes an `AlgoCall`.
  *   - a `Link` used with arguments *also* becomes an `AlgoCall` even when the
  *     name isn't one of ours — it's still unambiguously a call syntactically
  *     (an argument list doesn't attach to a term reference), just one resolved
  *     elsewhere at compile time: a Wasm embedding function (e.g.
  *     `func_invoke`, see `esmeta.wji.bridge.host.WasmHost`) or an ECMA-262/
  *     WebIDL AO (e.g. `MakeBasicObject`, dispatched via `Compiler`'s merged
  *     `cfg.fnameMap`). Wasm Core Spec math/value notation used with args
  *     (`ℤ(...)`, `signed_32(...)`, `ref.func(...)`) is a known gap this
  *     doesn't yet handle — those still become `AlgoCall` too, and will surface
  *     as an "unknown function" at compile time until a future pass recognizes
  *     them.
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
        if args.nonEmpty || known.contains(stripLink(link).toLowerCase) then
          Expr.AlgoCall(link, resolvedArgs)
        else Expr.SpecTerm(stripLink(link))
      // ExprParser's `[=link=](args)` form (unambiguous call syntax) already
      // parses straight into AlgoCall; still need to recurse into its args
      // in case one of them is itself an unresolved Link.
      case Expr.AlgoCall(link, args) => Expr.AlgoCall(link, args.map(go))
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
