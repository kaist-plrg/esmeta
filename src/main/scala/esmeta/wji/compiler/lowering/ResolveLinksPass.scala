package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, AlgorithmKind, Cond, Expr, Instr}

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
  *     SpecTec notation term's *own* discriminating word (the part after its
  *     last `/`, for a `for`-scoped dfn like `external-type/func` — or the
  *     whole thing, if it isn't `for`-scoped at all, like `i32.const`) is
  *     always a single token, never containing a space — even when the `for`
  *     scope itself is a multi-word dfn (`external value/tag`). So a link whose
  *     *last segment* contains a space (e.g. `converted to a JavaScript value`,
  *     filtered out of `known` by `SpecFile.webidlFilter`) is assumed to be
  *     prose referring to an algorithm and stays `AlgoCall`; anything else
  *     becomes `Case`. Imperfect — a multi-word variant name (none seen so far)
  *     would still be misclassified — but a deliberate, simple tradeoff over a
  *     larger lookup table.
  *   - a zero-arg `Link` that doesn't match a known algorithm becomes a
  *     `SpecTerm` — a bare reference to something else.
  *
  * A [[Cond.IsOfForm]]'s `form` field is the one exception to the first rule
  * above: it's always a pattern to destructure against (`ExpandIsOfFormPass`
  * only ever handles a `form` that's already `Expr.Case`), never a genuine
  * call, even when its link text happens to also be a real algorithm's
  * (lowercased) name — e.g. embedding.rst's 1-arg `exception` case tag collides
  * with js-api's `Exception` constructor (index.bs:1297 vs. 1722). Resolved via
  * [[resolveForm]], the same heuristic minus that first check.
  *
  * Category: Housekeeping.
  */
object ResolveLinksPass extends LoweringPass:
  def run(algos: List[Algorithm]): List[Algorithm] =
    // lower-cased: Bikeshed link matching is case-insensitive (e.g. a
    // sentence-initial "Read the imports" links to a dfn written "read the
    // imports"), so names must be compared case-insensitively here too.
    val known = algos.flatMap(_.name).map(_.toLowerCase).toSet
    // `[$...$]` JSCall/Perform syntax is free-function-call notation, never
    // used for a WebIDL interface member (Method/Getter/Setter/Constructor)
    // — those are only ever invoked on a receiver (`|obj|.member(...)`), a
    // different call shape WJI compiles separately — so a same-named
    // interface member (e.g. {{Table}}'s `get(index)` method) must not
    // shadow a real free-standing AO of the same bare name (ECMA-262's
    // `Get(O, P)`) when resolving a `[$...$]` call; see `resolveFuncName`.
    val plainKnown = algos
      .filter(_.kind == AlgorithmKind.Plain)
      .flatMap(_.name)
      .map(_.toLowerCase)
      .toSet
    algos.map(a => a.copy(body = a.body.map(rewriteInstr(known, plainKnown))))

  private def rewriteInstr(known: Set[String], plainKnown: Set[String])(
    instr: Instr,
  ): Instr =
    val e = rewriteExpr(known, plainKnown)
    val c = rewriteCond(known, plainKnown)
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
      case i: Instr.Perform =>
        i.copy(
          func = resolveFuncName(plainKnown, i.func),
          args = i.args.map(e),
        )
      case i: Instr.IfChain =>
        i.copy(branches = i.branches.map((cond, body) => (c(cond), body)))
      case other => other
    rewritten.mapBody(_.map(rewriteInstr(known, plainKnown)))

  /** `Link`/`AlgoCall`'s `link` field is stored with its `[=...=]` delimiters
    * (see `Compiler.nameFromLink`); `Algorithm.name` is not, so the two must be
    * normalized to the same form before comparing.
    */
  private def stripLink(link: String): String =
    link.stripPrefix("[=").stripSuffix("=]").trim

  /** The part of a (already-`stripLink`ed) link text after its last `/`, or the
    * whole thing if it has none — see the class doc's Case heuristic.
    */
  private def lastSegment(text: String): String =
    text.substring(text.lastIndexOf('/') + 1)

  /** `Instr.Perform`'s `func` / `Expr.JSCall`'s `name`, case-corrected when it
    * actually names a known *free-standing* WJI algorithm (`plainKnown` —
    * excludes interface members, see `run`'s comment) rather than a genuine
    * ECMA-262/WebIDL AO. Both are written and parsed case-preserved (`[$Name$]`
    * syntax is `Compiler.nameFromLink`'s "exact ECMA-262 AO name" case, since a
    * real AO's name must match `cfg.fnameMap` exactly) — but js-api/index.bs
    * also defines its own abstract-ops this same way (e.g. `UnwrapString`,
    * `FromCharCode`, `CharCodeAt`, all `<dfn abstract-op>`s called via
    * `[$...$]`), and `Compiler.compileAlgo` registers every WJI-compiled
    * function lowercased — so, without this, a self-defined abstract-op called
    * via `[$...$]` resolves to nothing at all.
    */
  private def resolveFuncName(plainKnown: Set[String], name: String): String =
    if plainKnown.contains(name.toLowerCase) then name.toLowerCase else name

  /** Resolves a [[Cond.IsOfForm]]'s `form` field specifically — the same
    * AlgoCall/Case/SpecTerm heuristic [[rewriteExpr]]'s `Expr.Link` case uses,
    * minus its first branch (a name matching a known algorithm). A form is
    * always a pattern to destructure against, never a call, so that branch
    * would only ever mask the correct `Case`/`SpecTerm` resolution when the
    * form's link text happens to coincide with some unrelated algorithm's name
    * (see class doc).
    */
  private def resolveForm(known: Set[String], plainKnown: Set[String])(
    form: Expr,
  ): Expr =
    val e = rewriteExpr(known, plainKnown)
    form match
      case Expr.Link(link, args) =>
        val resolvedArgs = args.map(e)
        if args.nonEmpty && !lastSegment(stripLink(link)).contains(" ") then
          Expr.Case(link, resolvedArgs)
        else if args.nonEmpty then Expr.AlgoCall(link, resolvedArgs)
        else Expr.SpecTerm(stripLink(link))
      case other => e(other)

  private def rewriteExpr(known: Set[String], plainKnown: Set[String])(
    expr: Expr,
  ): Expr =
    val go = rewriteExpr(known, plainKnown)
    expr match
      case Expr.Link(link, args) =>
        val resolvedArgs = args.map(go)
        if known.contains(stripLink(link).toLowerCase) then
          Expr.AlgoCall(link, resolvedArgs)
        else if args.nonEmpty && !lastSegment(stripLink(link)).contains(
            " ",
          )
        then
          // heuristic split between AlgoCall/Case — see class doc above
          Expr.Case(link, resolvedArgs)
        else if args.nonEmpty then Expr.AlgoCall(link, resolvedArgs)
        else Expr.SpecTerm(stripLink(link))
      // ExprParser's `[=link=](args)` form (unambiguous call syntax) already
      // parses straight into AlgoCall; still need to recurse into its args
      // in case one of them is itself an unresolved Link.
      case Expr.AlgoCall(link, args) => Expr.AlgoCall(link, args.map(go))
      case Expr.Case(tag, args)      => Expr.Case(tag, args.map(go))
      case Expr.JSCall(name, args) =>
        Expr.JSCall(resolveFuncName(plainKnown, name), args.map(go))
      case Expr.Field(base, name) => Expr.Field(go(base), name)
      case Expr.Index(base, key)  => Expr.Index(go(base), go(key))
      case Expr.Abrupt(check, e)  => Expr.Abrupt(check, go(e))
      case Expr.List_(elems)      => Expr.List_(elems.map(go))
      case Expr.Map_(entries) =>
        Expr.Map_(entries.map((k, v) => (go(k), go(v))))
      case Expr.Length(e)       => Expr.Length(go(e))
      case Expr.BinOp(l, op, r) => Expr.BinOp(go(l), op, go(r))
      case Expr.Pow(base, exp)  => Expr.Pow(go(base), go(exp))
      case Expr.Neg(e)          => Expr.Neg(go(e))
      case Expr.AsMath(e)       => Expr.AsMath(go(e))
      case Expr.Tuple(elems)    => Expr.Tuple(elems.map(go))
      // parsed directly by ExprParser (not synthesized by a later pass), so a
      // Link nested in one of these can already be present when this — the
      // only Link-resolving pass — runs; skipping them here would let it
      // silently reach compileExpr's "unresolved link" case instead.
      case Expr.NewByteSequence(length) => Expr.NewByteSequence(go(length))
      case Expr.Range(low, high)        => Expr.Range(go(low), go(high))
      case Expr.ClosureCall(closure, args) =>
        Expr.ClosureCall(go(closure), args.map(go))
      case other => other

  private def rewriteCond(known: Set[String], plainKnown: Set[String])(
    cond: Cond,
  ): Cond =
    val e = rewriteExpr(known, plainKnown)
    val go = rewriteCond(known, plainKnown)
    cond match
      case Cond.Eq(l, r, neg)     => Cond.Eq(e(l), e(r), neg)
      case Cond.Compare(l, op, r) => Cond.Compare(e(l), op, e(r))
      case Cond.HasField(ex, neg) => Cond.HasField(e(ex), neg)
      case Cond.Implements(ex, iface, neg) =>
        Cond.Implements(e(ex), iface, neg)
      case Cond.IsOfForm(ex, form, condOpt, neg) =>
        Cond.IsOfForm(
          e(ex),
          resolveForm(known, plainKnown)(form),
          condOpt.map(go),
          neg,
        )
      case Cond.Matches(l, t, r, neg) => Cond.Matches(e(l), t, e(r), neg)
      case Cond.IsMissing(ex, neg)    => Cond.IsMissing(e(ex), neg)
      case Cond.IsType(ex, t, neg)    => Cond.IsType(e(ex), t, neg)
      case Cond.And(l, r)             => Cond.And(go(l), go(r))
      case Cond.Or(l, r)              => Cond.Or(go(l), go(r))
      case Cond.Abbreviated(ex)       => Cond.Abbreviated(e(ex))
      case Cond.Exists(binder, collections, body) =>
        Cond.Exists(binder, collections.map(e), go(body))
      case other => other
