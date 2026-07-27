package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, AlgorithmKind, Cond, Expr, Instr}
import esmeta.wji.lang.walker.Walker

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
  *     larger lookup table. Built flat here regardless of what the real
  *     `ALValue.CaseV` shape actually nests like — see
  *     [[NormalizeSpecTecCaseShapePass]], the very next pass, for that.
  *   - a zero-arg `Link` that doesn't match a known algorithm becomes a
  *     `SpecTerm` — a bare reference to something else.
  *
  * A [[Cond.IsOfForm]]'s `form` field is the one exception to the first rule
  * above: it's always a pattern to destructure against (`ExpandIsOfFormPass`
  * only ever handles a `form` that's already `Expr.Case`), never a genuine
  * call, even when its link text happens to also be a real algorithm's
  * (lowercased) name — e.g. embedding.rst's 1-arg `exception` case tag collides
  * with js-api's `Exception` constructor. Resolved via
  * [[LinkResolver.resolveForm]], the same heuristic minus that first check.
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
    val resolver = LinkResolver(known, plainKnown)
    algos.map(a => a.copy(body = a.body.map(resolver.walk)))

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

  /** The `AlgoCall`/`Case`/`SpecTerm` a link with already-resolved `args`
    * becomes, once it's known not to be a call to a known algorithm — shared
    * between [[LinkResolver.walk(Expr)]] and [[LinkResolver.resolveForm]] (the
    * latter skips the "known algorithm" branch above this one, but needs the
    * exact same Case/AlgoCall split otherwise). Kept as one function so the two
    * can't silently diverge on this.
    */
  private def buildCaseOrCall(link: String, resolvedArgs: List[Expr]): Expr =
    if resolvedArgs.nonEmpty && !lastSegment(stripLink(link)).contains(" ") then
      // heuristic split between AlgoCall/Case — see class doc above
      Expr.Case(link, resolvedArgs)
    else if resolvedArgs.nonEmpty then Expr.AlgoCall(link, resolvedArgs)
    else Expr.SpecTerm(stripLink(link))

  /** Walks a single algorithm body once, resolving every [[Expr.Link]] against
    * `known`/`plainKnown` — see class doc. Only overrides the node types it
    * actually needs to inspect ([[Expr.Link]]/[[Expr.JSCall]],
    * `Instr.Perform.func`, and [[Cond.IsOfForm]]'s `form`); every other
    * `Expr`/`Cond`/`Instr` is reached by [[Walker]]'s own default recursion.
    */
  private class LinkResolver(known: Set[String], plainKnown: Set[String])
    extends Walker:

    override def walk(expr: Expr): Expr = expr match
      case Expr.Link(link, args) =>
        val resolvedArgs = args.map(walk)
        if known.contains(stripLink(link).toLowerCase) then
          Expr.AlgoCall(link, resolvedArgs)
        else buildCaseOrCall(link, resolvedArgs)
      case Expr.JSCall(name, args) =>
        Expr.JSCall(resolveFuncName(plainKnown, name), args.map(walk))
      case other => super.walk(other)

    override def walk(instr: Instr): Instr = instr match
      case i: Instr.Perform =>
        super.walk(i.copy(func = resolveFuncName(plainKnown, i.func)))
      case other => super.walk(other)

    override def walk(cond: Cond): Cond = cond match
      case Cond.IsOfForm(e, form, condOpt, neg) =>
        Cond.IsOfForm(walk(e), resolveForm(form), condOpt.map(walk), neg)
      case other => super.walk(other)

    /** Resolves a [[Cond.IsOfForm]]'s `form` field specifically — the same
      * [[buildCaseOrCall]] heuristic `walk(Expr)`'s `Expr.Link` case uses,
      * minus its "known algorithm" branch. A form is always a pattern to
      * destructure against, never a call, so that branch would only ever mask
      * the correct `Case`/`SpecTerm` resolution when the form's link text
      * happens to coincide with some unrelated algorithm's name (see class
      * doc).
      *
      * Confirmed load-bearing, not just theoretical: temporarily replacing this
      * call with plain `walk` regresses `call an Exported Function`'s "If
      * |ret| is of the form [=exception=] |exnaddr|" — `[=exception=]` collides
      * (case-insensitively, `known` is lower-cased) with the real `Exception`
      * constructor algorithm, so without this it resolves to `AlgoCall` and
      * `ExpandIsOfFormPass` no longer recognizes it, falling back to `EYet`.
      */
    private def resolveForm(form: Expr): Expr = form match
      case Expr.Link(link, args) => buildCaseOrCall(link, args.map(walk))
      case other                 => walk(other)
