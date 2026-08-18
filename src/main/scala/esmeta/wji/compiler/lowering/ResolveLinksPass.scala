package esmeta.wji.compiler.lowering

import esmeta.error.UnsupportedSpecShape
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
  *   - a `Link` whose (case-insensitively-compared) name is in `spreadTags` — a
  *     small, hardcoded set of dfn names known to be annotation-only prose
  *     labels, never a callable algorithm and never a real SpecTec case tag
  *     (currently `identifier`, `interface`) — is never wrapped at all: its own
  *     args are spliced straight into whichever call's argument list it sits in
  *     (see `LinkResolver.resolveArgs`). `ExprParser.parseArgs` has no way to
  *     know a tag like `identifier` isn't callable, so it parses whatever
  *     unrelated clauses happen to follow it in the same sentence as if they
  *     were its call arguments, e.g. `identifier(id, interface(I, 0))` from
  *     "with [=identifier=] |id| on [=interface=] |I| and with argument count
  *     0" (webidl/index.bs:12581-12583 and three more sites). A `spreadTags`
  *     link found anywhere *not* inside an argument list — i.e. with no list to
  *     spread into — throws [[esmeta.error.UnsupportedSpecShape]] rather than
  *     silently falling through to the heuristic above, which would just
  *     reproduce the same mis-wrapped-`Case` bug.
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

  /** See this object's own doc — dfn names confirmed to be annotation-only
    * prose labels (checked against `webidl/index.bs`: neither is a `<div
    * algorithm>` name, and neither collides with a genuine SpecTec case tag,
    * that vocabulary being Wasm Core Spec-specific, e.g. `i32.const` — disjoint
    * from WebIDL's own English glossary terms).
    */
  private val spreadTags: Set[String] = Set("identifier", "interface")

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
    * `Instr.Perform.func`, and [[Cond.IsOfForm]]'s `form`).
    */
  private class LinkResolver(known: Set[String], plainKnown: Set[String])
    extends Walker:

    override def walk(expr: Expr): Expr = expr match
      // Every real `spreadTags` occurrence is consumed by `resolveArgs`
      // below, as an element of some enclosing call's argument list, before
      // it ever reaches general `walk`. Reaching here means one showed up
      // somewhere with no argument list to spread into (e.g. as a bare
      // `Let` RHS) — either a genuinely new spec idiom this pass doesn't
      // cover yet, or a bug — so this fails loudly (see this object's own
      // doc) instead of silently falling through to `buildCaseOrCall` below,
      // which would just reproduce the original mis-wrapped-`Case` bug.
      case Expr.Link(link, _) if spreadTags(stripLink(link).toLowerCase) =>
        throw UnsupportedSpecShape(
          "ResolveLinksPass",
          s"spread-only link [=${stripLink(link)}=] found outside a call's argument list: $expr",
        )
      case Expr.Link(link, args) =>
        val resolvedArgs = resolveArgs(args)
        if known.contains(stripLink(link).toLowerCase) then
          Expr.AlgoCall(link, resolvedArgs)
        else buildCaseOrCall(link, resolvedArgs)
      case Expr.JSCall(name, args) =>
        Expr.JSCall(resolveFuncName(plainKnown, name), args.map(walk))
      case other => super.walk(other)

    override def walk(instr: Instr): Instr = instr match
      case i: Instr.Perform =>
        Instr.Perform(
          resolveFuncName(plainKnown, i.func),
          resolveArgs(i.args),
          i.outcome,
          i.body.map(walk),
        )
      case other => super.walk(other)

    override def walk(cond: Cond): Cond = cond match
      case Cond.IsOfForm(e, form, condOpt, neg) =>
        Cond.IsOfForm(walk(e), resolveForm(form), condOpt.map(walk), neg)
      case other => super.walk(other)

    /** A call's argument list, with any element that's itself a
      * `spreadTags`-named `Link` "spread" into the arguments *it* captured
      * rather than walked into a nested `Case`/`AlgoCall`/`SpecTerm` — see
      * `spreadTags`'s own doc. Recurses so a chain of spread-only tags (e.g.
      * `identifier(id, interface(I, n))`, `ExprParser.parseArgs`'s nesting for
      * "with [=identifier=] |id| on [=interface=] |I| and with argument count
      * |n|") fully flattens in one pass.
      */
    private def resolveArgs(args: List[Expr]): List[Expr] =
      args.flatMap {
        case Expr.Link(link, innerArgs)
            if spreadTags(stripLink(link).toLowerCase) =>
          resolveArgs(innerArgs)
        case other => List(walk(other))
      }

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
      case Expr.Link(link, _) if spreadTags(stripLink(link).toLowerCase) =>
        throw UnsupportedSpecShape(
          "ResolveLinksPass",
          s"spread-only link [=${stripLink(link)}=] found as a Cond.IsOfForm form, which is never a call's argument list: $form",
        )
      case Expr.Link(link, args) => buildCaseOrCall(link, resolveArgs(args))
      case other                 => walk(other)
