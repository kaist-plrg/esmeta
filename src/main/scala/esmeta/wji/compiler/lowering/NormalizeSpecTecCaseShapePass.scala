package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Expr}
import esmeta.wji.lang.walker.Walker

/** Finalizes every `Expr.Case`'s `tag` into SpecTec's real runtime
  * `ALValue.CaseV` tag, reshapes the handful whose runtime nesting is deeper
  * than spec prose writes it, and converts every `Expr.SpecTerm` that's
  * actually a Wasm-boundary nullary/shorthand case in disguise (`error`, a
  * `valtype` literal like `i32`, a `reftype` shorthand like `funcref`) into the
  * real `Expr.Case`/`Expr.Opt` it denotes — wherever any of these show up (a
  * value under construction, or a `Cond.IsOfForm`'s `form` pattern). Runs
  * immediately after [[ResolveLinksPass]], so every downstream pass
  * (`ExpandIsOfFormPass`, `ExpandDestructuringLetPass`, `Compiler`) can treat
  * `Expr.Case` as the sole representation of a Wasm-boundary constructor value
  * — a `SpecTerm` reaching them is *never* secretly one of these, only a
  * genuine spec-term reference (`null`, `current Realm`, ...).
  *
  * Only overrides [[Walker.walk(Expr)]] for the node types it actually rewrites
  * ([[Expr.Case]], and the handful of [[Expr.SpecTerm]] names above) — every
  * other `Expr`/`Cond`/`Instr` (including wherever one of these shows up
  * nested, e.g. inside a `Cond.IsOfForm`'s `form`) is reached automatically by
  * [[Walker]]'s own default structural recursion, with no per-pass exhaustive
  * case list to keep in sync here.
  *
  * Tag translation: a `for`-scoped dfn's linking text is always
  * `family/variant` (e.g. `external value/func`, `external-type/global`; see
  * `SpecPatch` #15/#16, which normalized every such link into exactly this
  * shape), and SpecTec's own variant families consistently name their `CaseV`
  * tag after the uppercased variant alone (confirmed against both `externtype`
  * and `externaddr` in `4.0-execution.configurations.spectec` /
  * `construct.ml`'s `al_of_externtype`) — so the last `/`-segment, uppercased,
  * is the tag, with no per-family table needed. A `tag` that's already a bare
  * runtime tag (e.g. `ExprParser.CompTypeArrow`'s literal `"->"`, no `/` and
  * already the right case) passes through unchanged.
  *
  * Nesting mismatches, both `construct.ml`-confirmed:
  *   - a numeric const (`i32.const`/`i64.const`/`f32.const`/`f64.const`):
  *     `al_to_num` only recognizes `CaseV("CONST", [CaseV(numtypeTag, []),
  *     payload])` — a nested numtype tag, not the flat `CaseV("I32.CONST",
  *     [payload])` `ResolveLinksPass` would otherwise leave in place.
  *   - `external-type/global`: `al_of_externtype`/`al_of_globaltype` nest
  *     `mut`/`valuetype` one level under externtype's GLOBAL variant's sole
  *     arg, even though index.bs writes both components flat directly under
  *     `external-type/global`.
  *   - a non-nullable `reftype` (`[=ref=] |heaptype|`, no `|null|` token):
  *     `al_to_valtype` requires `CaseV("REF", [nullability, heaptype])` always,
  *     but prose only ever spells out the first (nullability) component when
  *     the ref *is* nullable — a non-nullable one leaves it implicit, so only
  *     one arg is ever parsed.
  * {{{
  *   Case("[=i32.const=]", [Var(u32)])
  * }}}
  * becomes
  * {{{
  *   Case("CONST", [Case("I32", []), Var(u32)])
  * }}}
  * and
  * {{{
  *   Case("[=external-type/global=]", [Var(mut), Var(valuetype)])
  * }}}
  * becomes
  * {{{
  *   Case("GLOBAL", [Case("", [Var(mut), Var(valuetype)])])
  * }}}
  * and
  * {{{
  *   Case("[=ref=]", [Case("[=heap-type/any=]", [])])
  * }}}
  * becomes
  * {{{
  *   Case("REF", [Opt(None), Case("ANY", [])])
  * }}}
  * and (see [[refAddrSuffix]]'s own doc for the full `ref`-variant table)
  * {{{
  *   Case("[=ref.host=]", [Var(hostaddr)])
  * }}}
  * becomes
  * {{{
  *   Case("REF.HOST_ADDR", [Var(hostaddr)])
  * }}}
  * — one variant, `REF.NULL`, both renames *and* drops its argument (spec prose
  * always writes a heaptype alongside it, but the runtime null-ref value
  * carries none: `REF.NULL_ADDR` always types as the bottom heap type
  * regardless of context), so `Case("[=ref.null=]", [Var(heaptype)])` becomes
  * the argument-less `Case("REF.NULL_ADDR", [])`.
  *
  * Every other `Case` (an already-correctly-flat variant like
  * `external-type/func`, or one [[ExprParser]] built directly rather than via a
  * link, like `CompTypeArrow`'s `"->"` or `parseUntaggedForm`'s `""`) has its
  * tag translated (a no-op if already final) and its args recursed into, with
  * no extra nesting added — this pass only ever *adds* structure for the two
  * nesting mismatches above, never removes or reinterprets anything else. Once
  * this runs, every downstream pass (`ExpandIsOfFormPass`,
  * `ExpandDestructuringLetPass`, `Compiler`) can treat any `Expr.Case` it sees
  * as already shaped exactly like the real `ALValue.CaseV` it corresponds to,
  * with zero SpecTec knowledge of its own.
  *
  * Category: Spec-dependent — SpecTec.
  */
object NormalizeSpecTecCaseShapePass extends LoweringPass:

  /** Requires:
    *   - [[ResolveLinksPass]]: needs every `Expr.Link` already resolved to
    *     `Expr.Case`/`Expr.AlgoCall`/`Expr.SpecTerm` — nothing here resolves a
    *     raw `Link` itself, just reshapes an already-built `Case`.
    */
  override def requires: Set[LoweringPass] = Set(ResolveLinksPass)

  /** `Link`/`Case`'s tag text may still carry its `[=...=]` delimiters (see
    * `ResolveLinksPass.stripLink`) — strip them the same way before comparing
    * against [[numConstTags]]/[[nestedFormLinks]]'s (bare) keys.
    */
  private def stripLink(tag: String): String =
    tag.stripPrefix("[=").stripSuffix("=]").trim

  /** Translates a `Case.tag` out of spec-link-text form into SpecTec's real
    * runtime `ALValue.CaseV` tag — see class doc's "Tag translation" paragraph.
    * Idempotent on an already-final tag (no `/`, already uppercase), so it's
    * safe to apply unconditionally to every `Case` this pass sees, regardless
    * of where its tag originally came from.
    */
  private def runtimeCaseTag(tag: String): String =
    val name = stripLink(tag)
    name.substring(name.lastIndexOf('/') + 1).trim.toUpperCase

  /** A numeric-const link (e.g. `[=i32.const=]`), mapped to the nested numtype
    * tag `construct.ml`'s `al_to_num` actually expects at position 0 of a
    * `CaseV("CONST", [nested, payload])`.
    */
  private val numConstTags: Map[String, String] = Map(
    "i32.const" -> "I32",
    "i64.const" -> "I64",
    "f32.const" -> "F32",
    "f64.const" -> "F64",
  )

  /** Link names whose args should be wrapped in one extra untagged `Case("",
    * args)` — see class doc's `external-type/global` example. A genuine *value*
    * is never constructed against this link in the corpus today (only matched
    * against, via `Cond.IsOfForm`), but the reshaping itself doesn't need to
    * care which context a matching `Case` shows up in.
    */
  private val nestedFormLinks: Set[String] = Set("external-type/global")

  /** `ref`'s address/immediate-carrying variants — every one whose runtime
    * `CaseV` tag needs a suffix spec prose's own `[=ref.X=]` link text never
    * spells out, but keeps its single argument as-is (contrast `REF.NULL`,
    * handled separately just below, and `REF.EXTERN`, which needs no change at
    * all — it already carries no suffix). Confirmed against the canonical `ref`
    * syntax rule (`4.1-execution.values.spectec`): `REF.I31_NUM i |
    * REF.STRUCT_ADDR a | REF.ARRAY_ADDR a | REF.FUNC_ADDR a | REF.EXN_ADDR a
    * | REF.HOST_ADDR a` — `I31` alone gets `_NUM` (an inline immediate, not a
    * store index); the rest get `_ADDR`.
    */
  private val refAddrSuffix: Map[String, String] = Map(
    "REF.I31" -> "REF.I31_NUM",
    "REF.STRUCT" -> "REF.STRUCT_ADDR",
    "REF.ARRAY" -> "REF.ARRAY_ADDR",
    "REF.FUNC" -> "REF.FUNC_ADDR",
    "REF.EXN" -> "REF.EXN_ADDR",
    "REF.HOST" -> "REF.HOST_ADDR",
  )

  /** A bare Wasm Core Spec `numtype`/`vectype` literal (`i32`/`i64`/`f32`/
    * `f64`/`v128`) — matches iff `s` is one of exactly these 5, extracting the
    * uppercased `Case` tag SpecTec's own `al_of_numtype`/`al_of_vectype`
    * expect.
    */
  private object NullaryValtype:
    private val names = Set("i32", "i64", "f32", "f64", "v128")
    def unapply(s: String): Option[String] =
      Option.when(names.contains(s))(s.toUpperCase)

  /** `funcref`/`externref`/`exnref` — Wasm's nullable-`reftype` shorthand names
    * — extracting the `heaptype` `Case` tag each abbreviates (wrapped below in
    * the full `REF(null?, heaptype)` shape).
    */
  private object ShorthandReftype:
    private val heaptypes =
      Map("funcref" -> "FUNC", "externref" -> "EXTERN", "exnref" -> "EXN")
    def unapply(s: String): Option[String] = heaptypes.get(s)

  /** A bare `heaptype` case tag, referenced with no operand of its own (e.g.
    * `[=heap-type/extern=]`, naming the heaptype rather than constructing
    * something from it) — `ResolveLinksPass.buildCaseOrCall` only applies the
    * `family/variant` Case-vs-SpecTerm split when the link carries args, so a
    * bare reference like this always lands here as a `SpecTerm` instead. Scoped
    * to the `heap-type/` family specifically, not `/`-containing text in
    * general — other `for`-scoped dfns (e.g. HTML's `realm/settings object`,
    * also reachable as a zero-arg `SpecTerm` here) use the exact same link-text
    * shape but aren't SpecTec constructs at all.
    */
  private object HeapType:
    def unapply(s: String): Option[String] =
      Option.when(s.startsWith("heap-type/"))(
        s.substring("heap-type/".length).toUpperCase,
      )

  private object reshaper extends Walker:
    override def walk(expr: Expr): Expr = expr match
      // embedding.rst's `error` production (`error ::= ERROR`) crosses the
      // Wasm boundary as `Wasm(CaseV("ERROR", []))` (see embedding.ml's
      // `embedding_error`) — never a WJI-internal `EEnum`.
      case Expr.SpecTerm("error") => Expr.Case("ERROR", Nil)
      // Wasm Core Spec `valtype` literals (js-api/index.bs's `ToValueType`,
      // `match_valtype` checks, ...) need to actually cross the WasmHost
      // boundary as real SpecTec AL values, not a bare WJI-internal `EEnum`
      // — confirmed against SpecTec's own `al_of_numtype`/`al_of_vectype`
      // (`construct.ml`), which encode these as a plain nullary
      // `CaseV(TAG, [])` tag, uppercased.
      case Expr.SpecTerm(NullaryValtype(tag)) => Expr.Case(tag, Nil)
      // `funcref`/`externref`/`exnref` aren't `valtype` constructors
      // themselves — each is Wasm's own shorthand for a nullable `reftype`,
      // i.e. `REF(null?, heaptype)` with `null?` always present (that's
      // exactly what makes them the *nullable* shorthand). Confirmed against
      // SpecTec's own `al_of_reftype`/`al_of_null` (`construct.ml`,
      // `!version = 3`, this project's configured Wasm version):
      // `CaseV("REF", [OptV(Some(CaseV("NULL", []))), CaseV(<heaptype>, [])])`.
      case Expr.SpecTerm(ShorthandReftype(heaptype)) =>
        Expr.Case(
          "REF",
          List(
            Expr.Opt(Some(Expr.Case("NULL", Nil))),
            Expr.Case(heaptype, Nil),
          ),
        )
      case Expr.SpecTerm(HeapType(tag)) => Expr.Case(tag, Nil)
      case Expr.Case(tag, args) =>
        val reshapedArgs = args.map(walk)
        val stripped = stripLink(tag).toLowerCase
        numConstTags.get(stripped) match
          case Some(numTag) =>
            Expr.Case("CONST", Expr.Case(numTag, Nil) :: reshapedArgs)
          case None =>
            val finalTag = runtimeCaseTag(tag)
            if nestedFormLinks.contains(stripped) then
              Expr.Case(finalTag, List(Expr.Case("", reshapedArgs)))
            // `reftype ::= REF NULL? heaptype` always has 2 AL positions
            // (nullability, heaptype) — but spec prose only writes an
            // explicit `|null|` token when the ref *is* nullable
            // (`[=ref=] |null| |heaptype|`); a non-nullable one
            // (`[=ref=] |heaptype|`, e.g. index.bs:1451's `ref
            // heap-type/any`) has nothing there at all, so `parseArgs` only
            // ever extracts the one heaptype argument. Confirmed against
            // SpecTec's own `al_to_valtype` (`construct.ml`): it rejects a
            // 1-arg `CaseV("REF", [heaptype])` outright
            // (`WrongConversion("reftype: invalid construction ...")`) —
            // the missing nullability marker must be filled in as
            // `OptV(None)` here, the same way the two mismatches above fill
            // in structure spec prose leaves implicit.
            else if finalTag == "REF" && reshapedArgs.size == 1 then
              Expr.Case(finalTag, Expr.Opt(None) :: reshapedArgs)
            // `REF.NULL_ADDR` (the runtime null-ref *value*) takes no
            // argument at all — `s |- REF.NULL_ADDR : REF NULL BOT` always
            // types it as the bottom heap type regardless of context, so
            // the heaptype spec prose always writes alongside it (either as
            // a literal construction argument or an ignored `<var
            // ignore>` in a match) plays no role at the value level and is
            // dropped here, not just renamed — unlike every other `REF.*`
            // variant below, which keeps its one argument as-is.
            else if finalTag == "REF.NULL" then Expr.Case("REF.NULL_ADDR", Nil)
            else if refAddrSuffix.contains(finalTag) then
              Expr.Case(refAddrSuffix(finalTag), reshapedArgs)
            else Expr.Case(finalTag, reshapedArgs)
      case other => super.walk(other)

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = a.body.map(reshaper.walk)))
