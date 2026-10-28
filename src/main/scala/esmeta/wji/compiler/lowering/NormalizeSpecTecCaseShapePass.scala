package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}
import esmeta.wji.lang.walker.Walker

/** Finalizes every `Expr.Case`'s `tag` into SpecTec's real runtime
  * `ALValue.CaseV` tag, reshapes the handful whose runtime nesting is deeper
  * than spec prose writes it, converts every `Expr.SpecTerm` that's actually a
  * Wasm-boundary nullary/shorthand case in disguise (`error`, a `valtype`
  * literal like `i32`, a `reftype` shorthand like `funcref`) into the real
  * `Expr.Case`/`Expr.Opt` it denotes, and promotes every surviving
  * `Expr.TypeAnnotated(term, Seq_(...))` (see that node's own doc —
  * [[ResolveTypeAnnotationPass]] already dropped every one whose EXPR was
  * self-describing, so what's left here always wraps an untagged `Seq_`) into a
  * real tagged `Expr.Case` the same way — wherever any of these show up (a
  * value under construction, or a `Cond.IsOfForm`'s `form` pattern). Runs
  * immediately after [[ResolveLinksPass]], so every downstream pass
  * (`ExpandIsOfFormPass`, `ExpandDestructuringLetPass`, `Compiler`) can treat
  * `Expr.Case` as the sole representation of a Wasm-boundary constructor value
  * — a `SpecTerm`/`TypeAnnotated` reaching them is *never* secretly one of
  * these, only a genuine spec-term reference (`null`, `current Realm`, ...).
  *
  * `TypeAnnotated` promotion (TERM -> tag, `construct.ml`'s
  * `al_of_memorytype`/`al_of_tabletype`/`al_of_limits`): `"memory type"` ->
  * `CaseV("PAGE", [addrtype, limits])`, `"table type"` -> `CaseV("", [addrtype,
  * limits, reftype])` — a genuinely untagged case, confirming this session's
  * very first read of it. Both build the shared nested `limits` range the same
  * way, `CaseV("[", [min, max])`; every `[X .. Y]`-range `ExprParser`'s
  * `BracketRange` produces shows up as a `Seq_` nested directly inside another
  * `Seq_` (there's no other producer of that shape in this codebase today), so
  * any such nesting found while promoting a `TypeAnnotated` is assumed to be
  * exactly this `limits` pair — structural, not term-specific, unlike the TERM
  * -> tag table itself. Only the two terms this session's `Memory`/`Table`
  * constructor work actually needs; extend `typeAnnotationTag` as more come up
  * (see `docs/hardcodes.md`).
  *
  * `limits`' `max` field is SpecTec's `OptV` (`empty` when unbounded) — unlike
  * `min`, it's never passed here as a bare positional arg. Its `Opt`-wrapping
  * happens at its own binding site instead (`Instr.Let(Var("maximum"), _)`, see
  * that case's own doc), the same place and the same way `mut`'s is
  * (`docs/spec_errors.md` #39/#40) — by the time either reaches *use*, both are
  * already a proper, static `Opt(Some(...))`/`Opt(None)`, so this pass never
  * needs a runtime "is this empty?" check at the use site at all.
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
  * Nesting mismatches
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
  *   - `[=mut=]` (js-api/index.bs's own `globaltype` construction, post
  *     `SpecPatch` #76/`docs/spec_errors.md` #39 — upstream still writes the
  *     now-nonexistent `[=var=]`/`[=const=]`): `al_of_globaltype` is `CaseV("",
  *     [mut; vt])` — an untagged pair, `mut` itself `al_of_mut`'s `OptV
  *     None`/`OptV (Some (CaseV ("MUT", [])))` (an *optional* flag, not a
  *     two-variant tag) — but prose writes `mut`'s own link juxtaposed with
  *     `|valuetype|`, which `ResolveLinksPass` reads as a one-arg call to that
  *     link instead of two untagged siblings. The immutable case writes a bare
  *     `|valuetype|` instead (no symmetric tag — `mut`'s absence needs none),
  *     homogenized to the same pair shape by `wrapGlobaltype`.
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
  * and
  * {{{
  *   Case("[=ref.host=]", [Var(hostaddr)])
  * }}}
  * becomes
  * {{{
  *   Case("REF.HOST_ADDR", [Var(hostaddr)])
  * }}}
  * and
  * {{{
  *   Case("[=mut=]", [Var(valuetype)])
  * }}}
  * becomes
  * {{{
  *   Case("", [Opt(Some(Case("MUT", []))), Var(valuetype)])
  * }}}
  *
  * Every other `Case` (an already-correctly-flat variant like
  * `external-type/func`, or one [[ExprParser]] built directly rather than via a
  * link, like `CompTypeArrow`'s `"->"` or `parseUntaggedForm`'s `""`) has its
  * tag translated (a no-op if already final) and its args recursed into, with
  * no extra nesting added. Once this runs, every downstream pass
  * (`ExpandIsOfFormPass`, `ExpandDestructuringLetPass`, `Compiler`) can treat
  * any `Expr.Case` it sees as already shaped exactly like the real
  * `ALValue.CaseV` it corresponds to, with zero SpecTec knowledge of its own.
  *
  * Category: Spec-dependent — SpecTec.
  */
object NormalizeSpecTecCaseShapePass extends LoweringPass:

  /** Requires:
    *   - [[ResolveLinksPass]]: needs every `Expr.Link` already resolved to
    *     `Expr.Case`/`Expr.AlgoCall`/`Expr.SpecTerm` — nothing here resolves a
    *     raw `Link` itself, just reshapes an already-built `Case` or
    *     `SpecTerm`.
    *   - [[ResolveTypeAnnotationPass]]: needs its "drop or keep" decision on
    *     every `Expr.TypeAnnotated` already made — only ever sees one this pass
    *     chose to keep (wrapping a `Seq_`), never one still wrapping a
    *     self-describing EXPR.
    */
  override def requires: Set[LoweringPass] =
    Set(ResolveLinksPass, ResolveTypeAnnotationPass)

  /** TERM -> real SpecTec tag for a promoted `Expr.TypeAnnotated`. Falls back
    * to TERM itself (via `getOrElse` at the call site) for anything not listed
    * — safe since neither promotion the fallback would misfire on
    * (memtype/tabletype) has any other current producer; a wrong tag would just
    * be as "not yet mechanized" downstream as `Seq_` already was.
    */
  private val typeAnnotationTag: Map[String, String] = Map(
    "memory type" -> "PAGE",
    "table type" -> "",
  )

  /** Translates a `Case.tag` out of spec-link-text form into SpecTec's real
    * runtime `ALValue.CaseV` tag. Idempotent on an already-final tag (stripped,
    * no `/`, already uppercase), so it's safe to apply unconditionally to every
    * `Case` this pass sees, regardless of where its tag originally came from.
    */
  private def runtimeCaseTag(tag: String): String =
    tag
      .stripPrefix("[=")
      .stripSuffix("=]")
      .trim
      .split('/')
      .last
      .trim
      .toUpperCase

  /** Pure tag rename — output arity always matches input arity (`Nil` stays
    * `Nil`). Kept separate from [[ShorthandReftype]]/[[ShorthandConst]] below:
    * those two don't just rename a tag, they also restructure args (wrap into a
    * compound `Case`, prepend a nested tag), so folding them in here would
    * conflate "what to call it" with "what shape to build".
    */
  private object RenamedTag:
    private val heapTypePrefix = "heap-type/"
    private val table: PartialFunction[String, String] =
      case "I32"        => "I32"
      case "I64"        => "I64"
      case "F32"        => "F32"
      case "F64"        => "F64"
      case "V128"       => "V128"
      case "ERROR"      => "ERROR"
      case "REF.NULL"   => "REF.NULL_ADDR"
      case "REF.I31"    => "REF.I31_NUM"
      case "REF.STRUCT" => "REF.STRUCT_ADDR"
      case "REF.ARRAY"  => "REF.ARRAY_ADDR"
      case "REF.FUNC"   => "REF.FUNC_ADDR"
      case "REF.EXN"    => "REF.EXN_ADDR"
      case "REF.HOST"   => "REF.HOST_ADDR"
      case "THROW_REF"  => "THROW_REF"
      case tag if tag.startsWith(heapTypePrefix.toUpperCase) =>
        tag.substring(heapTypePrefix.length)

    def unapply(s: String): Option[String] = table.lift(s.toUpperCase)

  /** Extract heaptype from Wasm's nullable-`reftype` shorthand names */
  private object ShorthandReftype:
    private val heaptypes: Map[String, String] = Map(
      "FUNCREF" -> "FUNC",
      "EXTERNREF" -> "EXTERN",
      "EXNREF" -> "EXN",
    )
    def unapply(s: String): Option[String] = heaptypes.get(s.toUpperCase)

  /** Extract numtype from raw const tags */
  private object ShorthandConst:
    private val numtypes: Map[String, String] = Map(
      "I32.CONST" -> "I32",
      "I64.CONST" -> "I64",
      "F32.CONST" -> "F32",
      "F64.CONST" -> "F64",
    )
    def unapply(tag: String): Option[String] = numtypes.get(tag)

  /** Swaps every nullary/shorthand `SpecTerm` that's secretly a Wasm-boundary
    * `Case` in disguise, then finalizes every genuine `Case`'s tag (and, for
    * the handful with mismatched nesting, its shape)
    */
  private object reshaper extends Walker:
    // "|f32| is [=nan=]": any NaN, whatever its sign or payload -- a check of
    // the magnitude's tag rather than an equality
    override def walk(cond: Cond): Cond = cond match
      case Cond.Eq(x, Expr.SpecTerm("nan"), negated) =>
        Cond.Eq(
          Expr.CaseTag(Expr.Index(walk(x), Expr.Num("0"))),
          Expr.Str("NAN"),
          negated,
        )
      case other => super.walk(other)

    override def walk(expr: Expr): Expr = expr match
      case Expr.TypeAnnotated(term, Expr.Seq_(args)) =>
        // `memtype`'s own grammar (`addrtype limits PAGE`) ends in a fixed
        // "page" keyword that disambiguates nothing (unlike comptype's
        // FUNC/STRUCT/ARRAY, memtype has no competing variant to tell
        // apart) and carries no data of its own -- the "memory type" TERM
        // above already conveys the same information (`typeAnnotationTag`
        // maps it straight to the real runtime tag `"PAGE"`), so it's
        // dropped here rather than kept as a third positional arg, which
        // `al_of_memorytype`'s real 2-arg shape (`CaseV("PAGE", [addrtype;
        // limits])`) has no room for. `ExprParser.PageKeyword` is what
        // parses the literal word into this `SpecTerm("page")` in the
        // first place -- purely structural there, with no SpecTec
        // knowledge of its own.
        val realArgs =
          if term == "memory type" then
            args.filterNot(_ == Expr.SpecTerm("page"))
          else args
        Expr.Case(
          typeAnnotationTag.getOrElse(term, term),
          realArgs.map(promoteLimits),
        )
      case Expr.SpecTerm(RenamedTag(tag)) => Expr.Case(tag, Nil)
      // The bare word "empty" (Infra-spec's own "no value here") always
      // denotes an absent `Opt` -- `al_to_mut`'s `OptV None` (`mut` being
      // "empty", `docs/spec_errors.md` #39) and `al_to_limits`'s `max` field
      // (`limits` being unbounded, `docs/spec_errors.md` #40) are both this
      // same optionality, just on different fields. The *present* side of
      // each (`[=mut=] |valuetype|` above; `|maximum|`'s own binding below)
      // is wrapped in `Opt(Some(...))` at its own construction site, so by
      // the time either field is actually used (compared against `empty`, or
      // assembled into a `globaltype`/`limits` pair), it's already a proper
      // `Opt` either way -- nothing downstream needs to know "empty" was
      // ever a bare word at all.
      case Expr.SpecTerm("empty") => Expr.Opt(None)
      // `[=+∞=]`/`[=−∞=]`: `fN`'s `POS`/`NEG` around the `INF` magnitude
      case Expr.SpecTerm("+∞") => Expr.Case("POS", List(Expr.Case("INF", Nil)))
      case Expr.SpecTerm("−∞") => Expr.Case("NEG", List(Expr.Case("INF", Nil)))
      case Expr.SpecTerm(ShorthandReftype(heaptype)) =>
        Expr.Case(
          "REF",
          List(
            Expr.Opt(Some(Expr.Case("NULL", Nil))),
            Expr.Case(heaptype, Nil),
          ),
        )

      case Expr.Case(tag, args) =>
        val finalTag = runtimeCaseTag(tag)
        val reshapedArgs = args.map(walk)

        finalTag match
          case RenamedTag(renamedTag) =>
            Expr.Case(renamedTag, reshapedArgs)

          // [=i32.const=] u32 -> CONST(I32, u32)
          case ShorthandConst(numTag) =>
            Expr.Case("CONST", Expr.Case(numTag, Nil) :: reshapedArgs)

          // [=ref=] heaptype -> REF(Opt(None), heaptype)
          case "REF" if reshapedArgs.size == 1 =>
            Expr.Case(finalTag, Expr.Opt(None) :: reshapedArgs)

          // [=external-type/global=] mut vt -> GLOBAL((mut, vt))
          case _
              if tag == "[=external-type/global=]" && reshapedArgs.size == 2 =>
            Expr.Case(finalTag, List(Expr.Case("", reshapedArgs)))

          // [=external-type/global=] vt -> GLOBAL((Opt(None), vt)) -- the
          // immutable-only case (`stringExternType`, `docs/spec_inconsistencies.md`
          // #21): only a single positional arg reaches here (the greedy
          // one-arg-call parse swallows `[=ref=] [=heap-type/extern=]` as one
          // unit before `external-type/global` ever sees it), so `mut` is
          // never written at all -- same "absent optional needs no token"
          // convention as `Global`'s own bare-|valuetype| immutable branch
          // above, reusing the exact same `wrapGlobaltype` helper to supply
          // the implicit `Opt(None)`. Guarded to exclude a bare `Var` arg
          // specifically: this same tag/arity also appears as a `form`
          // pattern (`Cond.IsOfForm`, "is of the form [=external-type/global=]
          // <var ignore>globaltype</var>", index.bs:719) where a single `Var`
          // means "bind the whole inner value, whatever shape it is" --
          // wrapping that would corrupt the destructure instead of fixing a
          // construction.
          case _
              if tag == "[=external-type/global=]" && reshapedArgs.size == 1
              && !reshapedArgs.head.isInstanceOf[Expr.Var] =>
            Expr.Case(finalTag, List(wrapGlobaltype(reshapedArgs.head)))

          // [=mut=] vt -> (Opt(MUT), vt) -- index.bs's `Global` constructor
          // (post-`SpecPatch` #76/`docs/spec_errors.md` #39) builds a
          // `globaltype` by writing `mut`'s own link (same idiom as `[=i32=]`
          // for `addrtype`) juxtaposed with a separate `|valuetype|`, which
          // `ResolveLinksPass.buildCaseOrCall` reads as a one-arg call to that
          // link instead of two untagged sibling components -- the same
          // nesting mismatch as `external-type/global` above. The immutable
          // case needs no symmetric tag here at all (`construct.ml`'s
          // `al_of_mut` is `Cons -> OptV None` / `Var -> OptV (Some (CaseV
          // ("MUT", [])))` -- immutable is the plain *absence* of this flag,
          // not its own variant) -- it's written as a bare, unwrapped
          // |valuetype| instead, homogenized to the same `(Opt(_), vt)` pair
          // shape by `wrapGlobaltype` below (see its own doc for where).
          case _ if tag == "[=mut=]" =>
            Expr.Case(
              "",
              Expr.Opt(Some(Expr.Case("MUT", Nil))) :: reshapedArgs,
            )

          // [=comp-type/func=] params results -> "->"(params, results) — a
          // functype-shaped comptype's runtime tag is `"->"` itself
          // (`construct.ml`'s `al_of_comptype`: `FuncT (rt1, rt2) -> CaseV
          // ("->", [rt1; rt2])`); `.spectec`'s `FUNC resulttype -> resulttype`
          // grammar keyword (`docs/spec_errors.md` #18) only exists at the
          // surface-syntax level, to disambiguate `comptype`'s three variants.
          // Checked against the *raw* `tag`, not `finalTag`, and kept out of
          // `RenamedTag` above deliberately: `runtimeCaseTag`'s generic
          // `for:`-scope stripping reduces `[=external-type/func=]` (a
          // *different* Sort, whose own real runtime tag genuinely is `"FUNC"`
          // — see e.g. `read the imports`'s `externtype` matching, unchanged)
          // down to the exact same bare `"FUNC"` as this one — a shared
          // string-keyed table can't tell the two apart once stripped, only
          // the still-scoped raw link text can.
          case _ if tag == "[=comp-type/func=]" =>
            Expr.Case("->", reshapedArgs)

          case _ => Expr.Case(finalTag, reshapedArgs)

      // `read the imports`' global-import branch (post-`SpecPatch`
      // #76/`docs/spec_errors.md` #39) passes a bare, unwrapped |valtype| as
      // `global_alloc`'s globaltype argument for the always-immutable case —
      // see `wrapGlobaltype`'s own doc for why a bare valtype needs wrapping
      // here at all. Matched on the argument being literally `|valtype|` by
      // name, not `global_alloc` calls generically: `Global`'s own
      // constructor also calls `global_alloc`, forwarding its own
      // already-correctly-paired `|globaltype|` variable as the same
      // positional argument — wrapping unconditionally there would nest an
      // already-proper pair one level too deep.
      case Expr.AlgoCall(tag, args)
          if tag == "[=global_alloc=]" && args.size == 3 && args(
            1,
          ) == Expr.Var("valtype") =>
        val walkedArgs = args.map(walk)
        Expr.AlgoCall(
          tag,
          List(walkedArgs(0), wrapGlobaltype(walkedArgs(1)), walkedArgs(2)),
        )

      case other => super.walk(other)

    // `Global`'s constructor (post-`SpecPatch` #76/`docs/spec_errors.md` #39)
    // writes "If |mutable| is true, let |globaltype| be [=mut=] |valuetype|;
    // otherwise, let |globaltype| be |valuetype|." -- this compiles to an
    // `Instr.If`/`Instr.Else` with *two separate* `Let(Var("globaltype"), _)`
    // nodes (one per branch), not a single ternary `Expr.Conditional`, so the
    // asymmetry has to be fixed up at the `Instr.Let` level instead: the
    // mutable branch's RHS is already the proper `(Opt(Some(MUT)), vt)` pair
    // by the time this runs (reshaped above), but the immutable branch's is a
    // bare, unwrapped |valuetype| -- faithful to the real grammar
    // (`globaltype ::= mut? valtype`, where an absent `mut` is simply never
    // written), but `globaltype`'s own runtime shape is *always* the same
    // 2-element pair regardless, so it needs wrapping to match its sibling.
    // Matched on the RHS being literally `Var("valtype")`, not just the
    // bound variable's name: `GetGlobalValue` *also* binds a variable named
    // `globaltype` (`Let |globaltype| be [=global_type=](...)`, reading an
    // already-correctly-shaped pair straight back from the embedding RPC) --
    // wrapping that one too would nest an already-proper pair one level too
    // deep, the same mistake `wrapGlobaltype`'s own `global_alloc` call site
    // above had to guard against.
    override def walk(instr: Instr): Instr = instr match
      case i: Instr.Let
          if i.lhs == Expr.Var("globaltype") && i.expr == Expr.Var(
            "valuetype",
          ) =>
        Instr.Let(i.lhs, wrapGlobaltype(walk(i.expr)), i.body.map(walk))
      // `Memory`/`Table`'s constructors (`docs/spec_errors.md` #40) write
      // "If |descriptor|[\"maximum\"] exists, let |maximum| be
      // [=?=][=AddressValueToU64=](...); otherwise, let |maximum| be empty."
      // -- the `empty` branch already becomes `Opt(None)` via the generic
      // rule above, so only the *present* branch needs wrapping here, to
      // `Opt(Some(...))`, so that by the time `|maximum|` is actually used
      // (assembled into a `limits` pair, `promoteLimits` below) it's already
      // a proper `Opt` either way -- no runtime "is this empty?" check
      // needed at the use site at all.
      case i: Instr.Let
          if i.lhs == Expr.Var("maximum") && i.expr != Expr.SpecTerm(
            "empty",
          ) =>
        Instr.Let(i.lhs, Expr.Opt(Some(walk(i.expr))), i.body.map(walk))
      case other => super.walk(other)

    /** `globaltype`'s runtime shape (`construct.ml`'s `al_of_globaltype`) is
      * always the untagged pair `CaseV("", [mut; vt])`, `mut` itself always an
      * `OptV` (see the `[=mut=]` case above) -- no other Wasm-boundary value
      * construction produces that exact (untagged, `Opt` first element) shape,
      * so it's an unambiguous signature to test for.
      */
    private def isGlobaltypePair(e: Expr): Boolean = e match
      case Expr.Case("", List(Expr.Opt(_), _)) => true
      case _                                   => false

    private def wrapGlobaltype(e: Expr): Expr =
      if isGlobaltypePair(e) then e else Expr.Case("", List(Expr.Opt(None), e))

    /** A `TypeAnnotated`'s outer `Seq_`'s own element: a nested `Seq_` of
      * exactly two parts is a `[min .. max]` range, promoted to `limits`'
      * runtime shape; anything else just recurses normally. `max` needs no
      * `Opt`-wrapping here — it's already a proper `Opt` by the time it reaches
      * here, from its own binding site (the `Instr.Let` case above, or the
      * generic `empty` rule).
      */
    private def promoteLimits(e: Expr): Expr = e match
      case Expr.Seq_(List(min, max)) =>
        Expr.Case("[", List(walk(min), walk(max)))
      case other => walk(other)

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = a.body.map(reshaper.walk)))
