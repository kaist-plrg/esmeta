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
    */
  override def requires: Set[LoweringPass] = Set(ResolveLinksPass)

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
    override def walk(expr: Expr): Expr = expr match
      case Expr.SpecTerm(RenamedTag(tag)) => Expr.Case(tag, Nil)
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
          case _ if tag == "[=external-type/global=]" =>
            Expr.Case(finalTag, List(Expr.Case("", reshapedArgs)))

          case _ => Expr.Case(finalTag, reshapedArgs)
      case other => super.walk(other)

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = a.body.map(reshaper.walk)))
