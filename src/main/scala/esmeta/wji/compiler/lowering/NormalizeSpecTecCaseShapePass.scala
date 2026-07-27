package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Expr}
import esmeta.wji.lang.util.Walker

/** Finalizes every `Expr.Case`'s `tag` into SpecTec's real runtime
  * `ALValue.CaseV` tag, and reshapes the handful whose runtime nesting is
  * deeper than spec prose writes it — wherever a `Case` shows up (a value under
  * construction, or a `Cond.IsOfForm`'s `form` pattern). Runs immediately after
  * [[ResolveLinksPass]], so every downstream pass (`ExpandIsOfFormPass`,
  * `ExpandDestructuringLetPass`, `Compiler`) can treat `Case.tag` as already
  * final and never needs to translate it itself.
  *
  * Only overrides [[Walker.walk(Expr)]] for the one node type ([[Expr.Case]])
  * it actually rewrites — every other `Expr`/`Cond`/`Instr` (including wherever
  * a `Case` shows up nested, e.g. inside a `Cond.IsOfForm`'s `form`) is reached
  * automatically by [[Walker]]'s own default structural recursion, with no
  * per-pass exhaustive case list to keep in sync here.
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
  * Category: SpecTec dependent.
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

  private object reshaper extends Walker:
    override def walk(expr: Expr): Expr = expr match
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
            else Expr.Case(finalTag, reshapedArgs)
      case other => super.walk(other)

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = a.body.map(reshaper.walk)))
