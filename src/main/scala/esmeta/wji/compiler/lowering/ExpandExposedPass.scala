package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr}
import esmeta.wji.lang.walker.Walker

/** Converts `Cond.Exposed(subject, realm, negated)` — "SUBJECT is [not]
  * [=exposed=] in REALM" — into a real call to `exposed` (`webidl/index.bs`'s
  * `<dfn id="dfn-exposed">`, extracted via `SpecFile.webidlFilter` with its
  * body hardcoded to `Return true.` by `SpecPatch` #38 — see
  * `docs/hardcodes.md` #13), compared against the literal `true`:
  * {{{
  *   Exposed(op, realm, negated = true)
  * }}}
  * becomes
  * {{{
  *   Eq(AlgoCall("[=exposed=]", [op, realm]), Bool(true), negated = true)
  * }}}
  *
  * This pass only rewrites the `Cond` shape — it doesn't hoist anything itself.
  * `Compiler.compileCond` is a pure `Cond => ir.Expr` mapping with no way to
  * emit instructions of its own, so a genuine call embedded in a condition
  * still needs reducing to an already-computed boolean before `Compiler` ever
  * sees it, exactly like [[ExpandMatchesExistsPass]]'s
  * `Cond.Matches`/`Cond.Any` — but rather than duplicating that hoist logic
  * here too, this just builds an ordinary nonempty-arg `AlgoCall` inside an
  * `Eq`, the exact shape [[NormalizeEvaluationOrderPass]]'s `Cond.Eq` case
  * already knows how to extract into a preceding `Let`, which
  * [[ExpandInlineAlgoCallPass]] then turns into a real `Perform`. Both are
  * general-purpose passes with no `exposed`-specific knowledge at all.
  *
  * Category: Spec-dependent — WJI.
  */
object ExpandExposedPass extends LoweringPass:

  /** Must precede:
    *   - [[NormalizeEvaluationOrderPass]]: needs the `AlgoCall` this pass
    *     builds already in place — that pass is what actually hoists it out of
    *     the `Eq` into a `Let`.
    */
  override def mustPrecede: Set[LoweringPass] = Set(
    NormalizeEvaluationOrderPass,
  )

  private object rewriter extends Walker:
    override def walk(cond: Cond): Cond = cond match
      case Cond.Exposed(subject, realm, negated) =>
        Cond.Eq(
          Expr.AlgoCall("[=exposed=]", List(walk(subject), walk(realm))),
          Expr.Bool(true),
          negated,
        )
      case other => super.walk(other)

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = a.body.map(rewriter.walk)))
