package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}

/** Expands `Expr.Abrupt(marker, inner)` — the spec's `?` (ReturnIfAbrupt) and
  * `!` (assert-not-abrupt) operators — into explicit completion-record
  * inspection, mirroring `esmeta.compiler.Compiler.returnIfAbrupt`'s `check`
  * boolean (`true` for `?`, `false` for `!`): both read the wrapped value's
  * `.Value` once its completion-ness is settled — `?` returns the completion
  * outright when it's abrupt, `!` instead asserts it's `~normal~` (mirroring
  * mainline's own `IAssert(ETypeCheck(xExpr, IRType(NormalT)))` — `!` is not
  * simply a no-op there either).
  *
  * Both markers presuppose `inner` genuinely *is* a Completion Record —
  * ECMA-262's own "ReturnIfAbrupt Shorthands" bakes in the exact same
  * assumption, via an unconditional `Assert: ... is a Completion Record` with
  * no graceful fallback for a non-completion operand. `inner` failing that here
  * is never something this pass should defend against silently; it means either
  * `inner`'s target isn't actually completion-returning (a spec bug — `?`/`!`
  * should never have been written there at all, see docs/spec_errors.md
  * #14/#16, fixed via `SpecPatch` rather than papered over here) or, for a
  * WJI-compiled target, that `WrapCompletionReturnsPass` hasn't actually
  * wrapped every one of its exit paths yet:
  * {{{
  *   Let(x, Abrupt("?", inner), body)
  * }}}
  * becomes:
  * {{{
  *   IfChain(
  *     [(IsType(inner, "AbruptCompletion"), [Return(inner)])],
  *     fallback = [Let(x, inner.Value, body)],
  *   )
  * }}}
  * and `Abrupt("!", inner)` the same way, except the branch is a flat
  * `Assert(inner.Type is ~normal~)` followed by the `.Value` bind, rather than
  * a further `IfChain`.
  *
  * Only ever needs to recognize *one* shape: `inner` is always already a bare
  * `Var` by the time this pass runs, and `Abrupt(marker, inner)` is always
  * already directly the RHS of a `Let`/`Set`/`Return` — both guaranteed by
  * [[NormalizeEvaluationOrderPass]]'s own "Evaluation Order" normalization
  * (ECMA-262 5.2.4.3), which hoists every other occurrence (a nested argument,
  * an un-atomic wrapped value, ...) into exactly this canonical shape first. No
  * scanning of `Append`/`Perform`/`Assert`/`IfChain`/etc. is needed here at all
  * — anything reaching one of those `Instr` shapes as an `Expr.Abrupt` is a bug
  * in that earlier normalization pass, not something this one is expected to
  * work around.
  *
  * Category: Completion-record convention.
  */
object ExpandAbruptPass extends LoweringPass:

  /** Requires:
    *   - [[NormalizeEvaluationOrderPass]]: guarantees every remaining
    *     `Expr.Abrupt` is already directly the RHS of a `Let`/`Set`/`Return`,
    *     with its own `inner` already a bare `Var` — see class doc.
    *
    * Must precede:
    *   - [[ExtractInlineAlgoCallPass]]: needs `Let(_tupleN, AlgoCall(...))`
    *     nodes this pass produces already in place — see its own doc.
    */
  override def requires: Set[LoweringPass] = Set(NormalizeEvaluationOrderPass)
  override def mustPrecede: Set[LoweringPass] = Set(ExtractInlineAlgoCallPass)

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = transform(a.body)))

  private def transform(instrs: List[Instr]): List[Instr] =
    instrs.flatMap(expandInstr)

  private def expandInstr(instr: Instr): List[Instr] = instr match

    case Instr.Let(lhs, Expr.Abrupt(marker, inner), body) =>
      expand(marker, inner, Instr.Let(lhs, _, transform(body)))

    case Instr.Set(lhs, Expr.Abrupt(marker, inner), body) =>
      expand(marker, inner, Instr.Set(lhs, _, transform(body)))

    case Instr.Return(Some(Expr.Abrupt(marker, inner)), body) =>
      expand(marker, inner, x => Instr.Return(Some(x), transform(body)))

    case _ =>
      List(instr.mapBody(transform))

  /** Shared `?`/`!` expansion. `bind` embeds the final unwrapped value into
    * whichever instruction shape (`Let`/`Set`/`Return`) is using it. `inner` is
    * always already a bare `Var` (see class doc), so — unlike before
    * `NormalizeEvaluationOrderPass` took over hoisting it — this never needs to
    * bind it under a fresh alias first.
    */
  private def expand(
    marker: String,
    inner: Expr,
    bind: Expr => Instr,
  ): List[Instr] =
    val typeField = Expr.Field(inner, "Type")
    val valueField = Expr.Field(inner, "Value")

    marker match
      case "?" =>
        List(
          Instr.IfChain(
            branches = List(
              (
                Cond.IsType(inner, "AbruptCompletion"),
                List(Instr.Return(Some(inner))),
              ),
            ),
            fallback = List(bind(valueField)),
          ),
        )
      case _ => // "!" — assert it's Normal (never abrupt), then unwrap
        List(
          Instr.Assert(Cond.Eq(typeField, Expr.SpecTerm("normal"))),
          bind(valueField),
        )
