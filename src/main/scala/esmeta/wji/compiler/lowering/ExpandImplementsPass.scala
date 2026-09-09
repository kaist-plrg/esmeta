package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr}
import esmeta.wji.lang.walker.Walker

/** Converts `Cond.Implements(value, iface, negated)` — "VALUE [=implements=]
  * IFACE" / "VALUE does not [=implement=] IFACE" — into webidl/index.bs's own
  * definition (`index.bs:13799-13802`): a JS value implements an interface if
  * it [=is a platform object=] (an Object with a `[[PrimaryInterface]]`
  * internal slot) and IFACE is among the [=inclusive inherited interfaces=]
  * (`index.bs:707-718`) of that slot's value:
  * {{{
  *   Implements(value, iface, negated = false)
  * }}}
  * becomes
  * {{{
  *   And(
  *     And(IsType(value, "Object"), HasSlot(value, "PrimaryInterface")),
  *     Contains(iface, AlgoCall("[=inclusive inherited interfaces=]",
  *                               [Field(value, "PrimaryInterface")])),
  *   )
  * }}}
  * and the negated form via De Morgan (`And`/`Or` carry no `negated` of their
  * own — every leaf `Cond` here already does):
  * {{{
  *   Or(
  *     Or(IsType(value, "Object", negated = true),
  *        HasSlot(value, "PrimaryInterface", negated = true)),
  *     Contains(iface, AlgoCall(...), negated = true),
  *   )
  * }}}
  * `And`/`Or` compile to short-circuiting `EBinary(BOp.And/Or, ...)`
  * (`Interpreter.shortCircuit`), which is why this shape is safe: the
  * `[[PrimaryInterface]]` read is never reached unless the slot's existence
  * has already been confirmed.
  *
  * `iface` itself is threaded through unchanged — a literal `{{X}}` parses to
  * `Expr.SpecTerm(X)` (`CondParser`), and *that* is resolved to the
  * `HOST_DEFINED.<X>` runtime record only later, by `Compiler.compileExpr`'s
  * generic `SpecTerm` case (the same one every other `{{X}}` — `new`'s own
  * argument included — resolves through). This pass has no interface-name
  * knowledge of its own; it's purely the structural `Cond.Implements =>
  * And/Or/Contains` rewrite.
  *
  * Mirrors `ExpandExposedPass`'s own shape and reasoning exactly — a bespoke
  * `Cond` node rewritten directly into an `AlgoCall` wrapped in ordinary
  * `Cond` nodes, since `Compiler.compileCond` is a pure `Cond => ir.Expr`
  * mapping with no way to emit instructions of its own: the `AlgoCall` this
  * pass builds still needs [[NormalizeEvaluationOrderPass]] to hoist it out
  * into a preceding `Let` (via the same general `Cond.Contains`-inside-a-
  * `Cond` handling every other call-bearing `Cond` already goes through), and
  * [[ExpandInlineAlgoCallPass]] to turn that into a real `Perform`.
  *
  * Category: Spec-dependent — WJI.
  */
object ExpandImplementsPass extends LoweringPass:

  /** Must precede:
    *   - [[NormalizeEvaluationOrderPass]]: needs the `AlgoCall` this pass
    *     builds already in place — that pass is what actually hoists it out
    *     of the `Contains` into a `Let`.
    */
  override def mustPrecede: Set[LoweringPass] = Set(
    NormalizeEvaluationOrderPass,
  )

  private object rewriter extends Walker:
    override def walk(cond: Cond): Cond = cond match
      case Cond.Implements(value, iface, negated) =>
        val v = walk(value)
        val i = walk(iface)
        val ancestry = Expr.AlgoCall(
          "[=inclusive inherited interfaces=]",
          List(Expr.Field(v, "PrimaryInterface")),
        )
        if !negated then
          Cond.And(
            Cond.And(
              Cond.IsType(v, "Object"),
              Cond.HasSlot(v, "PrimaryInterface"),
            ),
            Cond.Contains(i, ancestry),
          )
        else
          Cond.Or(
            Cond.Or(
              Cond.IsType(v, "Object", negated = true),
              Cond.HasSlot(v, "PrimaryInterface", negated = true),
            ),
            Cond.Contains(i, ancestry, negated = true),
          )
      case other => super.walk(other)

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = a.body.map(rewriter.walk)))
