package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, AlgorithmKind, Expr, Instr, WjiParam}

/** Reshapes every `Getter`-kind [[Algorithm]] (e.g. `Instance.exports`,
  * `Table.length`) into the `<BUILTIN>:` calling convention mainline's own
  * `Call`/`BuiltinCallOrConstruct` machinery expects for an accessor property's
  * `[[Get]]` — the same two fix-ups [[AddBuiltinBehaviourPass]] applies to a
  * hoisted `CreateBuiltinFunction` closure, for the same reason (a
  * calling-convention requirement, not conditional on whether the algorithm
  * itself can abruptly complete):
  *
  *   - '''this-binding''': a getter's `**this**` normally only resolves via
  *     `Global("this")` being seeded by hand (see `WjiInterp`'s debug-only
  *     harness) — nothing else on a real property-access call path ever sets
  *     it. Fixed the same way any other `**this**`-using algorithm would set it
  *     explicitly: a leading `Set **this** to |this|.`, using the `|this|`
  *     parameter this pass also adds.
  *   - '''Completion-record wrapping''': every exit path must return a real
  *     Completion Record, same as [[AddBuiltinBehaviourPass]]'s own reason
  *     (mainline's Call machinery always expects one back, regardless of
  *     whether the algorithm itself can abruptly complete — see that pass's
  *     doc). Unlike that pass, this one *can* just reuse
  *     [[WrapCompletionReturnsPass]]'s work when it already ran — a getter that
  *     transitively calls something abrupt (`Table.length`, `Global.value`) is
  *     already `returnsCompletion = true` and has already been wrapped by it;
  *     only a getter with no abrupt-completion signal of its own (e.g.
  *     `Instance.exports`, just `return **this**.\[[Exports]]`) still needs
  *     [[CompletionWrapping.expandAlgorithm]] called directly here, or it
  *     wouldn't be wrapped by anything at all.
  *
  * `esmeta.wji.compiler.Compiler.compileAlgo` handles the remaining, genuinely
  * compiler-level half — registering the result under the exact name
  * `manuals/intrinsics` references (e.g.
  * `INTRINSICS.get:WebAssembly.Instance.prototype.exports`, case-preserved,
  * unlike every other algorithm's lowercased name) with `FuncKind.Builtin` —
  * since naming/`FuncKind` aren't things this metalang-level pipeline has any
  * other reason to know about.
  *
  * Category: Structural desugaring.
  */
object AddGetterBuiltinBehaviourPass extends LoweringPass:

  /** Requires:
    *   - [[ExpandFollowingStepsPass]]/[[ExtractInlineAlgoCallPass]]: same
    *     reason [[AddBuiltinBehaviourPass]] needs them before calling the same
    *     [[CompletionWrapping.expandAlgorithm]] utility — its own `Return`
    *     handling assumes a call already sitting in `Instr.Perform` form, not a
    *     raw inline `Expr.AlgoCall`.
    *   - [[WrapCompletionReturnsPass]]: needs its wrapping already applied to a
    *     `returnsCompletion = true` getter's body, so this pass can tell that
    *     case apart from one it still has to wrap itself (see class doc).
    */
  override def requires: Set[LoweringPass] = Set(
    ExpandFollowingStepsPass,
    ExtractInlineAlgoCallPass,
    WrapCompletionReturnsPass,
  )

  private val BuiltinParams =
    List(
      WjiParam("|this|"),
      WjiParam("|ArgumentsList|"),
      WjiParam("|NewTarget|"),
    )

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map { a =>
      a.kind match
        case AlgorithmKind.Getter(_) =>
          val wrappedBody =
            if a.returnsCompletion then a.body
            else CompletionWrapping.expandAlgorithm(a.body)
          a.copy(
            params = BuiltinParams,
            body = List(Instr.Set(Expr.This, Expr.Var("this"), wrappedBody)),
          )
        case _ => a
    }
