package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Instr}

/** Ensures every algorithm's top-level body ends with an explicit
  * `Instr.Return` — mirrors `esmeta.wji.compiler.Compiler.compileAlgo`'s own
  * "falls off the end implicitly returns `~unused~`" fallback, just made
  * explicit as a lowering step instead of a special case baked into the
  * compiler.
  *
  * Exists so `WrapCompletionReturnsPass` (which only wraps *existing*
  * `Instr.Return`/`Instr.Throw` nodes) sees a real exit path to wrap for an
  * algorithm that would otherwise just fall off the end — closing the one gap
  * that would otherwise keep `PropagateUnguardedCallsPass`'s runtime guard from
  * being safely simplified from a defensive 3-way check ("is this even a
  * completion?") down to a 2-way one ("is it abrupt or normal?"): once every
  * `returnsCompletion` algorithm's *every* exit path is provably wrapped, a
  * call site targeting one can assume the result always already has a `.Type`
  * field.
  *
  * Same "top-level only" check as `Compiler.compileAlgo`'s own (not recursive
  * into `IfChain` branches) — deliberately, to match that existing,
  * already-relied-on behavior exactly rather than second-guessing it here.
  * Applied to every algorithm, not just `returnsCompletion` ones — harmless
  * either way (this is exactly what `Compiler.compileAlgo`'s own fallback would
  * have done at compile time regardless), and simpler than threading a
  * condition through.
  *
  * Doesn't reach a `FollowingSteps` closure's own body (not yet split into its
  * own `Algorithm` at this pipeline stage — see `CompletionAlgorithms`);
  * `Compiler.compileAlgo`'s own fallback stays in place to cover that case (and
  * as a general safety net besides).
  *
  * Category: Completion-record convention.
  */
object InsertFallthroughReturnPass extends LoweringPass:

  /** Requires:
    *   - [[ExpandPerformReturnResultPass]]: needs every `Instr.Perform(...,
    *     ReturnResult, ...)` — the implicit-return shape
    *     `ExtractInlineAlgoCallPass` produces for a bare
    *     `Return(Some(AlgoCall(...)))` — already expanded back into a real
    *     `Instr.Return` first, so the "does this algorithm's top-level body
    *     already end in a `Return`" check below isn't fooled by one that hasn't
    *     been rewritten yet (which would otherwise get a spurious extra
    *     `Instr.Return(None)` appended after it).
    */
  override def requires: Set[LoweringPass] = Set(ExpandPerformReturnResultPass)

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map { a =>
      if a.body.exists(_.isInstanceOf[Instr.Return]) then a
      else a.copy(body = a.body :+ Instr.Return(None))
    }
