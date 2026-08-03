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
  * algorithm that would otherwise just fall off the end — without this, a
  * `returnsCompletion` algorithm reachable only via implicit fall-through would
  * have one exit path left un-wrapped, breaking the guarantee every caller
  * relies on: that a `completionAlgos` member's result always already has a
  * `.Type` field, with no need to check for that defensively first.
  *
  * Uses `Instr.alwaysExits` (same helper `Compiler.compileAlgo` uses) to decide
  * whether a fallback is needed — this recognizes an `IfChain` whose every
  * branch (including a real `else`) already returns/throws as already
  * exhaustive, so it doesn't append a redundant, unreachable `Return` after
  * one. Still doesn't look inside loop bodies (`ForEach`/`For`/`While`) or a
  * `FollowingSteps` closure's own body (not yet split into its own `Algorithm`
  * at this pipeline stage — see `CompletionAlgorithms`);
  * `Compiler.compileAlgo`'s own fallback stays in place to cover the closure
  * case (and as a general safety net besides). Applied to every algorithm, not
  * just `returnsCompletion` ones — harmless either way (this is exactly what
  * `Compiler.compileAlgo`'s own fallback would have done at compile time
  * regardless), and simpler than threading a condition through.
  *
  * Category: Completion-record convention.
  */
object InsertFallthroughReturnPass extends LoweringPass:

  /** Requires:
    *   - [[ExpandPerformReturnResultPass]]: needs every `Instr.Perform(...,
    *     ReturnResult, ...)` — the implicit-return shape
    *     `ExpandInlineAlgoCallPass` produces for a bare
    *     `Return(Some(AlgoCall(...)))` — already expanded back into a real
    *     `Instr.Return` first, so the "does this algorithm's top-level body
    *     already end in a `Return`" check below isn't fooled by one that hasn't
    *     been rewritten yet (which would otherwise get a spurious extra
    *     `Instr.Return(None)` appended after it).
    */
  override def requires: Set[LoweringPass] = Set(ExpandPerformReturnResultPass)

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map { a =>
      if Instr.alwaysExits(a.body) then a
      else a.copy(body = a.body :+ Instr.Return(None))
    }
