package esmeta.wji.compiler.lowering

import esmeta.wji.lang.Algorithm

/** For every algorithm with `returnsCompletion = true` — a definite or
  * transitively-inferred completion-returning operation (see
  * [[CompletionAlgorithms]]/[[MarkCompletionAlgorithmsPass]]) — wraps every
  * `Return`/`Throw` exit path in an ECMA-262 Completion Record via
  * [[CompletionWrapping.expand]] (see that object's own doc for the actual
  * rewrite, the ECMA-262 rationale, and why the wrapping logic itself lives
  * there rather than in this pass).
  *
  * Per ECMA-262 convention, an operation that can abruptly complete must
  * consistently return a Completion Record on *every* exit path, not just the
  * throwing ones — a caller has no way to tell "raw value" and "normal
  * completion" apart otherwise.
  *
  * Category: Completion-record convention.
  */
object WrapCompletionReturnsPass extends LoweringPass:

  /** Requires:
    *   - [[ExpandAbruptPass]]: needs a `Throw`/`Return`'s own `body`/`expr`
    *     already in their final shape (no leftover `?`/`!` markers to expand).
    *   - [[ExpandDestructuringLetPass]]: same — needs destructuring `Let`s
    *     already expanded before it inspects `body`/`expr`.
    *   - [[InsertFallthroughReturnPass]]: needs a real `Instr.Return` to wrap
    *     even for an algorithm that would otherwise just fall off the end.
    *   - [[MarkCompletionAlgorithmsPass]]: needs `returnsCompletion` already
    *     stamped onto every `Algorithm` to know which ones to target.
    */
  override def requires: Set[LoweringPass] = Set(
    ExpandAbruptPass,
    ExpandDestructuringLetPass,
    InsertFallthroughReturnPass,
    MarkCompletionAlgorithmsPass,
  )

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map { a =>
      if a.returnsCompletion then
        a.copy(body = CompletionWrapping.expand(a.body))
      else a
    }
