package esmeta.wji.compiler.lowering

import esmeta.wji.lang.Algorithm

/** For every algorithm with `returnsCompletion = true` — a definite or
  * transitively-inferred completion-returning operation (see
  * [[CompletionAlgorithms]]/[[MarkCompletionAlgorithmsPass]]) — wraps every
  * `Return`/`Throw` exit path in an ECMA-262 Completion Record via
  * [[CompletionWrapping.expandAlgorithm]] (see that object's own doc for the
  * actual rewrite, the ECMA-262 rationale, and why the wrapping logic itself
  * lives there rather than in this pass).
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
    // the exact complement of MarkCompletionAlgorithmsPass's own fixed point
    // (CompletionAlgorithms.compute): every algorithm name it did *not* mark
    // `returnsCompletion = true` is one it proved can only ever exit with a
    // plain, non-Completion value (see CompletionWrapping.expandAlgorithm's
    // own doc on `pureCallees` for why that's sound to pass down here).
    val pureCallees: Set[String] =
      algos
        .filterNot(_.returnsCompletion)
        .flatMap(a => a.name.orElse(a.id))
        .map(_.toLowerCase)
        .toSet
    algos.map { a =>
      if a.returnsCompletion then
        a.copy(body = CompletionWrapping.expandAlgorithm(a.body, pureCallees))
      else a
    }
