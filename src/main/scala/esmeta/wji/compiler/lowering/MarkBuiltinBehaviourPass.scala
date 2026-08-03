package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Expr, Instr}

/** Stamps `isBuiltinBehaviour = true` onto every closure
  * [[ExpandFollowingStepsPass]] hoisted for `CreateBuiltinFunction`'s
  * `behaviour` argument, so [[AddBuiltinBehaviourPass]] (and its own
  * `preconditions`) can just read the field off whichever `Algorithm` it's
  * handed, mirroring [[MarkCompletionAlgorithmsPass]]'s exact relationship to
  * [[WrapCompletionReturnsPass]] — see that pass's own doc for the general
  * shape of this mark-then-consume split.
  *
  * Detects the shape `ExpandFollowingStepsPass` leaves behind: a `Let(Var(x),
  * Closure(closureName, _), _)` whose sibling steps (`rest`) contain a
  * `CreateBuiltinFunction(Var(x), ...)` call — the same detection
  * `ExpandFollowingStepsPass` itself used to do inline before this pass
  * existed. Kept as its own pass (rather than folded back into
  * `ExpandFollowingStepsPass`) specifically so the classification only happens
  * once: both `AddBuiltinBehaviourPass`'s `preconditions` (checked by
  * `Lowering.run` before its `run`) and `run` itself can read the persisted
  * field instead of each independently re-walking every algorithm's body to
  * recompute the same `Set[String]` of closure names.
  *
  * Category: Structural desugaring — Injection.
  */
object MarkBuiltinBehaviourPass extends LoweringPass:

  /** Requires:
    *   - [[ExpandFollowingStepsPass]]: needs its `Closure`s already hoisted to
    *     scan for.
    *   - [[ExpandInlineAlgoCallPass]]: needs a `CreateBuiltinFunction` call
    *     already in `Perform` form (not a raw `Expr.AlgoCall`) to recognize it
    *     among a closure's sibling steps.
    */
  override def requires: Set[LoweringPass] =
    Set(ExpandFollowingStepsPass, ExpandInlineAlgoCallPass)

  /** whether the closure bound to `varName` is passed as
    * `CreateBuiltinFunction`'s `behaviour` argument among its sibling steps
    * `rest` — the shape every WJI spec text with this pattern uses so far, e.g.
    * WebIDL's `react`: `Let onFulfilledSteps be the following steps given
    * argument V: ...` immediately followed by `Let onFulfilled be
    * CreateBuiltinFunction(onFulfilledSteps, 1, "", « »).` in the very same
    * algorithm.
    */
  private def isBuiltinBehaviour(varName: String, rest: List[Instr]): Boolean =
    rest.exists {
      case Instr.Perform("CreateBuiltinFunction", args, _, _) =>
        args.headOption.contains(Expr.Var(varName))
      case _ => false
    }

  /** Collects every hoisted closure's name that's used as a builtin behaviour,
    * recursing through nested branches/loops the same way
    * `CompletionAlgorithms`'s own traversal helpers do (an `IfChain`'s `body`
    * is always `Nil` — its real nested content lives in `branches`/ `fallback`
    * instead, so it needs its own case rather than the generic `instr.body`
    * fallback).
    */
  private def collectBuiltinClosureNames(instrs: List[Instr]): Set[String] =
    instrs match
      case Nil => Set.empty
      case Instr.Let(Expr.Var(x), Expr.Closure(closureName, _), _) :: rest =>
        val here =
          if isBuiltinBehaviour(x, rest) then Set(closureName) else Set.empty
        here ++ collectBuiltinClosureNames(rest)
      case (c: Instr.IfChain) :: rest =>
        c.branches.flatMap((_, b) => collectBuiltinClosureNames(b)).toSet ++
        collectBuiltinClosureNames(c.fallback) ++
        collectBuiltinClosureNames(rest)
      case instr :: rest =>
        collectBuiltinClosureNames(instr.body) ++ collectBuiltinClosureNames(
          rest,
        )

  def run(algos: List[Algorithm]): List[Algorithm] =
    val builtinNames =
      algos.flatMap(a => collectBuiltinClosureNames(a.body)).toSet
    algos.map { a =>
      if a.name.exists(builtinNames.contains) then
        a.copy(isBuiltinBehaviour = true)
      else a
    }
