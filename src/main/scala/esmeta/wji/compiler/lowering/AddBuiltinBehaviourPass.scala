package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr, WjiParam}

/** Adapts every closure [[ExpandFollowingStepsPass]] hoisted for
  * `CreateBuiltinFunction`'s `behaviour` argument into a valid builtin function
  * body. Two independent fix-ups `manuals/rule.json`'s hand-patched
  * `BuiltinCallOrConstruct` rule requires, neither of which
  * `ExpandFollowingStepsPass` itself is responsible for — that pass only hoists
  * a closure/`Algorithm` pair, with no notion of what convention the closure is
  * ultimately invoked under:
  *
  *   - '''Builtin arg unpacking''': `BuiltinCallOrConstruct` always invokes a
  *     builtin as `func.__CODE__(thisArgument, argumentsList, newTarget)` — a
  *     fixed 3-argument calling convention — regardless of what parameters the
  *     underlying closure itself declares. Mirrors mainline
  *     `esmeta.compiler.Compiler`'s `fixClosurePrefixAOs`/`getBuiltinPrefix`,
  *     which solves the exact same gap for ECMA-262's own Abstract Closures
  *     (e.g. `NewPromiseCapability`'s resolve/reject functions) by giving them
  *     the builtin signature plus a prefix that unpacks `argumentsList`
  *     positionally into the real parameter names.
  *   - '''Completion-record wrapping''': `manuals/rule.json`'s Call rule (`Let
  *     result be the Completion Record that is the result of evaluating...
  *     F.__CODE__(...)`) reads `.[[Type]]`/`.[[Value]]` off whatever
  *     `F.__CODE__` returns unconditionally, mirroring mainline's own
  *     `esmeta.compiler.Compiler.compile`'s `Builtin => needRetComp = true` —
  *     *not* gated on whether the closure itself can abruptly complete, unlike
  *     [[WrapCompletionReturnsPass]]'s own `returnsCompletion` test for
  *     ordinary algorithms. Reuses [[CompletionWrapping.expandAlgorithm]]
  *     directly — see that object's own doc for why it's a shared plain
  *     function rather than living inside `WrapCompletionReturnsPass`.
  *
  * Detects which hoisted `Algorithm` needs this by re-scanning for the exact
  * shape `ExpandFollowingStepsPass` leaves behind: a `Let(Var(x),
  * Closure(closureName, _), _)` whose sibling steps (`rest`) contain a
  * `CreateBuiltinFunction(Var(x), ...)` call — the same detection
  * `ExpandFollowingStepsPass` itself used to do inline before this pass
  * existed. Collected as a plain `Set[String]` of closure names local to `run`,
  * not persisted as an `Algorithm` field the way `returnsCompletion` is: unlike
  * that field, nothing else in the pipeline needs this fact, so a separate
  * mark-then-consume pair of passes would only add indirection here.
  *
  * Category: Structural desugaring.
  */
object AddBuiltinBehaviourPass extends LoweringPass:

  /** Requires:
    *   - [[ExpandFollowingStepsPass]]: needs its `Closure`s already hoisted to
    *     scan for.
    *   - [[ExtractInlineAlgoCallPass]]: needs a `CreateBuiltinFunction` call
    *     already in `Perform` form (not a raw `Expr.AlgoCall`) to recognize it
    *     among a closure's sibling steps.
    */
  override def requires: Set[LoweringPass] =
    Set(ExpandFollowingStepsPass, ExtractInlineAlgoCallPass)

  /** formal parameter names `BuiltinCallOrConstruct`'s hand-patched IR always
    * calls `func.__CODE__` with, regardless of what parameters the underlying
    * `behaviour` closure itself declares.
    */
  private val BuiltinParams = List("thisArgument", "argumentsList", "newTarget")

  private def stripPipes(s: String): String =
    s.stripPrefix("|").stripSuffix("|")

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

  /** the `params.zipWithIndex` prefix instructions that unpack a builtin's
    * `argumentsList` positionally into the closure's own declared parameter
    * names, defaulting to `undefined` past the end of the list — mirrors
    * mainline `Compiler.getBuiltinPrefix`'s `Param(name, _, Normal)` case (a
    * hoisted WJI closure never has a variadic/optional parameter, so there's no
    * need for the other cases that function has).
    */
  private def unpackArgumentsList(params: List[String]): List[Instr] =
    params.zipWithIndex.map {
      case (p, i) =>
        Instr.IfChain(
          List(
            Cond.Compare(
              Expr.Num(i.toString),
              Cond.CompareOp.Lt,
              Expr.Length(Expr.Var("argumentsList")),
            ) -> List(
              Instr.Let(
                Expr.Var(p),
                Expr.Index(Expr.Var("argumentsList"), Expr.Num(i.toString)),
              ),
            ),
          ),
          List(Instr.Let(Expr.Var(p), Expr.SpecTerm("undefined"))),
        )
    }

  def run(algos: List[Algorithm]): List[Algorithm] =
    val builtinNames =
      algos.flatMap(a => collectBuiltinClosureNames(a.body)).toSet
    algos.map { a =>
      if a.name.exists(builtinNames.contains) then
        val originalParams = a.params.map(p => stripPipes(p.name))
        a.copy(
          params = BuiltinParams.map(p => WjiParam(s"|$p|")),
          body = unpackArgumentsList(originalParams) ++ CompletionWrapping
            .expandAlgorithm(a.body),
        )
      else a
    }
