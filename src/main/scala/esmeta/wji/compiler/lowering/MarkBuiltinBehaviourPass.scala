package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Expr, Instr}
import esmeta.wji.lang.walker.Walker

/** Stamps `isBuiltinBehaviour = true` onto every `Expr.FollowingSteps` used as
  * `CreateBuiltinFunction`'s `behaviour` argument, so
  * [[AddBuiltinBehaviourPass]] can just read the field off whichever
  * `FollowingSteps` it's looking at, and [[ExpandFollowingStepsPass]] can carry
  * it over onto the `Algorithm` it hoists (where `CompletionAlgorithms` reads
  * it), mirroring [[MarkCompletionAlgorithmsPass]]'s exact relationship to
  * [[WrapCompletionReturnsPass]] — see that pass's own doc for the general
  * shape of this mark-then-consume split.
  *
  * Detects a `Let(Var(x), expr, substeps)` whose sibling steps (`rest`) contain
  * a `CreateBuiltinFunction(Var(x), ...)` call, and marks the `FollowingSteps`
  * `expr` is or contains (see [[mark]]).
  *
  * Runs *before* [[ExpandFollowingStepsPass]], on the not-yet-hoisted
  * placeholder rather than on the hoisted `Closure`/`Algorithm` pair: a builtin
  * behaviour binds its own `this`, and hoisting needs to already know that to
  * decide whether `this` is one of the closure's captured variables or not (see
  * [[ExpandFollowingStepsPass]]'s own doc).
  *
  * Category: Structural desugaring — Injection.
  */
object MarkBuiltinBehaviourPass extends LoweringPass:

  /** Must precede:
    *   - [[ExpandFollowingStepsPass]]: see class doc — the `FollowingSteps`
    *     this pass marks no longer exist once that pass has run.
    */
  override def mustPrecede: Set[LoweringPass] = Set(ExpandFollowingStepsPass)

  /** the arguments of a `CreateBuiltinFunction` call an instruction makes, in
    * whichever shape it's currently in — `Instr.Perform` if
    * [[ExpandInlineAlgoCallPass]] already ran, or still a raw `Instr.Let(_,
    * Expr.AlgoCall(...) | Expr.JSCall(...), _)` if it hasn't (the spec text's
    * own `[$CreateBuiltinFunction$](...)` reference into mainline ECMA-262
    * parses to `Expr.JSCall`, matching
    * [[ExpandInlineAlgoCallPass.extractCall]]'s own two cases) — so this
    * detection doesn't actually need that pass to have run first.
    */
  private def createBuiltinFunctionArgs(instr: Instr): Option[List[Expr]] =
    instr match
      case Instr.Perform("CreateBuiltinFunction", args, _, _) => Some(args)
      case Instr.Let(_, Expr.AlgoCall("CreateBuiltinFunction", args), _) =>
        Some(args)
      case Instr.Let(_, Expr.JSCall("CreateBuiltinFunction", args), _) =>
        Some(args)
      case _ => None

  /** whether the closure bound to `varName` is passed as
    * `CreateBuiltinFunction`'s `behaviour` argument among its sibling steps
    * `rest` — the shape every WJI spec text with this pattern uses so far, e.g.
    * WebIDL's `react`: `Let onFulfilledSteps be the following steps given
    * argument V: ...` immediately followed by `Let onFulfilled be
    * CreateBuiltinFunction(onFulfilledSteps, 1, "", « »).` in the very same
    * algorithm.
    */
  private def isBuiltinBehaviour(varName: String, rest: List[Instr]): Boolean =
    rest.exists(i =>
      createBuiltinFunctionArgs(i).exists(
        _.headOption.contains(Expr.Var(varName)),
      ),
    )

  /** Marks the `FollowingSteps` reachable from `expr`, at any nesting depth —
    * not just `expr` itself, since the closure may be only one alternative of
    * the bound value (e.g. `create an interface object`'s "Let steps be I's
    * overridden constructor steps if they exist, or the following steps
    * otherwise", a `FollowingSteps` inside an `Expr.Conditional`).
    */
  private def mark(expr: Expr): Expr =
    val marker = new Walker:
      override def walk(expr: Expr): Expr = expr match
        case fs: Expr.FollowingSteps => fs.copy(isBuiltinBehaviour = true)
        case other                   => super.walk(other)
    marker.walk(expr)

  /** Marks every builtin-behaviour `FollowingSteps` in `instrs`, recursing into
    * every nested body — including a `FollowingSteps`-owning `Let`'s own
    * `body`, the closure's substeps, which may themselves define further
    * builtin behaviours (e.g. a `react`-inside-`react` call site).
    */
  private def transform(instrs: List[Instr]): List[Instr] =
    instrs match
      case Nil => Nil
      case (i @ Instr.Let(Expr.Var(x), expr, body)) :: rest
          if isBuiltinBehaviour(x, rest) =>
        i.copy(expr = mark(expr), body = transform(body)) :: transform(rest)
      case instr :: rest =>
        instr.mapBody(transform) :: transform(rest)

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = transform(a.body)))
