package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr, WjiParam}
import esmeta.error.UnsupportedSpecShape

/** Adapts every `Algorithm` [[MarkBuiltinBehaviourPass]] flagged
  * `isBuiltinBehaviour = true` into a valid builtin function body. Two
  * independent fix-ups `manuals/rule.json`'s hand-patched
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
  * Doesn't itself detect which `Algorithm` needs this — see
  * [[MarkBuiltinBehaviourPass]] for that (kept as a separate pass so its
  * classification is computed once and shared between this pass's own `run` and
  * its [[preconditions]], rather than each independently re-deriving the same
  * information).
  *
  * [[preconditions]] checks the one assumption `run` relies on: every flagged
  * closure's body already ends in an explicit top-level `Return`/`Throw`.
  * `CreateBuiltinFunction`'s own `behaviour` parameter is defined as an
  * Abstract Closure that "must return either a normal completion containing an
  * ECMAScript language value or a throw completion", so a real spec text
  * satisfying that contract can never actually fall off the end.
  * [[CompletionWrapping.expandAlgorithm]] only rewrites *existing*
  * `Return`/`Throw` nodes (same as [[WrapCompletionReturnsPass]]); unlike that
  * pass, this one has no [[InsertFallthroughReturnPass]] upstream of it to
  * guarantee one exists first (that pass runs before
  * [[ExpandFollowingStepsPass]] hoists these closures out at all — see its own
  * doc), so the assumption is checked directly rather than patched around,
  * failing loudly via `UnsupportedSpecShape` (see `Lowering.run`) instead of
  * silently falling through to `Compiler.compileAlgo`'s raw, un-wrapped
  * `~unused~` fallback if it's ever wrong.
  *
  * Category: Structural desugaring.
  */
object AddBuiltinBehaviourPass extends LoweringPass:

  /** Requires:
    *   - [[MarkBuiltinBehaviourPass]]: needs `isBuiltinBehaviour` already
    *     stamped onto every `Algorithm` to know which ones to target.
    */
  override def requires: Set[LoweringPass] = Set(MarkBuiltinBehaviourPass)

  /** Delegates to `Instr.alwaysExits` (same helper
    * `InsertFallthroughReturnPass` and `Compiler.compileAlgo` use), which
    * recognizes an `IfChain` as exhaustive only when it has a real `else` and
    * every branch, including the fallback, itself always exits — same caveat as
    * there: doesn't look inside loop bodies (`ForEach`/`For`/`While`).
    */
  override def preconditions: List[Condition] = List(
    Condition(
      "every isBuiltinBehaviour closure ends in an explicit top-level " +
      "Return/Throw (CreateBuiltinFunction's behaviour contract)",
      _.forall(a => !a.isBuiltinBehaviour || Instr.alwaysExits(a.body)),
    ),
  )

  /** formal parameter names `BuiltinCallOrConstruct`'s hand-patched IR always
    * calls `func.__CODE__` with, regardless of what parameters the underlying
    * `behaviour` closure itself declares.
    */
  private val BuiltinParams = List("thisArgument", "argumentsList", "newTarget")

  private def stripPipes(s: String): String =
    s.stripPrefix("|").stripSuffix("|")

  /** the `params.zipWithIndex` prefix instructions that unpack a builtin's
    * `argumentsList` into the closure's own declared parameter names — mirrors
    * mainline `Compiler.getBuiltinPrefix`'s two param kinds:
    *
    *   - a `Normal` param (the common case) is unpacked positionally,
    *     defaulting to `undefined` past the end of the list.
    *   - a `variadic` param ([[WjiParam.variadic]] — see
    *     `esmeta.wji.lang.Expr.FollowingSteps`'s "given the list of arguments
    *     V" phrasing) binds the *entire* `argumentsList` directly, mirroring
    *     `getBuiltinPrefix`'s `Param(name, _, Variadic)` case for `remaining ==
    *     0` (a direct alias, no slicing needed) — the only shape seen in the
    *     corpus today (`create a new Exported Function`'s hoisted `argValues`
    *     closure, SpecPatch #22) has no preceding `Normal` params to skip. A
    *     variadic param anywhere but index 0 would need `getBuiltinPrefix`'s
    *     fuller slice-construction (copying from an offset into a fresh list);
    *     nothing needs that yet, so it fails loudly via `UnsupportedSpecShape`
    *     instead of being spec­ulatively built.
    */
  private def unpackArgumentsList(params: List[WjiParam]): List[Instr] =
    params.zipWithIndex.map {
      case (p, i) if p.variadic =>
        if i == 0 then
          Instr.Let(Expr.Var(stripPipes(p.name)), Expr.Var("argumentsList"))
        else
          throw UnsupportedSpecShape(
            "AddBuiltinBehaviourPass",
            s"variadic parameter ${p.name} at index $i — only a sole " +
            "variadic parameter at index 0 is supported today",
          )
      case (p, i) =>
        val name = stripPipes(p.name)
        Instr.IfChain(
          List(
            Cond.Compare(
              Expr.Num(i.toString),
              Cond.CompareOp.Lt,
              Expr.Length(Expr.Var("argumentsList")),
            ) -> List(
              Instr.Let(
                Expr.Var(name),
                Expr.Index(Expr.Var("argumentsList"), Expr.Num(i.toString)),
              ),
            ),
          ),
          List(Instr.Let(Expr.Var(name), Expr.SpecTerm("undefined"))),
        )
    }

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map { a =>
      if a.isBuiltinBehaviour then
        a.copy(
          params = BuiltinParams.map(p => WjiParam(s"|$p|")),
          body = unpackArgumentsList(a.params) ++ CompletionWrapping
            .expandAlgorithm(a.body),
        )
      else a
    }
