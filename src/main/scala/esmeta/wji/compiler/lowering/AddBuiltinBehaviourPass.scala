package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr, WjiParam}
import esmeta.error.UnsupportedSpecShape

/** Adapts every `Algorithm` [[MarkBuiltinBehaviourPass]] flagged
  * `isBuiltinBehaviour = true` into a valid builtin function body: unpacks
  * `argumentsList` into the closure's own declared parameter names, the fix-up
  * `manuals/rule.json`'s hand-patched `BuiltinCallOrConstruct` rule requires
  * (`BuiltinCallOrConstruct` always invokes a builtin as
  * `func.__CODE__(thisArgument, argumentsList, newTarget)` — a fixed 3-argument
  * calling convention — regardless of what parameters the underlying closure
  * itself declares). Mirrors mainline `esmeta.compiler.Compiler`'s
  * `fixClosurePrefixAOs`/`getBuiltinPrefix`, which solves the exact same gap
  * for ECMA-262's own Abstract Closures (e.g. `NewPromiseCapability`'s
  * resolve/reject functions) by giving them the builtin signature plus a prefix
  * that unpacks `argumentsList` positionally into the real parameter names.
  * `ExpandFollowingStepsPass` itself isn't responsible for this — that pass
  * only hoists a closure/`Algorithm` pair, with no notion of what convention
  * the closure is ultimately invoked under.
  *
  * Doesn't wrap completion records itself — `manuals/rule.json`'s Call rule
  * reads `.[[Type]]`/`.[[Value]]` off whatever `F.__CODE__` returns
  * unconditionally, so every `isBuiltinBehaviour` closure needs one on every
  * exit path, but that's `CompletionAlgorithms.compute`'s job (seeded
  * unconditionally for `isBuiltinBehaviour`, same idea as its `Constructor`
  * case), together with `InsertFallthroughReturnPass`/
  * `WrapCompletionReturnsPass`'s ordinary treatment. This pass runs before
  * those (see `Lowering.pipeline`), so a hoisted closure's body reaches them
  * looking like an ordinary algorithm body.
  *
  * Doesn't itself detect which `Algorithm` needs this — see
  * [[MarkBuiltinBehaviourPass]] for that.
  *
  * Category: Structural desugaring — Injection.
  */
object AddBuiltinBehaviourPass extends LoweringPass:

  /** Requires:
    *   - [[MarkBuiltinBehaviourPass]]: needs `isBuiltinBehaviour` already
    *     stamped onto every `Algorithm` to know which ones to target.
    */
  override def requires: Set[LoweringPass] = Set(MarkBuiltinBehaviourPass)

  /** formal parameter names `BuiltinCallOrConstruct`'s hand-patched IR always
    * calls `func.__CODE__` with, regardless of what parameters the underlying
    * `behaviour` closure itself declares.
    */
  private val BuiltinParams = List("thisArgument", "argumentsList", "newTarget")

  private def stripPipes(s: String): String =
    s.stripPrefix("|").stripSuffix("|")

  /** the instructions that run in `unpackArgumentsList`'s `IfChain` when fewer
    * arguments were actually supplied than a param's own position — mirrors
    * [[AddInterfaceMemberBuiltinBehaviourPass.omittedBranch]] (duplicated for
    * the same reason `unpackArgumentsList` itself is). In practice a `Plain`
    * algorithm's [[WjiParam.default]] is always `None` — the only source of
    * `optional`/`default` here is `AlgorithmExtractor.extractParams`'s "using
    * optional X |Y|" prose detection, which has no default-value syntax of its
    * own (unlike `esmeta.wji.extractor.Extractor.enrichParamTypes`'s WebIDL
    * source, which only stamps a `Method`/`Constructor`-kind algorithm) — but
    * the logic is identical either way, so it's not special-cased away.
    */
  private def omittedBranch(p: WjiParam, name: String): List[Instr] =
    if !p.optional then List(Instr.Assert(Cond.Unreachable))
    else
      p.default match
        case None => Nil
        case Some("{}") =>
          List(
            Instr.Perform(
              "OrdinaryObjectCreate",
              List(Expr.SpecTerm("null")),
              Instr.PerformOutcome.BindResult(name),
            ),
          )
        case Some(other) =>
          throw UnsupportedSpecShape(
            "AddBuiltinBehaviourPass",
            s"parameter ${p.name} has unsupported default value: $other",
          )

  /** the `params.zipWithIndex` prefix instructions that unpack a builtin's
    * `argumentsList` into the closure's own declared parameter names — mirrors
    * mainline `Compiler.getBuiltinPrefix`'s two param kinds:
    *
    *   - a `Normal` param (the common case) is unpacked positionally — see
    *     [[omittedBranch]] for what happens when fewer arguments were actually
    *     supplied than its own position.
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
          omittedBranch(p, name),
        )
    }

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map { a =>
      if a.isBuiltinBehaviour then
        a.copy(
          params = BuiltinParams.map(p => WjiParam(s"|$p|")),
          body = unpackArgumentsList(a.params) ++ a.body,
        )
      else a
    }
