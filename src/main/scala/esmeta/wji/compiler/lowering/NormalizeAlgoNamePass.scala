package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, AlgorithmKind, Expr, Instr}
import esmeta.wji.lang.walker.Walker

/** Normalizes each `Plain`-kind algorithm's `name` — space-to-underscore *and*
  * lower-cased, so it's both a valid function identifier and matches how
  * `esmeta.wji.compiler.Compiler.compileAlgo` registers `Func` names — and does
  * the same inside every [[Expr.Closure]] reference to a (possibly synthetic,
  * lowering-pass-generated) algorithm name, so a closure reference always
  * matches its target's registered name exactly (`cfg.fnameMap` lookups are
  * case-sensitive even though the Bikeshed prose these names ultimately derive
  * from is not).
  *
  * A Getter/Setter/Constructor/Method-kind algorithm's `name` is left exactly
  * as extracted instead — `Compiler.compileAlgo` registers all four
  * case-preserved (see `AddInterfaceMemberBuiltinBehaviourPass`), matching the
  * real, case-sensitive JS property names `manuals/intrinsics` expects (e.g.
  * `Global.value`'s setter, `Module.customSections`'s real casing rather than
  * `customsections`) — every one of those kinds is only ever reached via real
  * property/call access, never via a `[=link=]`-style reference
  * `nameFromLink`'s Bikeshed case-insensitivity exists for in the first place.
  * `Method` used to be excluded from this and normalized like `Plain` instead
  * (a former TODO) — its own `js-api`/`Interpreter.EClo`'s
  * exact-then-lowercase-retry lookup convention means any genuine
  * `[=link=]`-style reference to a Method algorithm still resolves correctly
  * either way, so there was no reason left to single it out from the other
  * three case-preserved kinds.
  *
  * [[Expr.AlgoCall]]'s `link` and [[Instr.Perform]]'s `func` are deliberately
  * left on space-only normalization here (not lower-cased): unlike
  * `Algorithm.name`/`Closure.name`, a `func`/`link` can also be a literal
  * reference to a manually-registered native hook (e.g.
  * `"HostEnqueuePromiseJob"`) whose real name is case-sensitive, not a
  * lower-cased Bikeshed dfn — lower-casing those is instead handled
  * conditionally at compile time by `Compiler.nameFromLink`, which only folds
  * case for `[=...=]`-wrapped (i.e. genuinely Bikeshed-derived) references.
  *
  * This isn't a desugaring/lowering (it changes no control-flow/expression
  * shape) but lives alongside the other passes since it needs to run over the
  * same `List[Algorithm]` before/after which the rest of the pipeline runs.
  *
  * Deliberately positioned last, not because it `requires` any one specific
  * pass, but because it needs to see the *final* set of `Algorithm.name`/
  * `Expr.Closure`/`Expr.AlgoCall`/`Instr.Perform.func` occurrences — several
  * earlier passes (`ExpandFollowingStepsPass`, `ExpandQueueATaskPass`,
  * `WrapCompletionReturnsPass`, ...) each introduce more of these as they run.
  * Normalization is idempotent and order-independent given the final tree, so
  * running last (rather than declaring a `requires` on every producer) is both
  * correct and simpler — no `LoweringPass` before it needs anything back *from*
  * it, so nothing stops it running anywhere after all of them; last just
  * guarantees it's really seen everything.
  *
  * Category: Housekeeping.
  */
object NormalizeAlgoNamePass extends LoweringPass:
  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map { a =>
      val name = a.kind match
        case AlgorithmKind.Plain =>
          a.name.map(normalize)
        case _ => a.name
      a.copy(name = name, body = a.body.map(normalizer.walk))
    }

  private def underscore(s: String): String = s.replace(' ', '_')
  private def normalize(s: String): String = underscore(s).toLowerCase

  /** Only overrides the node types it actually renames ([[Expr.AlgoCall]]'s
    * `link`, [[Expr.AlgoRef]]'s `link`, [[Expr.Closure]]'s `name`,
    * `Instr.Perform`'s `func`) — [[Expr.Case]] is deliberately left alone: its
    * `tag` is never a function name (see class doc).
    */
  private object normalizer extends Walker:
    override def walk(expr: Expr): Expr = expr match
      case Expr.AlgoCall(link, args) =>
        Expr.AlgoCall(underscore(link), args.map(walk))
      case Expr.AlgoRef(link) =>
        Expr.AlgoRef(underscore(link))
      case Expr.Closure(name, captured) =>
        Expr.Closure(normalize(name), captured)
      case other => super.walk(other)

    override def walk(instr: Instr): Instr = instr match
      case i: Instr.Perform => super.walk(i.copy(func = underscore(i.func)))
      case other            => super.walk(other)
