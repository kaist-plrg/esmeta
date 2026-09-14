package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Expr, Instr}
import esmeta.wji.lang.walker.Walker

/** Threads the bit width `ExpandIsOfFormPass.buildFormMatch` attaches to an
  * f32/f64 `f32.const`/`f64.const` payload — `Expr.WasmFloatPayload(width,
  * ...)`, bound to a plain `Var` by a `Let` right at the `IsOfForm` destructure
  * site — forward to wherever that `Var` is later read back out through
  * `Expr.AsMath` ("|f32|/|f64| interpreted as a [=mathematical value=]",
  * `ToJSValue`'s own phrasing), which is the one place the width still matters
  * (`Compiler`'s `AsMath(WasmFloatPayload(width, e))` case, `docs/hardcodes.md`
  * #19's counterpart entry).
  *
  * Necessary because a bound `Var` carries no type information of its own by
  * this point in the pipeline — `AsMath(Var("f32"))`, on its own, has already
  * lost the fact that `f32` came from an `F32` (rather than `F64`, or any
  * other) `CONST` payload; that fact only still exists at the `Let` that bound
  * it. This pass is a small single-pass forward propagation over each
  * algorithm's own body, tracking (in `Rewriter`'s own mutable `env`) which
  * variable names are currently known to hold a wasm float payload of which
  * width, and rewriting a later `AsMath(Var(name))` into `AsMath(
  * WasmFloatPayload(width, Var(name)))` wherever `name` is in scope --
  * `Compiler`'s special-cased nested match on exactly that shape is what then
  * picks the right conversion (`docs/hardcodes.md` #19's `ToMathF32`/
  * `ToMathF64`) over the generic, width-blind `AsMath(e) => EConvert(ToMath,
  * ...)` case.
  *
  * `env` only ever grows (never shadowed/removed) — safe here because every
  * `Var` this pipeline produces is effectively assigned exactly once (no reuse
  * of the same name for an unrelated binding later in the same algorithm), the
  * same assumption every other single-pass rewrite in this pipeline already
  * relies on.
  *
  * Category: Spec-dependent — WJI.
  */
object PropagateWasmFloatPayloadPass extends LoweringPass:

  /** Requires:
    *   - [[ExpandIsOfFormPass]]: needs `Expr.WasmFloatPayload` already attached
    *     to the `Let` bindings it produces for an f32/f64 `CONST` payload's own
    *     `Var` — this pass does no destructuring of its own, only
    *     forward-propagates a shape that pass already built.
    */
  override def requires: Set[LoweringPass] = Set(ExpandIsOfFormPass)

  private class Rewriter extends Walker:
    private var env: Map[String, Int] = Map.empty

    override def walk(expr: Expr): Expr = expr match
      case Expr.AsMath(Expr.Var(name)) if env.contains(name) =>
        Expr.AsMath(Expr.WasmFloatPayload(env(name), Expr.Var(name)))
      case other => super.walk(other)

    override def walk(instr: Instr): Instr = instr match
      case Instr.Let(lhs, rhs, body) =>
        val walkedLhs = walk(lhs)
        val walkedRhs = walk(rhs)
        (walkedLhs, walkedRhs) match
          case (Expr.Var(name), Expr.WasmFloatPayload(width, _)) =>
            env = env.updated(name, width)
          case _ =>
        Instr.Let(walkedLhs, walkedRhs, body.map(walk))
      case other => super.walk(other)

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map { a =>
      val rewriter = Rewriter()
      a.copy(body = a.body.map(rewriter.walk))
    }
