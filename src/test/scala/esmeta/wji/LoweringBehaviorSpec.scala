package esmeta.wji

import org.scalatest.funsuite.AnyFunSuite
import esmeta.cfgBuilder.CFGBuilder
import esmeta.error.AssertionFail
import esmeta.interpreter.{Interpreter => EsInterpreter}
import esmeta.state.{Bool, Context, State}
import esmeta.wji.compiler.Compiler
import esmeta.wji.compiler.lowering.Lowering
import esmeta.wji.lang.*
import scala.collection.mutable.{Map => MMap}

/** Scenario/behavior tests for the lowering+compile pipeline: metalang is
  * hand-built directly with `Instr`/`Expr`/`Cond` constructors (bypassing
  * spec-prose extraction, so only `Lowering`+`Compiler` are under test), run
  * through `Lowering.run` -> `Compiler.compile` -> `CFGBuilder` ->
  * `Interpreter`, and checked by observable runtime behavior rather than
  * intermediate IR/metalang shape. Self-contained — no mainline CFG merge
  * needed, since every call made by a hand-built algorithm here targets another
  * algorithm defined in the same file.
  */
class LoweringBehaviorSpec extends AnyFunSuite:

  /** Unconditionally fails when actually invoked — a "poison" marker used to
    * detect whether a call embedded in a branch condition was evaluated. Takes
    * an (unused) argument on purpose: `NormalizeEvaluationOrderPass` only
    * hoists a non-trivial `AlgoCall` when `args.nonEmpty` — a zero-arg
    * `AlgoCall` compiles to a bare value reference rather than an actual call
    * (see `Compiler.compileExpr`'s `AlgoCall` case), so it would never execute
    * at all regardless of where it's placed.
    *
    * As spec prose, this would read:
    * {{{
    * To <dfn>poison</dfn> given |x|, perform the following steps:
    *   1. Assert: **false**.
    * }}}
    */
  private val poison = Algorithm(
    id = Some("poison"),
    name = Some("poison"),
    params = List(WjiParam("|x|")),
    head = "",
    body = List(Instr.Assert(Cond.Eq(Expr.Bool(true), Expr.Bool(false)))),
  )

  /** An `IfChain` whose second branch's condition calls [[poison]] — the shape
    * `NormalizeEvaluationOrderPass` must hoist `[=poison=](**true**)` out of
    * before `Compiler` ever sees it (a raw non-trivial `AlgoCall` can't sit
    * inside a `Cond` — see `Compiler.compileExpr`'s `AlgoCall` case), and must
    * hoist it *only* into that branch's own guarded arm, not unconditionally to
    * the top.
    *
    * As spec prose, this would read:
    * {{{
    * To <dfn>test if chain</dfn> given |flag|, perform the following steps:
    *   1. If |flag| is **true**, then
    *     1. Return "first".
    *   2. Else if [=poison=](**true**) is **true**, then
    *     1. Return "second".
    *   3. Else,
    *     1. Return "fallback".
    * }}}
    */
  private val testIfChain = Algorithm(
    id = Some("test-if-chain"),
    name = Some("testIfChain"),
    params = List(WjiParam("|flag|")),
    head = "",
    body = List(
      Instr.IfChain(
        branches = List(
          (
            Cond.Eq(Expr.Var("flag"), Expr.Bool(true)),
            List(Instr.Return(Some(Expr.Str("first")))),
          ),
          (
            Cond.Eq(
              Expr.AlgoCall("[=poison=]", List(Expr.Bool(true))),
              Expr.Bool(true),
            ),
            List(Instr.Return(Some(Expr.Str("second")))),
          ),
        ),
        fallback = List(Instr.Return(Some(Expr.Str("fallback")))),
      ),
    ),
  )

  /** Compiles `algos` through the full pipeline and invokes `fname` directly
    * with `args` — no JS entry point or builtin calling convention involved,
    * mirroring `esmeta.phase.WjiInterp`'s own `callAO` helper.
    */
  private def invoke(
    algos: List[Algorithm],
    fname: String,
    args: List[esmeta.state.Value],
  ): Unit =
    val program = Compiler.compile(Lowering.run(algos))
    val cfg = CFGBuilder(program)
    val f = cfg.getFunc(fname)
    val st = State(cfg, Context(f, MMap.from(f.params.map(_.lhs).zip(args))))
    EsInterpreter(st)

  test(
    "IfChain hoisting: a later branch's side-effecting call must not run once an earlier branch already matched",
  ) {
    val algos = List(poison, testIfChain)

    // first branch matches -> second branch's condition (and its poison
    // call) must never be evaluated
    invoke(algos, "testifchain", List(Bool(true)))

    // first branch doesn't match -> second branch's condition IS evaluated
    // -> poison must fire, proving the call wasn't left un-hoisted either
    intercept[AssertionFail] {
      invoke(algos, "testifchain", List(Bool(false)))
    }
  }
