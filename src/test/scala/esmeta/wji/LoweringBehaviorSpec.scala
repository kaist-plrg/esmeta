package esmeta.wji

import org.scalatest.funsuite.AnyFunSuite
import esmeta.cfgBuilder.CFGBuilder
import esmeta.error.AssertionFail
import esmeta.interpreter.{Interpreter => EsInterpreter}
import esmeta.state.{Bool, Context, State, Str, Undef, Value}
import esmeta.state.GLOBAL_RESULT
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

  /** An `And` whose right operand calls [[poison]] — `extractFromCond`
    * deliberately hoists only `And`/`Or`'s left operand, never the right, to
    * preserve short-circuit semantics (hoisting the right unconditionally to
    * the top would evaluate it even when the left is already false).
    *
    * As spec prose, this would read:
    * {{{
    * To <dfn>test and short circuit</dfn> given |flag|, perform the
    * following steps:
    *   1. If |flag| is **true** and [=poison=](**true**) is **true**, then
    *     1. Return "matched".
    *   2. Return "not matched".
    * }}}
    */
  private val testAndShortCircuit = Algorithm(
    id = Some("test-and-short-circuit"),
    name = Some("testAndShortCircuit"),
    params = List(WjiParam("|flag|")),
    head = "",
    body = List(
      Instr.IfChain(
        branches = List(
          (
            Cond.And(
              Cond.Eq(Expr.Var("flag"), Expr.Bool(true)),
              Cond.Eq(
                Expr.AlgoCall("[=poison=]", List(Expr.Bool(true))),
                Expr.Bool(true),
              ),
            ),
            List(Instr.Return(Some(Expr.Str("matched")))),
          ),
        ),
        fallback = List(Instr.Return(Some(Expr.Str("not matched")))),
      ),
    ),
  )

  /** Returns |x| unchanged — a controllable stand-in for [[poison]] where a
    * test needs the right operand of `Cond.And` to actually evaluate to
    * **false** rather than always throwing, to check which branch runs once it
    * does (poison can only prove *whether* a call ran, not what happens once it
    * evaluates falsy).
    *
    * As spec prose, this would read:
    * {{{
    * To <dfn>echo</dfn> given |x|, perform the following steps:
    *   1. Return |x|.
    * }}}
    */
  private val echo = Algorithm(
    id = Some("echo"),
    name = Some("echo"),
    params = List(WjiParam("|x|")),
    head = "",
    body = List(Instr.Return(Some(Expr.Var("x")))),
  )

  /** Same shape as [[testAndShortCircuit]], but with a controllable right
    * operand ([[echo]] instead of [[poison]]) so the branch actually taken once
    * that operand evaluates can be checked — in particular, that a true left
    * operand with a false right operand reaches the `IfChain`'s original
    * fallback rather than falling through to nothing (a risk in any fix that
    * nests the right operand's hoisted call under a guard on the left: the
    * guard's own "false" arm must still lead to that fallback).
    *
    * As spec prose, this would read:
    * {{{
    * To <dfn>test and with else</dfn> given |flag| and |check|, perform the
    * following steps:
    *   1. If |flag| is **true** and [=echo=](|check|) is **true**, then
    *     1. Return "matched".
    *   2. Return "not matched".
    * }}}
    */
  private val testAndWithElse = Algorithm(
    id = Some("test-and-with-else"),
    name = Some("testAndWithElse"),
    params = List(WjiParam("|flag|"), WjiParam("|check|")),
    head = "",
    body = List(
      Instr.IfChain(
        branches = List(
          (
            Cond.And(
              Cond.Eq(Expr.Var("flag"), Expr.Bool(true)),
              Cond.Eq(
                Expr.AlgoCall("[=echo=]", List(Expr.Var("check"))),
                Expr.Bool(true),
              ),
            ),
            List(Instr.Return(Some(Expr.Str("matched")))),
          ),
        ),
        fallback = List(Instr.Return(Some(Expr.Str("not matched")))),
      ),
    ),
  )

  /** Compiles `algos` through the full pipeline and invokes `fname` directly
    * with `args` — no JS entry point or builtin calling convention involved,
    * mirroring `esmeta.phase.WjiInterp`'s own `callAO` helper. Returns the
    * algorithm's return value, read the same way `WjiInterp.callAO` reads it
    * (`st.globals(GLOBAL_RESULT)`, set once the top-level context returns).
    */
  private def invoke(
    algos: List[Algorithm],
    fname: String,
    args: List[esmeta.state.Value],
  ): Value =
    val program = Compiler.compile(Lowering.run(algos))
    val cfg = CFGBuilder(program)
    val f = cfg.getFunc(fname)
    val st = State(cfg, Context(f, MMap.from(f.params.map(_.lhs).zip(args))))
    EsInterpreter(st)
    st.globals.getOrElse(GLOBAL_RESULT, Undef)

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

  test(
    "And short-circuit: the right operand's call must not run when the left operand is already false",
  ) {
    val algos = List(poison, testAndShortCircuit)

    // left is false -> right (poison) must never be evaluated
    invoke(algos, "testandshortcircuit", List(Bool(false)))
  }

  test(
    "And short-circuit: the right operand's call must still run when the left operand is true",
  ) {
    val algos = List(poison, testAndShortCircuit)

    // left is true -> right (poison) IS evaluated -> poison must fire,
    // proving the call was actually compiled and run rather than silently
    // dropped. Currently fails with NotSupported: `extractFromCond`'s
    // `Cond.And` case never hoists the right operand's call at all, so it
    // survives as a raw `AlgoCall` down to `Compiler.compileExpr`'s fallback.
    intercept[AssertionFail] {
      invoke(algos, "testandshortcircuit", List(Bool(true)))
    }
  }

  test(
    "And short-circuit with else: left true, right false must reach the IfChain's original fallback",
  ) {
    val algos = List(echo, testAndWithElse)

    // left true, right true -> matched
    assert(
      invoke(algos, "testandwithelse", List(Bool(true), Bool(true)))
      == Str("matched"),
    )
    // left true, right false -> must land on the *original* fallback, not
    // silently fall through to nothing
    assert(
      invoke(algos, "testandwithelse", List(Bool(true), Bool(false)))
      == Str("not matched"),
    )
    // left false -> fallback, same as always, right never evaluated
    assert(
      invoke(algos, "testandwithelse", List(Bool(false), Bool(true)))
      == Str("not matched"),
    )
  }
