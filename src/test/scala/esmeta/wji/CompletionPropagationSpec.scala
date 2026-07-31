package esmeta.wji

import org.scalatest.funsuite.AnyFunSuite
import esmeta.ESMetaTest
import esmeta.cfg.CFG
import esmeta.cfgBuilder.CFGBuilder
import esmeta.interpreter.{Interpreter => EsInterpreter}
import esmeta.ir.Program
import esmeta.state.{
  Addr,
  Bool,
  Context,
  Enum,
  RecordObj,
  State,
  Str,
  Undef,
  Value,
  GLOBAL_RESULT,
}
import esmeta.wji.compiler.Compiler
import esmeta.wji.compiler.lowering.Lowering
import esmeta.wji.lang.*
import scala.collection.mutable.{Map => MMap}

/** Scenario tests for how the lowering pipeline handles abrupt-completion
  * propagation end to end — `ExpandAbruptPass` (explicit `?`/`!`),
  * `PropagateUnguardedCallsPass` (implicit propagation into an unmarked call),
  * `WrapCompletionReturnsPass`/`CompletionWrapping` (wrapping a
  * completion-returning algorithm's own `Return`/`Throw`), and
  * `ExpandThrowsPass` (the "if this throws an exception, catch it" idiom) all
  * interacting on the same call site. What's under test here is specifically
  * the *interaction* between these passes, not any one of them in isolation —
  * mirrors [[LoweringBehaviorSpec]]'s pattern of running hand-built
  * `Algorithm`s through the real `Lowering.run` -> `Compiler.compile` ->
  * `CFGBuilder` -> `Interpreter`, but checks the top-level *return value* (via
  * `GLOBAL_RESULT`, same as `esmeta.phase.WjiInterp.callAO`) rather than
  * reachability of a poison marker — what matters here is which value comes
  * out, not whether some line ran at all.
  *
  * Most scenario algorithms below are built so their own call site of interest
  * either (a) is genuinely unguarded/unmarked — the exact shape
  * `PropagateUnguardedCallsPass` targets — or (b) manually absorbs the result
  * itself (reading `.[[Type]]`/`.[[Value]]` directly, or the "if this throws,
  * catch it" idiom) so their own `Return`s stay un-wrapped by
  * `WrapCompletionReturnsPass` and can be compared directly against a plain
  * `Value`. [[explode]]/[[callExplodeUnguarded]] are the two algorithms
  * expected to themselves become `completionAlgos`-classified (that's the
  * behavior under test) — the last scenario below invokes them directly and
  * inspects the raw `CompletionRecord` shape on the heap; every other scenario
  * instead puts a small absorbing "driver"/self-check on top to get a
  * directly-comparable plain value back out.
  *
  * Written *before* the `PropagateUnguardedCallsPass` reorder it's meant to
  * guard, and run once against the pre-reorder pipeline to record a baseline —
  * see git history for which scenarios passed/failed before that change.
  */
class CompletionPropagationSpec extends AnyFunSuite:

  /** Returns a bare `ThrowCompletion` directly, bypassing `Instr.Throw`'s own
    * `CompletionWrapping`-driven expansion (which calls `__NEW_ERROR_OBJ__` to
    * build a real `%TypeError.prototype%`-based error object — needing a fully
    * bootstrapped `@EXECUTION_STACK`/`Realm`, unnecessary machinery for what
    * this file needs). `ThrowCompletion` itself (ECMA-262 6.2.4.2: `Return
    * Completion Record { [[Type]]: throw, [[Value]]: argument, [[Target]]:
    * empty }`) needs no Realm access at all. Not itself
    * `completionAlgos`-classified (no `Throw`/`?`/`!`/`Expr.New` of its own) —
    * its `Return` is never wrapped, so it hands back the raw completion record
    * as-is.
    *
    * As spec prose:
    * {{{
    * To <dfn>make throw completion</dfn> perform the following steps:
    *   1. Let |comp| be ThrowCompletion("boom").
    *   2. Return |comp|.
    * }}}
    */
  private val makeThrowCompletion = Algorithm(
    id = Some("makeThrowCompletion"),
    name = Some("makeThrowCompletion"),
    params = Nil,
    head = "",
    body = List(
      Instr.Perform(
        "ThrowCompletion",
        List(Expr.Str("boom")),
        Instr.PerformOutcome.BindResult("comp"),
      ),
      Instr.Return(Some(Expr.Var("comp"))),
    ),
  )

  /** A completion-returning algorithm (via its own explicit `?`-marked call to
    * [[makeThrowCompletion]], so `MarkCompletionAlgorithmsPass` puts it in
    * `completionAlgos`): abruptly completes when `flag` is true, otherwise
    * returns `true`.
    *
    * As spec prose:
    * {{{
    * To <dfn>explode</dfn> given |flag|, perform the following steps:
    *   1. If |flag| is true, then
    *     1. Let |unused| be ? [=make throw completion=]().
    *   2. Return true.
    * }}}
    */
  private val explode = Algorithm(
    id = Some("explode"),
    name = Some("explode"),
    params = List(WjiParam("|flag|")),
    head = "",
    body = List(
      Instr.If(
        Cond.Eq(Expr.Var("flag"), Expr.Bool(true)),
        List(
          Instr.Let(
            Expr.Var("unused"),
            Expr.Abrupt("?", Expr.AlgoCall("[=makeThrowCompletion=]", Nil)),
          ),
        ),
      ),
      Instr.Return(Some(Expr.Bool(true))),
    ),
  )

  // ---- Scenario 1: implicit propagation through a genuinely unguarded call ----

  /** Calls [[explode]] with no `?`/`!` marker and no manual handling — the call
    * site `PropagateUnguardedCallsPass` exists for. This algorithm itself
    * becomes `completionAlgos`-classified as a result (it transitively inherits
    * [[explode]]'s abruptness), so its own return is itself a Completion Record
    * — see [[runImplicitPropagation]] for the driver that unwraps it back to a
    * plain value (and the last test below, which invokes this one directly and
    * checks the raw Completion Record shape instead).
    *
    * As spec prose:
    * {{{
    * To <dfn>call explode unguarded</dfn> given |flag|, perform the
    * following steps:
    *   1. Let |result| be [=explode=](|flag|).
    *   2. Return "reached-end".
    * }}}
    */
  private val callExplodeUnguarded = Algorithm(
    id = Some("callExplodeUnguarded"),
    name = Some("callExplodeUnguarded"),
    params = List(WjiParam("|flag|")),
    head = "",
    body = List(
      Instr.Let(
        Expr.Var("result"),
        Expr.AlgoCall("[=explode=]", List(Expr.Var("flag"))),
      ),
      Instr.Return(Some(Expr.Str("reached-end"))),
    ),
  )

  /** Manually absorbs whatever [[callExplodeUnguarded]] returns (reading
    * `.[[Type]]` right after the call, so this driver itself stays OUT of
    * `completionAlgos`) and returns a plain, directly-comparable marker.
    *
    * As spec prose:
    * {{{
    * To <dfn>run implicit propagation</dfn> given |flag|, perform the
    * following steps:
    *   1. Let |outcome| be [=call explode unguarded=](|flag|).
    *   2. If |outcome|.[[Type]] is throw, then
    *     1. Return "threw".
    *   3. Return |outcome|.[[Value]].
    * }}}
    */
  private val runImplicitPropagation = Algorithm(
    id = Some("runImplicitPropagation"),
    name = Some("runImplicitPropagation"),
    params = List(WjiParam("|flag|")),
    head = "",
    body = List(
      Instr.Let(
        Expr.Var("outcome"),
        Expr.AlgoCall("[=callExplodeUnguarded=]", List(Expr.Var("flag"))),
      ),
      Instr.If(
        Cond
          .Eq(Expr.Field(Expr.Var("outcome"), "Type"), Expr.SpecTerm("throw")),
        List(Instr.Return(Some(Expr.Str("threw")))),
      ),
      Instr.Return(Some(Expr.Field(Expr.Var("outcome"), "Value"))),
    ),
  )

  // ---- Scenario 2: unguarded call already manually handled ----

  /** Calls [[explode]] with no marker, but manually inspects `.[[Type]]` itself
    * before ever reading `.[[Value]]` — the shape
    * `isAbsorbed`/`mentionsTypeField` must recognize as "already handled" so no
    * second, machine-inserted guard gets stacked on top and double-unwraps
    * `result` (exactly the bug found and fixed earlier this session for
    * `OrdinaryObjectCreate`-shaped calls). Stays out of `completionAlgos`
    * itself (the manual check absorbs the only abrupt-capable call), so its own
    * return is directly comparable with no driver needed.
    *
    * As spec prose:
    * {{{
    * To <dfn>call explode manually handled</dfn> given |flag|, perform the
    * following steps:
    *   1. Let |result| be [=explode=](|flag|).
    *   2. If |result|.[[Type]] is throw, then
    *     1. Return "threw".
    *   3. Return |result|.[[Value]].
    * }}}
    */
  private val callExplodeManuallyHandled = Algorithm(
    id = Some("callExplodeManuallyHandled"),
    name = Some("callExplodeManuallyHandled"),
    params = List(WjiParam("|flag|")),
    head = "",
    body = List(
      Instr.Let(
        Expr.Var("result"),
        Expr.AlgoCall("[=explode=]", List(Expr.Var("flag"))),
      ),
      Instr.If(
        Cond.Eq(Expr.Field(Expr.Var("result"), "Type"), Expr.SpecTerm("throw")),
        List(Instr.Return(Some(Expr.Str("threw")))),
      ),
      Instr.Return(Some(Expr.Field(Expr.Var("result"), "Value"))),
    ),
  )

  // ---- Scenario 3: unguarded call already absorbed by the "if this throws" idiom ----

  /** Calls [[explode]] with no marker, immediately followed by the "if this
    * throws an exception, catch it" idiom (`Cond.Throws`) — the shape
    * `ExpandThrowsPass` itself expands. Built as raw `Instr.If`/`Let`+
    * `AlgoCall` (not pre-grouped/pre-extracted) so it goes through
    * `GroupIfChainPass`/`ExtractInlineAlgoCallPass` exactly the way real
    * spec-parsed "Let |result| be explode(flag). If this throws an exception,
    * catch it, ..." text would — this matters because
    * `MarkCompletionAlgorithmsPass`'s classification runs on the tree in this
    * *raw*, pre-grouping shape, and `isAbsorbed`'s existing raw-`Instr.If`
    * recognition (the one call site of `isAbsorbed` this whole investigation
    * confirmed is NOT dead code) depends on seeing it that way. `isAbsorbed`
    * must ALSO recognize this idiom correctly at whatever point
    * `PropagateUnguardedCallsPass` itself runs, so no redundant guard gets
    * inserted next to `ExpandThrowsPass`'s own handling — this is the one
    * scenario this whole refactor's "KNOWN GAP" investigation centers on.
    *
    * As spec prose:
    * {{{
    * To <dfn>explode with catch</dfn> given |flag|, perform the following
    * steps:
    *   1. Let |result| be [=explode=](|flag|).
    *   2. If this throws an exception, catch it, and return "caught".
    *   3. Return |result|.
    * }}}
    */
  private val explodeWithCatch = Algorithm(
    id = Some("explodeWithCatch"),
    name = Some("explodeWithCatch"),
    params = List(WjiParam("|flag|")),
    head = "",
    body = List(
      Instr.Let(
        Expr.Var("result"),
        Expr.AlgoCall("[=explode=]", List(Expr.Var("flag"))),
      ),
      Instr.If(Cond.Throws(None), List(Instr.Return(Some(Expr.Str("caught"))))),
      Instr.Return(Some(Expr.Var("result"))),
    ),
  )

  // ---- Scenario 4: unguarded call into a non-completion-returning algorithm ----

  /** Not a `completionAlgos` member at all (no `Throw`/`?`/`!`/`Expr.New`
    * anywhere in its body).
    *
    * As spec prose:
    * {{{
    * To <dfn>harmless</dfn> perform the following steps:
    *   1. Return true.
    * }}}
    */
  private val harmless = Algorithm(
    id = Some("harmless"),
    name = Some("harmless"),
    params = Nil,
    head = "",
    body = List(Instr.Return(Some(Expr.Bool(true)))),
  )

  /** Calls [[harmless]] with no marker — must get no guard at all, not even a
    * 2-way one, and the raw value must flow straight through.
    *
    * As spec prose:
    * {{{
    * To <dfn>call harmless unguarded</dfn> perform the following steps:
    *   1. Let |result| be [=harmless=]().
    *   2. Return |result|.
    * }}}
    */
  private val callHarmlessUnguarded = Algorithm(
    id = Some("callHarmlessUnguarded"),
    name = Some("callHarmlessUnguarded"),
    params = Nil,
    head = "",
    body = List(
      Instr.Let(Expr.Var("result"), Expr.AlgoCall("[=harmless=]", Nil)),
      Instr.Return(Some(Expr.Var("result"))),
    ),
  )

  /** All scenario algorithms declared above, merged into the SAME shared
    * mainline CFG every `esmeta.es`/`esmeta.ir` test already uses
    * (`ESMetaTest.cfg`) — mirrors `WjiTest.mergedCfg` exactly, substituting
    * this file's own hand-built algorithms for the real extracted spec. Unlike
    * `LoweringBehaviorSpec` (whose fixtures deliberately call nothing outside
    * themselves, so a standalone `CFGBuilder(Compiler.compile(...))` is
    * enough), every completion-returning algorithm here goes through
    * `WrapCompletionReturnsPass`, which calls the real
    * `ThrowCompletion`/`NormalCompletion` bridge functions — those only exist
    * in the real mainline CFG, so this merge is required, not optional. Built
    * once and reused across every test in this file; function-name collisions
    * aren't a concern since every algorithm here has a distinct, made-up name.
    */
  private lazy val cfg: CFG =
    val allAlgos = List(
      makeThrowCompletion,
      explode,
      callExplodeUnguarded,
      runImplicitPropagation,
      callExplodeManuallyHandled,
      explodeWithCatch,
      harmless,
      callHarmlessUnguarded,
    )
    val wjiProgram = Compiler.compile(Lowering.run(allAlgos))
    val mainline = ESMetaTest.cfg.program
    CFGBuilder(Program(mainline.funcs ++ wjiProgram.funcs, mainline.spec))

  /** Invokes `fname` directly with `args` and returns `(State, top-level return
    * value)` via `GLOBAL_RESULT` — mirrors `esmeta.phase.WjiInterp.callAO`'s
    * call-construction (fresh `Context`, empty `callStack`), minus the
    * `CompletionRecord` unwrapping it also does (most scenarios above are
    * deliberately built so their own top-level return is already a plain value
    * — see class doc; the last scenario below is the exception, and inspects
    * the raw completion record on `st.heap` directly instead). No
    * `Realm`/`@EXECUTION_STACK` bootstrap needed — see
    * [[makeThrowCompletion]]'s doc for why every algorithm here deliberately
    * avoids anything that would require one.
    */
  private def invokeRaw(fname: String, args: List[Value]): (State, Value) =
    val f = cfg.getFunc(fname)
    val st = State(cfg, Context(f, MMap.from(f.params.map(_.lhs).zip(args))))
    EsInterpreter(st)
    (st, st.globals.getOrElse(GLOBAL_RESULT, Undef))

  private def invoke(fname: String, args: List[Value]): Value =
    invokeRaw(fname, args)._2

  /** Resolves `v` as a heap `Addr` pointing to a genuine `CompletionRecord`
    * `RecordObj` and returns its `(Type name, Value field)` — fails loudly (not
    * silently) if `v` isn't shaped that way at all, since that itself would
    * mean completion-wrapping didn't happen where it should have.
    */
  private def completionFields(st: State, v: Value): (String, Value) =
    v match
      case addr: Addr =>
        st.heap(addr) match
          case r: RecordObj if r.tname == "CompletionRecord" =>
            val ty = r.get("Type") match
              case Some(Enum(name)) => name
              case other            => fail(s"unexpected Type field: $other")
            (ty, r.get("Value").getOrElse(Undef))
          case other => fail(s"expected a CompletionRecord, got: $other")
      case other => fail(s"expected an Addr (heap object), got: $other")

  test(
    "implicit propagation: an unguarded call's abrupt completion must propagate out, not fall through",
  ) {
    assert(invoke("runimplicitpropagation", List(Bool(true))) == Str("threw"))
    assert(
      invoke("runimplicitpropagation", List(Bool(false))) == Str("reached-end"),
    )
  }

  test(
    "manual .[[Type]] handling: no machine-inserted guard should double-unwrap the result",
  ) {
    assert(
      invoke("callexplodemanuallyhandled", List(Bool(true))) == Str("threw"),
    )
    assert(
      invoke("callexplodemanuallyhandled", List(Bool(false))) == Bool(true),
    )
  }

  test(
    "if-this-throws-catch-it idiom: no machine-inserted guard should duplicate or interfere with ExpandThrowsPass's own handling",
  ) {
    assert(invoke("explodewithcatch", List(Bool(true))) == Str("caught"))
    assert(invoke("explodewithcatch", List(Bool(false))) == Bool(true))
  }

  test("no guard at all for a call into a non-completion-returning algorithm") {
    assert(invoke("callharmlessunguarded", Nil) == Bool(true))
  }

  test(
    "explode and callExplodeUnguarded, called directly, actually return real Completion Records",
  ) {
    val (st1t, v1t) = invokeRaw("explode", List(Bool(true)))
    assert(completionFields(st1t, v1t)._1 == "throw")

    val (st1f, v1f) = invokeRaw("explode", List(Bool(false)))
    assert(completionFields(st1f, v1f) == ("normal", Bool(true)))

    val (st2t, v2t) = invokeRaw("callexplodeunguarded", List(Bool(true)))
    // the abrupt completion propagates through unchanged from explode itself
    assert(completionFields(st2t, v2t)._1 == "throw")

    val (st2f, v2f) = invokeRaw("callexplodeunguarded", List(Bool(false)))
    assert(completionFields(st2f, v2f) == ("normal", Str("reached-end")))
  }
