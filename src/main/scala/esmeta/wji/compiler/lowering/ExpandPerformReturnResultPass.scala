package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Expr, Instr}
import esmeta.wji.lang.Instr.PerformOutcome

/** Expands `Perform(..., ReturnResult, body)` — the "perform X and return the
  * result" pattern — into an explicit bind + return.
  *
  * {{{
  *   Perform(func, args, ReturnResult, body)
  * }}}
  * becomes:
  * {{{
  *   <body>
  *   Perform(func, args, BindResult("_resultN"), [])
  *   Return(Var("_resultN"))
  * }}}
  *
  * This removes the `ReturnResult` outcome from `Instr.Perform` so the compiler
  * only needs to handle `Discard`/`BindResult` there (unlike
  * `Instr.PerformClosure`, whose `ReturnResult` case `Compiler` compiles
  * directly — see its doc — so this pass deliberately doesn't touch it).
  *
  * Category: Structural desugaring — Elimination.
  */
object ExpandPerformReturnResultPass extends LoweringPass:

  /** Requires:
    *   - [[ExpandInlineAlgoCallPass]]: needs a `Return(Some(AlgoCall(...)))`
    *     already converted to `Instr.Perform(..., ReturnResult, ...)` — one
    *     that arrives late (after this pass has already run) would never get
    *     simplified, and would reach `Compiler`'s `impossible()` guard for
    *     `Instr.Perform`'s `ReturnResult` case instead.
    */
  override def requires: Set[LoweringPass] = Set(ExpandInlineAlgoCallPass)

  /** Generates this pass's `_resultN` names for a single algorithm. Scoped as a
    * value local to each [[run]] iteration rather than a mutable field on this
    * `object` — the latter is JVM-wide singleton state, so concurrent `run`
    * calls (e.g. multiple ScalaTest suites compiling algorithms in parallel,
    * which sbt's default `Test / parallelExecution` allows) would race on
    * incrementing/resetting a shared counter, producing nondeterministic naming
    * depending on thread interleaving.
    */
  private class Counter:
    private var n = 0
    def freshRet(): String = { n += 1; s"_result$n" }

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map { a =>
      val counter = Counter()
      a.copy(body = transform(a.body, counter))
    }

  private def transform(instrs: List[Instr], counter: Counter): List[Instr] =
    instrs.flatMap(expandInstr(_, counter))

  private def expandInstr(instr: Instr, counter: Counter): List[Instr] =
    instr match
      case Instr.Perform(func, args, PerformOutcome.ReturnResult, body) =>
        val tmp = counter.freshRet()
        transform(body, counter) ++
        List(
          Instr.Perform(func, args, PerformOutcome.BindResult(tmp)),
          Instr.Return(Some(Expr.Var(tmp))),
        )
      case _ =>
        List(instr.mapBody(transform(_, counter)))
