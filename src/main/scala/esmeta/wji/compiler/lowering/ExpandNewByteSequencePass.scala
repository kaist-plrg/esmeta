package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}
import esmeta.error.UnsupportedSpecShape

/** Expands `Expr.NewByteSequence(length)` into an explicit allocate-then-fill
  * sequence, mirroring how [[ExpandForEachPass]] expands `ForEach` into an
  * index-based while loop:
  *
  * {{{
  *   Let(x, NewByteSequence(length), body)
  * }}}
  * becomes:
  * {{{
  *   Let(x, «»)
  *   Let(_byteIdxN, 0)
  *   While(_byteIdxN < length,
  *     Append(0, x)
  *     Set(_byteIdxN, _byteIdxN + 1))
  *   ...body...
  * }}}
  *
  * and likewise for `Return(Some(NewByteSequence(length)), body)`, using a
  * fresh variable in place of `x` (the loop's steps run after `body`, before
  * the final `Return`).
  *
  * Only handles `NewByteSequence` in these two positions — the only ones
  * observed in practice ("Let |bytes| be a new byte sequence of length equal to
  * ..."). If one ever turns up nested elsewhere, `run` throws
  * `UnsupportedSpecShape` rather than silently leaving it for `Compiler`'s much
  * later, less specific `EYet` fallback.
  *
  * Category: Structural desugaring — Elimination.
  */
object ExpandNewByteSequencePass extends LoweringPass:

  /** Generates this pass's `_byteIdxN`/`_byteSeqN` names for a single
    * algorithm. Scoped as a value local to each [[run]] iteration rather than a
    * mutable field on this `object` — the latter is JVM-wide singleton state,
    * so concurrent `run` calls (e.g. multiple ScalaTest suites compiling
    * algorithms in parallel, which sbt's default `Test / parallelExecution`
    * allows) would race on incrementing/resetting a shared counter, producing
    * nondeterministic naming depending on thread interleaving.
    */
  private class Counter:
    private var n = 0
    def freshIdx(): String = { n += 1; s"_byteIdx$n" }
    def freshVar(): String = { n += 1; s"_byteSeq$n" }

  def run(algos: List[Algorithm]): List[Algorithm] =
    val result = algos.map { a =>
      val counter = Counter()
      a.copy(body = transform(a.body, counter))
    }
    result.foreach { a =>
      if AstQuery.existsExpr(a.body)(_.isInstanceOf[Expr.NewByteSequence]) then
        throw UnsupportedSpecShape(
          "ExpandNewByteSequencePass",
          s"NewByteSequence outside direct Let/Return RHS position, in ${a.name
            .orElse(a.id)}",
        )
    }
    result

  private def transform(instrs: List[Instr], counter: Counter): List[Instr] =
    instrs.flatMap(expandInstr(_, counter))

  private def expandInstr(instr: Instr, counter: Counter): List[Instr] =
    instr match
      case Instr.Let(
            target @ Expr.Var(_),
            Expr.NewByteSequence(length),
            body,
          ) =>
        buildFill(target, length, transform(body, counter), counter)

      case Instr.Return(Some(Expr.NewByteSequence(length)), body) =>
        val tmp = Expr.Var(counter.freshVar())
        transform(body, counter) ::: buildFill(
          tmp,
          length,
          List(Instr.Return(Some(tmp))),
          counter,
        )

      case _ =>
        List(instr.mapBody(transform(_, counter)))

  private def buildFill(
    target: Expr,
    length: Expr,
    rest: List[Instr],
    counter: Counter,
  ): List[Instr] =
    val idxVar = Expr.Var(counter.freshIdx())
    List(
      Instr.Let(target, Expr.List_(Nil)),
      Instr.Let(idxVar, Expr.Num("0")),
      Instr.While(
        Cond.Compare(idxVar, Cond.CompareOp.Lt, length),
        List(
          Instr.Append(Expr.Byte(0), target),
          Instr.Set(idxVar, Expr.BinOp(idxVar, Expr.BOp.Add, Expr.Num("1"))),
        ),
      ),
    ) ::: rest
