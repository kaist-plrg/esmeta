package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}
import esmeta.error.UnsupportedSpecShape

/** Expands `Expr.Length(Expr.ShortestArgumentList(list))` into an explicit
  * running-minimum loop over `list`'s entries' `typeList` field lengths,
  * mirroring how [[ExpandIndexOfPass]] expands `IndexOf`:
  *
  * {{{
  *   Let(target, Length(ShortestArgumentList(list)), body)
  * }}}
  * becomes:
  * {{{
  *   Let(_shortLenN, Length(list[0].typeList))
  *   Let(_shortIN, 1)
  *   While(_shortIN < Length(list),
  *     IfChain([(Length(list[_shortIN].typeList) < _shortLenN,
  *               [Set(_shortLenN, Length(list[_shortIN].typeList))])], [])
  *     Set(_shortIN, _shortIN + 1))
  *   Let(target, _shortLenN)
  *   ...body...
  * }}}
  * Never materializes the winning entry (or its `typeList`) itself — only the
  * running-minimum length is tracked, which is all the one real occurrence of
  * this shape (webidl/index.bs:12584, wrapped in an outer `Length`) needs. Uses
  * `Instr.IfChain` (not a raw `Instr.If`) for the inner comparison, matching
  * [[ExpandHasDuplicatesPass]]'s own convention for passes that run after
  * `GroupIfChainPass` in the pipeline.
  *
  * `list[i].typeList` reads the `typeList` field
  * `manuals/funcs/compute_the_effective_overload_set.ir` (the manual IR stub
  * behind `compute the effective overload set`, webidl/index.bs:3179-3256)
  * actually produces on each entry of an effective overload set — WJI's own
  * chosen runtime representation for that tuple, the same kind of fact
  * [[ExpandWjiIsTypePass]] already documents itself as relying on.
  *
  * Only handles `Length(ShortestArgumentList(list))` in direct `Let` RHS
  * position — the only shape observed in practice (webidl/index.bs:12584,
  * `creating an operation function`). If `ShortestArgumentList` ever turns up
  * nested elsewhere, `run` throws `UnsupportedSpecShape` rather than silently
  * leaving it for `Compiler`'s much later, less specific `impossible` fallback.
  *
  * Category: Structural desugaring — Elimination.
  */
object ExpandShortestArgumentListPass extends LoweringPass:

  /** Generates this pass's `_shortLenN`/`_shortIN` names for a single
    * algorithm. Scoped as a value local to each [[run]] iteration rather than a
    * mutable field on this `object` — the latter is JVM-wide singleton state,
    * so concurrent `run` calls (e.g. multiple ScalaTest suites compiling
    * algorithms in parallel, which sbt's default `Test / parallelExecution`
    * allows) would race on incrementing/resetting a shared counter, producing
    * nondeterministic naming depending on thread interleaving.
    */
  private class Counter:
    private var n = 0
    def freshLen(): String = { n += 1; s"_shortLen$n" }
    def freshIdx(): String = { n += 1; s"_shortI$n" }

  def run(algos: List[Algorithm]): List[Algorithm] =
    val result = algos.map { a =>
      val counter = Counter()
      a.copy(body = transform(a.body, counter))
    }
    result.foreach { a =>
      if AstQuery.existsExpr(a.body)(
          _.isInstanceOf[Expr.ShortestArgumentList],
        )
      then
        throw UnsupportedSpecShape(
          "ExpandShortestArgumentListPass",
          s"ShortestArgumentList outside Length(...) in direct Let RHS position, in ${a.name
            .orElse(a.id)}",
        )
    }
    result

  private def transform(instrs: List[Instr], counter: Counter): List[Instr] =
    instrs.flatMap(expandInstr(_, counter))

  private def typeListLenAt(list: Expr, idx: Expr): Expr =
    Expr.Length(Expr.Field(Expr.Index(list, idx), "typeList"))

  private def expandInstr(instr: Instr, counter: Counter): List[Instr] =
    instr match
      case Instr.Let(
            target @ Expr.Var(_),
            Expr.Length(Expr.ShortestArgumentList(list)),
            body,
          ) =>
        val lenVar = Expr.Var(counter.freshLen())
        val idxVar = Expr.Var(counter.freshIdx())
        List(
          Instr.Let(lenVar, typeListLenAt(list, Expr.Num("0"))),
          Instr.Let(idxVar, Expr.Num("1")),
          Instr.While(
            Cond.Compare(idxVar, Cond.CompareOp.Lt, Expr.Length(list)),
            List(
              Instr.IfChain(
                List(
                  (
                    Cond.Compare(
                      typeListLenAt(list, idxVar),
                      Cond.CompareOp.Lt,
                      lenVar,
                    ),
                    List(Instr.Set(lenVar, typeListLenAt(list, idxVar))),
                  ),
                ),
                Nil,
              ),
              Instr.Set(idxVar, Expr.BinOp(idxVar, Expr.BOp.Add, Expr.Num("1"))),
            ),
          ),
          Instr.Let(target, lenVar),
        ) ::: transform(body, counter)

      case _ =>
        List(instr.mapBody(transform(_, counter)))
