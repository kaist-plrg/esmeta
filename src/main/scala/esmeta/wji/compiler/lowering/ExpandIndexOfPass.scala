package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}
import esmeta.error.UnsupportedSpecShape

/** Expands `Expr.IndexOf(list, elem)` into an explicit linear-search loop,
  * mirroring how [[ExpandNewByteSequencePass]] expands `NewByteSequence`:
  *
  * {{{
  *   Let(index, IndexOf(list, elem), body)
  * }}}
  * becomes:
  * {{{
  *   Let(_idxOfN, 0)
  *   While(_idxOfN < length(list) and list[_idxOfN] is not elem,
  *     Set(_idxOfN, _idxOfN + 1))
  *   Let(index, AsNumber(_idxOfN))
  *   ...body...
  * }}}
  * `_idxOfN` is a WJI-internal math value (a loop counter); `index` itself
  * leaves this pass and flows into real ECMA-262 AOs (e.g. `name of the
  * WebAssembly function`'s own `[$ToString$](|index|)`), which — unlike
  * list-indexing — actually inspect the spec-level type of their argument, so
  * it's cast to a real Number via `AsNumber` before the final bind rather than
  * left as a bare math value.
  *
  * Only handles `IndexOf` in direct `Let` RHS position — the only one observed
  * in practice ("the index of |moduleinst|.funcaddrs where
  * |funcaddr| is found", index.bs:1255). If one ever turns up nested elsewhere,
  * `run` throws `UnsupportedSpecShape` rather than silently leaving it for
  * `Compiler`'s much later, less specific `EYet` fallback.
  *
  * Category: Structural desugaring — Elimination.
  */
object ExpandIndexOfPass extends LoweringPass:
  private var counter = 0
  private def freshIdx(): String = { counter += 1; s"_idxOf$counter" }

  def run(algos: List[Algorithm]): List[Algorithm] =
    val result = algos.map { a =>
      counter = 0
      a.copy(body = transform(a.body))
    }
    result.foreach { a =>
      if AstQuery.existsExpr(a.body)(_.isInstanceOf[Expr.IndexOf]) then
        throw UnsupportedSpecShape(
          "ExpandIndexOfPass",
          s"IndexOf outside direct Let RHS position, in ${a.name.orElse(a.id)}",
        )
    }
    result

  private def transform(instrs: List[Instr]): List[Instr] =
    instrs.flatMap(expandInstr)

  private def expandInstr(instr: Instr): List[Instr] = instr match
    case Instr.Let(target @ Expr.Var(_), Expr.IndexOf(list, elem), body) =>
      val idxVar = Expr.Var(freshIdx())
      List(
        Instr.Let(idxVar, Expr.Num("0")),
        Instr.While(
          Cond.And(
            Cond.Compare(idxVar, Cond.CompareOp.Lt, Expr.Length(list)),
            Cond.Eq(Expr.Index(list, idxVar), elem, negated = true),
          ),
          List(
            Instr.Set(idxVar, Expr.BinOp(idxVar, Expr.BOp.Add, Expr.Num("1"))),
          ),
        ),
        Instr.Let(target, Expr.AsNumber(idxVar)),
      ) ::: transform(body)

    case _ =>
      List(instr.mapBody(transform))
