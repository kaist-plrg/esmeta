package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}

/** Expands `Expr.Abrupt("?", inner)` — the spec's `?` (ReturnIfAbrupt) operator
  * — into explicit completion checks.
  *
  * For example:
  * {{{
  *   Let(x, Abrupt("?", call), body)
  * }}}
  * becomes:
  * {{{
  *   Let(_compN, call)
  *   IfChain([(IsType(_compN, "AbruptCompletion"), [Return(_compN)])], fallback=[])
  *   Let(x, Field(_compN, "Value"), body)
  * }}}
  *
  * Handles `?` in the direct-RHS position of `Let`, `Set`, and `Return`. Nested
  * occurrences (e.g. inside a larger expression) are left as-is.
  */
object ExpandAbruptPass extends LoweringPass:
  private var counter = 0
  private def freshComp(): String = { counter += 1; s"_comp$counter" }

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = transform(a.body)))

  private def transform(instrs: List[Instr]): List[Instr] =
    instrs.flatMap(expandInstr)

  private def expandInstr(instr: Instr): List[Instr] = instr match

    case Instr.Let(lhs, Expr.Abrupt("?", inner), body) =>
      val tmp = freshComp()
      List(
        Instr.Let(Expr.Var(tmp), inner),
        abruptCheck(tmp),
        Instr.Let(lhs, Expr.Field(Expr.Var(tmp), "Value"), transform(body)),
      )

    case Instr.Set(lhs, Expr.Abrupt("?", inner), body) =>
      val tmp = freshComp()
      List(
        Instr.Let(Expr.Var(tmp), inner),
        abruptCheck(tmp),
        Instr.Set(lhs, Expr.Field(Expr.Var(tmp), "Value"), transform(body)),
      )

    case Instr.Return(Some(Expr.Abrupt("?", inner)), body) =>
      val tmp = freshComp()
      List(
        Instr.Let(Expr.Var(tmp), inner),
        abruptCheck(tmp),
        Instr.Return(Some(Expr.Field(Expr.Var(tmp), "Value")), transform(body)),
      )

    case _ =>
      List(instr.mapBody(transform))

  private def abruptCheck(tmp: String): Instr.IfChain =
    Instr.IfChain(
      branches = List(
        (
          Cond.IsType(Expr.Var(tmp), "AbruptCompletion"),
          List(Instr.Return(Some(Expr.Var(tmp)))),
        ),
      ),
      fallback = Nil,
    )
