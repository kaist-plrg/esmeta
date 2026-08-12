package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Expr, Instr}

/** Expands `Instr.Let(lhs, Expr.Conditional(cond, thenExpr, elseExpr), body)` —
  * WebIDL's conditional expression idiom parsed by `ExprParser`
  * (webidl_yet_categorized.md category I-G), e.g. `Let(|modifiable|,
  * Conditional(|op| is [=unforgeable=], <emu-val>false</emu-val>,
  * <emu-val>true</emu-val>))` — into a real conditional:
  * {{{
  *   IfChain([(cond, [Let(lhs, thenExpr)])], [Let(lhs, elseExpr)])
  * }}}
  * so `Compiler` never needs to know about `Expr.Conditional` at all — like
  * every other `Expr`/`Cond` node with no IR-level equivalent, it's fully
  * eliminated before compilation, not special-cased inside
  * `Compiler.compileInstr`.
  *
  * Category: Structural desugaring — Elimination.
  */
object ExpandConditionalPass extends LoweringPass:

  /** Requires:
    *   - [[GroupIfChainPass]]: this pass emits a ready-made `Instr.IfChain`
    *     directly, rather than a bare `If`/`Else` for a not-yet-run
    *     `GroupIfChainPass` to fold — mirrors `ExpandRemovePass`'s own
    *     precedent.
    */
  override def requires: Set[LoweringPass] = Set(GroupIfChainPass)

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = transform(a.body)))

  private def transform(instrs: List[Instr]): List[Instr] =
    instrs.flatMap(expandInstr)

  private def expandInstr(instr: Instr): List[Instr] = instr match
    case Instr.Let(lhs, Expr.Conditional(cond, thenExpr, elseExpr), body) =>
      Instr.IfChain(
        List((cond, List(Instr.Let(lhs, thenExpr)))),
        List(Instr.Let(lhs, elseExpr)),
      ) :: transform(body)
    case _ => List(instr.mapBody(transform))
