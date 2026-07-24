package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Cond, Expr, Instr}

/** Generic read-only queries over an already-(partially-)lowered [[Instr]]
  * tree, shared by lowering passes that only handle a construct in a
  * documented-narrow set of positions and need to check, once they're done,
  * whether an occurrence outside that set slipped through — see
  * [[ExpandNewByteSequencePass]], [[ExpandIndexOfPass]], and
  * [[ExpandFollowingStepsPass]], which throw
  * `esmeta.error.UnsupportedSpecShape` when [[existsExpr]] finds one, rather
  * than silently leaving it for `Compiler`'s own, much later `EYet` fallback.
  */
object AstQuery:

  /** Every direct (non-nested-body) `Expr` field of `instr` — the Exprs a
    * pass's own per-instruction pattern match would see, before recursing into
    * `instr.body`.
    */
  private def ownExprs(instr: Instr): List[Expr] = instr match
    case i: Instr.Let            => List(i.lhs, i.expr)
    case i: Instr.Set            => List(i.lhs, i.expr)
    case i: Instr.Return         => i.expr.toList
    case i: Instr.ForEach        => List(i.elem, i.collection)
    case i: Instr.For            => List(i.elem, i.collection)
    case i: Instr.Append         => List(i.item, i.collection)
    case i: Instr.Perform        => i.args
    case i: Instr.PerformClosure => i.closure :: i.args
    case _                       => Nil

  private def ownConds(instr: Instr): List[Cond] = instr match
    case i: Instr.If                => List(i.cond)
    case i: Instr.ElseIf            => List(i.cond)
    case i: Instr.Assert            => List(i.cond)
    case i: Instr.While             => List(i.cond)
    case Instr.IfChain(branches, _) => branches.map(_._1)
    case _                          => Nil

  /** Every `Expr` a `Cond` itself directly carries (nested sub-`Cond`s like
    * `And`/`Or`/`IsOfForm`'s `cond`/`Exists`'s `body` are walked separately —
    * see [[condChildren]]).
    */
  private def condExprs(cond: Cond): List[Expr] = cond match
    case Cond.Eq(l, r, _)               => List(l, r)
    case Cond.Compare(l, _, r)          => List(l, r)
    case Cond.HasField(e, _)            => List(e)
    case Cond.Implements(e, _, _)       => List(e)
    case Cond.IsOfForm(e, f, _, _)      => List(e, f)
    case Cond.Matches(l, _, r, _)       => List(l, r)
    case Cond.IsMissing(e, _)           => List(e)
    case Cond.HasSlot(e, _, _)          => List(e)
    case Cond.HasDuplicates(e, _)       => List(e)
    case Cond.IsType(e, _, _)           => List(e)
    case Cond.Abbreviated(e)            => List(e)
    case Cond.Exists(_, collections, _) => collections
    case _                              => Nil

  private def condChildren(cond: Cond): List[Cond] = cond match
    case Cond.And(l, r)            => List(l, r)
    case Cond.Or(l, r)             => List(l, r)
    case Cond.IsOfForm(_, _, c, _) => c.toList
    case Cond.Exists(_, _, body)   => List(body)
    case _                         => Nil

  private def allExprsOfCond(cond: Cond): List[Expr] =
    condExprs(cond) ++ condChildren(cond).flatMap(allExprsOfCond)

  /** Whether `pred` holds for any `Expr` reachable from `instrs`, at any depth:
    * every instruction's own direct fields (including inside its `Cond`s), plus
    * every nested instruction body (`IfChain`'s branches/ fallback included,
    * since its `body` is always `Nil` — see [[esmeta.wji.lang.Instr.IfChain]]).
    */
  def existsExpr(instrs: List[Instr])(pred: Expr => Boolean): Boolean =
    instrs.exists { instr =>
      ownExprs(instr).exists(_.containsWhere(pred)) ||
      ownConds(instr).flatMap(allExprsOfCond).exists(_.containsWhere(pred)) ||
      existsExpr(instr.body)(pred) || (instr match
        case Instr.IfChain(branches, fallback) =>
          branches.exists((_, b) => existsExpr(b)(pred)) ||
          existsExpr(fallback)(pred)
        case _ => false
      )
    }
