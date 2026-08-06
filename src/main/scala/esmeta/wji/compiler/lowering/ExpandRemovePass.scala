package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}
import esmeta.wji.lang.Expr.*

/** Expands `Instr.Remove(list, elemKind, property, body)` — webidl/index.bs's
  * `[=list/Remove=] from |list| all the [=elemKind=] that are [=property=].`
  * idiom (category I-O) — into an explicit filter that removes matching
  * elements from `list` itself in place (via `Instr.Pop`/`Instr.Append`),
  * rather than building a separate "kept" list and reassigning it via `Set`.
  *
  * {{{
  *   Remove(list, elemKind, property, body)
  * }}}
  * becomes a two-phase pop/append drain:
  * {{{
  *   // phase 1: drain list into a scratch list, tmp — tmp ends up holding
  *   // list's original elements in reverse order, list ends up empty.
  *   Let(_removeTmp, «»)
  *   Let(_removeLen, Length(list))
  *   Let(_removeIdx, 0)
  *   While(_removeIdx < _removeLen,
  *     Pop(_removeElem, list)
  *     Append(_removeElem, _removeTmp)
  *     Set(_removeIdx, _removeIdx + 1)
  *   )
  *   // phase 2: drain tmp back into list, keeping only non-matching
  *   // elements — reverses a second time, so surviving elements land back
  *   // in list in their original relative order.
  *   Let(_removeTmpLen, Length(_removeTmp))
  *   Let(_removeIdx2, 0)
  *   While(_removeIdx2 < _removeTmpLen,
  *     Pop(_removeElem, _removeTmp)
  *     Assert(_removeElem is a/an <singularized elemKind>)
  *     IfChain([(_removeElem.<Capitalized property> is true,
  *               [Append(_removeElem, list)])], [])
  *     Set(_removeIdx2, _removeIdx2 + 1)
  *   )
  *   ...body...
  * }}}
  *
  * Two phases are unavoidable given `Instr.Pop` only ever removes the list's
  * *last* element and `Instr.Append` only ever adds to the back: a single
  * pop-then-conditionally-append pass over `list` itself would visit elements
  * back-to-front and put survivors right back where they were popped from,
  * i.e. reverse their relative order. Draining fully into `tmp` first (an
  * unconditional pop/append transfer) reverses once; draining `tmp` back into
  * `list`, filtering as it goes, reverses a second time — the two reversals
  * cancel out, so surviving elements come out in their original order. `tmp` is
  * pure scratch space, discarded once this expansion is done — `list` itself is
  * never rebuilt or reassigned, only popped from and appended to.
  *
  * `elemKind` (e.g. "operations") becomes a real per-element `Cond.IsType`
  * assertion (singularized by stripping a trailing "s" — a simple heuristic,
  * good enough for every WJI-reachable use of this idiom so far); `property`
  * (e.g. "unforgeable") becomes a boolean field read, capitalized to match
  * WJI's own record-field naming convention. Both are genuinely evaluated
  * against whatever record the list's elements actually are at runtime — see
  * `esmeta.wji.Initialize`, which is what populates that field (always `false`
  * for every `Operation` it constructs, since WebAssembly's own operations
  * never declare `[Unforgeable]`).
  *
  * Category: Structural desugaring — Elimination.
  */
object ExpandRemovePass extends LoweringPass:

  /** Requires:
    *   - [[GroupIfChainPass]]: this pass emits a ready-made `Instr.IfChain`
    *     directly (mirroring [[ExpandHasDuplicatesPass]]'s own precedent),
    *     rather than a bare `If` for a not-yet-run `GroupIfChainPass` to fold —
    *     it runs among the eliminations, after `GroupIfChainPass`'s single pass
    *     over the original (pre-lowering) `If`/`ElseIf`/`Else` siblings.
    */
  override def requires: Set[LoweringPass] = Set(GroupIfChainPass)

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map { a =>
      a.copy(body = transform(a.body))
    }

  private def transform(instrs: List[Instr]): List[Instr] =
    instrs.flatMap(expandInstr)

  private def expandInstr(instr: Instr): List[Instr] = instr match
    case Instr.Remove(list, elemKind, "unforgeable", body) =>
      val elem = Expr.Var("_removeElem")
      val tmp = Expr.Var("_removeTmp")
      val len = Expr.Var("_removeLen")
      val idx = Expr.Var("_removeIdx")
      val tmpLen = Expr.Var("_removeTmpLen")
      val idx2 = Expr.Var("_removeIdx2")
      List(
        // phase 1: list -> tmp (unconditional transfer, reverses once)
        Instr.Let(tmp, Expr.List_(Nil)),
        Instr.Let(len, Expr.Length(list)),
        Instr.Let(idx, Expr.Num("0")),
        Instr.While(
          Cond.Compare(idx, Cond.CompareOp.Lt, len),
          List(
            Instr.Pop(elem, list),
            Instr.Append(elem, tmp),
            Instr.Set(idx, Expr.BinOp(idx, Expr.BOp.Add, Expr.Num("1"))),
          ),
        ),
        // phase 2: tmp -> list (filtered transfer, reverses again)
        Instr.Let(tmpLen, Expr.Length(tmp)),
        Instr.Let(idx2, Expr.Num("0")),
        Instr.While(
          Cond.Compare(idx2, Cond.CompareOp.Lt, tmpLen),
          List(
            Instr.Pop(elem, tmp),
            Instr.Assert(Cond.IsType(elem, singularize(elemKind))),
            Instr.IfChain(
              List(
                (
                  Cond.And(
                    Cond.HasField(
                      Expr.Field(
                        Expr.Field(elem, "extendedAttributes"),
                        "LegacyUnforgeable",
                      ),
                    ),
                    Cond.Eq(
                      Expr.Field(
                        Expr.Field(elem, "extendedAttributes"),
                        "LegacyUnforgeable",
                      ),
                      Expr.Bool(true),
                    ),
                  ),
                  List(Instr.Append(elem, list)),
                ),
              ),
              Nil,
            ),
            Instr.Set(idx2, Expr.BinOp(idx2, Expr.BOp.Add, Expr.Num("1"))),
          ),
        ),
      ) ::: transform(body)
    case _ => List(instr.mapBody(transform))

  private def singularize(s: String): String =
    if s.endsWith("s") then s.dropRight(1) else s
