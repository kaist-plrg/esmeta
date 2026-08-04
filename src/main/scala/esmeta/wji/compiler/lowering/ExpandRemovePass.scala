package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}

/** Expands `Instr.Remove(list, elemKind, property, body)` — webidl/index.bs's
  * `[=list/Remove=] from |list| all the [=elemKind=] that are [=property=].`
  * idiom (category I-O) — into an explicit filter loop:
  *
  * {{{
  *   Remove(list, elemKind, property, body)
  * }}}
  * becomes
  * {{{
  *   Let(_removeKeptN, «»)
  *   ForEach(_removeElemN, list,
  *     Assert(_removeElemN is a/an <singularized elemKind>)
  *     IfChain([(_removeElemN.<Capitalized property> is not true,
  *               [Append(_removeElemN, _removeKeptN)])], [])
  *   )
  *   Set(list, _removeKeptN)
  *   ...body...
  * }}}
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

  private var counter = 0
  private def freshKept(): String = { counter += 1; s"_removeKept$counter" }
  private def freshElem(): String = { counter += 1; s"_removeElem$counter" }

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map { a =>
      counter = 0
      a.copy(body = transform(a.body))
    }

  private def transform(instrs: List[Instr]): List[Instr] =
    instrs.flatMap(expandInstr)

  private def expandInstr(instr: Instr): List[Instr] = instr match
    case Instr.Remove(list, elemKind, property, body) =>
      val kept = Expr.Var(freshKept())
      val elem = Expr.Var(freshElem())
      List(
        Instr.Let(kept, Expr.List_(Nil)),
        Instr.ForEach(
          elem,
          list,
          List(
            Instr.Assert(Cond.IsType(elem, singularize(elemKind))),
            Instr.IfChain(
              List(
                (
                  Cond.Eq(
                    Expr.Field(elem, property),
                    Expr.Bool(true),
                    negated = true,
                  ),
                  List(Instr.Append(elem, kept)),
                ),
              ),
              Nil,
            ),
          ),
        ),
        Instr.Set(list, kept),
      ) ::: transform(body)
    case _ => List(instr.mapBody(transform))

  private def singularize(s: String): String =
    if s.endsWith("s") then s.dropRight(1) else s
