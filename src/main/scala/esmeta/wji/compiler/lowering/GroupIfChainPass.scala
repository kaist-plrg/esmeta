package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Instr}

/** Converts flat `If`/`ElseIf`/`Else` siblings into [[Instr.IfChain]] nodes.
  *
  * The spec emits these as adjacent numbered-list items; this pass groups them
  * into a proper tree so the compiler no longer needs `collectElseChain`.
  *
  * Category: Structural desugaring.
  */
object GroupIfChainPass extends LoweringPass:

  /** Requires:
    *   - [[DropNotesPass]]: `collectChain` only recognizes an `ElseIf`/`Else`
    *     as a chain continuation when it's the literal next element in the
    *     instruction list — a `Note` step sitting between an `If` and its
    *     following `Else if` would otherwise silently split one real
    *     if/elseif/else chain into disconnected pieces.
    */
  override def requires: Set[LoweringPass] = Set(DropNotesPass)

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = transform(a.body)))

  private def transform(instrs: List[Instr]): List[Instr] =
    instrs match
      case Nil => Nil
      case (i: Instr.If) :: rest =>
        val (elseIfs, maybeElse, remaining) = collectChain(rest)
        val branches = (i.cond, transform(i.body)) ::
          elseIfs.map((c, b) => (c, transform(b)))
        val fallback = maybeElse.map(transform).getOrElse(Nil)
        Instr.IfChain(branches, fallback) :: transform(remaining)
      case (i: Instr.ElseIf) :: rest =>
        // orphan ElseIf — shouldn't happen in well-formed spec, but handle gracefully
        val branches = List((i.cond, transform(i.body)))
        Instr.IfChain(branches, Nil) :: transform(rest)
      case (i: Instr.Else) :: rest =>
        // orphan Else — same
        Instr.IfChain(List.empty, transform(i.body)) :: transform(rest)
      case instr :: rest =>
        instr.mapBody(transform) :: transform(rest)

  private def collectChain(
    instrs: List[Instr],
  ): (
    List[(esmeta.wji.lang.Cond, List[Instr])],
    Option[List[Instr]],
    List[Instr],
  ) =
    instrs match
      case (e: Instr.ElseIf) :: rest =>
        val (moreElseIfs, maybeElse, remaining) = collectChain(rest)
        ((e.cond, e.body) :: moreElseIfs, maybeElse, remaining)
      case (e: Instr.Else) :: rest =>
        (Nil, Some(e.body), rest)
      case _ =>
        (Nil, None, instrs)
