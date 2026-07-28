package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Expr, Instr}

/** Expands `Expr.DataBlockOf(memaddr)` — "a [=Data Block=] which is
  * [=identified with=] the underlying memory of |memaddr|" — into an explicit
  * `mem_read_bytes` bridge call:
  *
  * {{{
  *   Let(x, DataBlockOf(memaddr), body)
  * }}}
  * becomes:
  * {{{
  *   Let(x, [$mem_read_bytes$]([=surrounding agent=].[[associated store]], memaddr))
  *   ...body...
  * }}}
  *
  * A one-instruction rewrite, not an allocate-then-fill loop: unlike a generic
  * embedding call, `mem_read_bytes`'s result is materialized natively as a
  * genuine heap `ListObj` by `esmeta.interpreter.Interpreter.callEmbedding`
  * itself (a WJI-specific special case there), rather than left as an opaque
  * `Wasm(ALValue.ListV(...))` needing a WJI-level fill loop to copy out byte by
  * byte. An earlier version of this pass did emit such a loop (allocate `x`,
  * then `While`-index/`AsMath`/`Append` one byte at a time) — for a single Wasm
  * page (64KiB) that loop dominated `memory-mutation.js`'s whole runtime (~70s
  * of an ~83s run). See `personal/debugging-lessons.md`.
  *
  * Only handles `DataBlockOf` in direct `Let` RHS position — the only shape
  * observed in practice (`create a fixed length memory buffer`/ `create a
  * resizable memory buffer`/`refresh the Memory buffer`, all `Let |block| be a
  * [=Data Block=] which is [=identified with=] ...`).
  *
  * Category: Structural desugaring.
  */
object ExpandDataBlockOfPass extends LoweringPass:

  override def requires: Set[LoweringPass] = Set.empty
  override def mustPrecede: Set[LoweringPass] = Set(ExtractInlineAlgoCallPass)

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = transform(a.body)))

  private def transform(instrs: List[Instr]): List[Instr] =
    instrs.flatMap(expandInstr)

  private def expandInstr(instr: Instr): List[Instr] = instr match
    case Instr.Let(target @ Expr.Var(_), Expr.DataBlockOf(memaddr), body) =>
      val store =
        Expr.Field(Expr.SpecTerm("surrounding agent"), "associated store")
      Instr.Let(
        target,
        Expr.AlgoCall("mem_read_bytes", List(store, memaddr)),
      ) :: transform(body)

    case _ =>
      List(instr.mapBody(transform))
