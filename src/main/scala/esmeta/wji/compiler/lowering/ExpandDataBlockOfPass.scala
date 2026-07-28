package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}

/** Expands `Expr.DataBlockOf(memaddr)` — "a [=Data Block=] which is
  * [=identified with=] the underlying memory of |memaddr|" — into an
  * explicit `mem_read_bytes` bridge call plus a fill loop copying each byte
  * out into a genuine heap list, mirroring `ExpandNewByteSequencePass`'s
  * allocate-then-fill idiom:
  *
  * {{{
  *   Let(x, DataBlockOf(memaddr), body)
  * }}}
  * becomes:
  * {{{
  *   Let(_wasmBytesN, [$mem_read_bytes$]([=surrounding agent=].[[associated store]], memaddr))
  *   Let(x, «»)
  *   Let(_idxN, 0)
  *   While(_idxN < length(_wasmBytesN),
  *     Append(AsMath(_wasmBytesN[_idxN]), x)
  *     Set(_idxN, _idxN + 1))
  *   ...body...
  * }}}
  *
  * `_wasmBytesN` is an opaque `Wasm(ALValue.ListV(...))` — `Length`/indexing
  * already handle that generically (`ESizeOf`/`State.apply`), but each
  * individual byte still needs `AsMath` to unwrap its own `Wasm(NumV(...))`
  * before it can be `Append`ed into an ordinary WJI list.
  *
  * Only handles `DataBlockOf` in direct `Let` RHS position — the only shape
  * observed in practice (`create a fixed length memory buffer`/
  * `create a resizable memory buffer`/`refresh the Memory buffer`, all
  * `Let |block| be a [=Data Block=] which is [=identified with=] ...`).
  *
  * Category: Structural desugaring.
  */
object ExpandDataBlockOfPass extends LoweringPass:

  override def requires: Set[LoweringPass] = Set.empty
  override def mustPrecede: Set[LoweringPass] = Set(ExtractInlineAlgoCallPass)

  private var counter = 0
  private def freshIdx(): String = { counter += 1; s"_dataBlockIdx$counter" }
  private def freshBytesVar(): String = { counter += 1; s"_wasmBytes$counter" }

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map { a =>
      counter = 0
      a.copy(body = transform(a.body))
    }

  private def transform(instrs: List[Instr]): List[Instr] =
    instrs.flatMap(expandInstr)

  private def expandInstr(instr: Instr): List[Instr] = instr match
    case Instr.Let(target @ Expr.Var(_), Expr.DataBlockOf(memaddr), body) =>
      buildFill(target, memaddr, transform(body))

    case _ =>
      List(instr.mapBody(transform))

  private def buildFill(
    target: Expr,
    memaddr: Expr,
    rest: List[Instr],
  ): List[Instr] =
    val bytesVar = Expr.Var(freshBytesVar())
    val idxVar = Expr.Var(freshIdx())
    val store =
      Expr.Field(Expr.SpecTerm("surrounding agent"), "associated store")
    List(
      Instr.Let(
        bytesVar,
        Expr.AlgoCall("mem_read_bytes", List(store, memaddr)),
      ),
      Instr.Let(target, Expr.List_(Nil)),
      Instr.Let(idxVar, Expr.Num("0")),
      Instr.While(
        Cond.Compare(idxVar, Cond.CompareOp.Lt, Expr.Length(bytesVar)),
        List(
          Instr.Append(Expr.AsMath(Expr.Index(bytesVar, idxVar)), target),
          Instr.Set(idxVar, Expr.BinOp(idxVar, Expr.BOp.Add, Expr.Num("1"))),
        ),
      ),
    ) ::: rest
