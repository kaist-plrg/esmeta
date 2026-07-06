package esmeta.wji.compiler.desugar

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}

/** Expands `Instr.ForEach(elem, collection, body)` into an index-based while
  * loop, mirroring how `esmeta.compiler.Compiler`'s `ForEachStep` compiles
  * ECMA-262's own "for each" steps:
  *
  * {{{
  *   ForEach(Var(x), collection, body)
  * }}}
  * becomes:
  * {{{
  *   Let(_iterListN, collection)
  *   Let(_iterIdxN, 0)
  *   While(_iterIdxN < length(_iterListN),
  *     Let(x, _iterListN[_iterIdxN])
  *     ...body...
  *     Set(_iterIdxN, _iterIdxN + 1))
  * }}}
  *
  * `elem` may also be a `Tuple` of `Var`s (destructuring, e.g. `For each (|a|,
  * |b|) of ...`); this pass still binds one element per iteration, and
  * [[ExpandDestructuringLetPass]] (which must run after this pass) then
  * destructures that `Let(Tuple(...), ...)` into individual bindings.
  *
  * Only handles `elem` shapes of `Var` or `Tuple` of `Var`s. Other forms seen
  * in the spec — parallel iteration over two lists ("`X` and `Y` of `A` and
  * `B`"), map iteration ("`k` &rarr; `v` in `map`"), or a typed qualifier
  * ("`[=custom section=] x` of `y`") — parse to something other than a bare
  * `Var`/`Tuple` and are left as `ForEach` for a future pass, so the compiler
  * still reports them as unsupported instead of silently mis-compiling.
  */
object ExpandForEachPass extends DesugarPass:
  private var counter = 0
  private def freshList(): String = { counter += 1; s"_iterList$counter" }
  private def freshIdx(): String = { counter += 1; s"_iterIdx$counter" }

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = transform(a.body)))

  private def transform(instrs: List[Instr]): List[Instr] =
    instrs.flatMap(expandInstr)

  private def expandInstr(instr: Instr): List[Instr] = instr match
    case Instr.ForEach(elem, collection, body) if isBindable(elem) =>
      val listVar = Expr.Var(freshList())
      val idxVar = Expr.Var(freshIdx())
      List(
        Instr.Let(listVar, collection),
        Instr.Let(idxVar, Expr.Num("0")),
        Instr.While(
          Cond.Compare(idxVar, "<", Expr.Length(listVar)),
          Instr.Let(elem, Expr.Index(listVar, idxVar)) ::
          transform(body) :::
          List(Instr.Set(idxVar, Expr.BinOp(idxVar, "+", Expr.Num("1")))),
        ),
      )
    case _ =>
      List(instr.mapBody(transform))

  private def isBindable(elem: Expr): Boolean = elem match
    case Expr.Var(_) => true
    case Expr.Tuple(elems) =>
      elems.forall(_.isInstanceOf[Expr.Var])
    case _ => false
