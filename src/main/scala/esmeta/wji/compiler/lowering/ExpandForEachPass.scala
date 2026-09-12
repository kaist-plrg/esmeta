package esmeta.wji.compiler.lowering

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
  * Also expands `Instr.ForEachPaired(elem1, elem2, collection1, collection2,
  * body)` ("For each X and Y of A and B, paired linearly, ...") the same way,
  * but with two `_iterListN`s advanced by one shared index:
  * {{{
  *   Let(_iterList1N, collection1)
  *   Let(_iterList2N, collection2)
  *   Let(_iterIdxN, 0)
  *   While(_iterIdxN < length(_iterList1N),
  *     Let(x, _iterList1N[_iterIdxN])
  *     Let(y, _iterList2N[_iterIdxN])
  *     ...body...
  *     Set(_iterIdxN, _iterIdxN + 1))
  * }}}
  * `collection1`'s length is used as the loop bound; the spec always asserts
  * the two collections' sizes are equal immediately before a "paired linearly"
  * step (e.g. js-api's `Exception` constructor), so either length works.
  *
  * Only handles `elem`/`elem1`/`elem2` shapes of `Var` or `Tuple` of `Var`s.
  * Other forms seen in the spec — map iteration ("`k` &rarr; `v` in `map`"), or
  * a typed qualifier ("`[=custom section=] x` of `y`") — parse to something
  * other than a bare `Var`/`Tuple` and are left as `ForEach` for a future pass,
  * so the compiler still reports them as unsupported instead of silently
  * mis-compiling.
  *
  * Category: Structural desugaring — Elimination.
  */
object ExpandForEachPass extends LoweringPass:

  /** Generates this pass's `_iterListN`/`_iterIdxN` names for a single
    * algorithm. Scoped as a value local to each [[run]] iteration rather than a
    * mutable field on this `object` — the latter is JVM-wide singleton state,
    * so concurrent `run` calls (e.g. multiple ScalaTest suites compiling
    * algorithms in parallel, which sbt's default `Test / parallelExecution`
    * allows) would race on incrementing/resetting a shared counter, producing
    * nondeterministic naming depending on thread interleaving.
    */
  private class Counter:
    private var n = 0
    def freshList(): String = { n += 1; s"_iterList$n" }
    def freshIdx(): String = { n += 1; s"_iterIdx$n" }

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map { a =>
      val counter = Counter()
      a.copy(body = transform(a.body, counter))
    }

  private def transform(instrs: List[Instr], counter: Counter): List[Instr] =
    instrs.flatMap(expandInstr(_, counter))

  private def expandInstr(instr: Instr, counter: Counter): List[Instr] =
    instr match
      case Instr.ForEach(elem, collection, body) if isBindable(elem) =>
        val listVar = Expr.Var(counter.freshList())
        val idxVar = Expr.Var(counter.freshIdx())
        List(
          Instr.Let(listVar, collection),
          Instr.Let(idxVar, Expr.Num("0")),
          Instr.While(
            Cond.Compare(idxVar, Cond.CompareOp.Lt, Expr.Length(listVar)),
            Instr.Let(elem, Expr.Index(listVar, idxVar)) ::
            transform(body, counter) :::
            List(
              Instr.Set(
                idxVar,
                Expr.BinOp(idxVar, Expr.BOp.Add, Expr.Num("1")),
              ),
            ),
          ),
        )
      case Instr.ForEachPaired(elem1, elem2, collection1, collection2, body)
          if isBindable(elem1) && isBindable(elem2) =>
        val listVar1 = Expr.Var(counter.freshList())
        val listVar2 = Expr.Var(counter.freshList())
        val idxVar = Expr.Var(counter.freshIdx())
        List(
          Instr.Let(listVar1, collection1),
          Instr.Let(listVar2, collection2),
          Instr.Let(idxVar, Expr.Num("0")),
          Instr.While(
            Cond.Compare(idxVar, Cond.CompareOp.Lt, Expr.Length(listVar1)),
            Instr.Let(elem1, Expr.Index(listVar1, idxVar)) ::
            Instr.Let(elem2, Expr.Index(listVar2, idxVar)) ::
            transform(body, counter) :::
            List(
              Instr.Set(
                idxVar,
                Expr.BinOp(idxVar, Expr.BOp.Add, Expr.Num("1")),
              ),
            ),
          ),
        )
      case _ =>
        List(instr.mapBody(transform(_, counter)))

  private def isBindable(elem: Expr): Boolean = elem match
    case Expr.Var(_) => true
    case Expr.Tuple(elems) =>
      elems.forall(_.isInstanceOf[Expr.Var])
    case _ => false
