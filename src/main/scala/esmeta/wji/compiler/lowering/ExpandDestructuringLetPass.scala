package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Expr, Instr}

/** Expands `Let(Tuple([x, y, ...]), expr, body)` into individual bindings.
  *
  * {{{
  *   Let((|store|, |addr|), call)
  * }}}
  * becomes:
  * {{{
  *   Let(_tupleN, call)
  *   Let(|store|, _tupleN.0)
  *   Let(|addr|,  _tupleN.1)
  * }}}
  *
  * Every tuple-destructuring `Let` in the spec (directly or indirectly, via a
  * preceding `Let |result| be ...`) unpacks the `(store, X)` result of a Wasm
  * embedding call — a `Wasm(TupV(...))`, not a heap list/record — so the
  * destructured fields are built as [[Expr.TupleProj]] (compiles to
  * `ir.EProj`), not a plain [[Expr.Index]] (compiles to a heap `Field` read;
  * see `esmeta.wji.compiler.Compiler`).
  *
  * The original body (if non-empty) is appended after the destructured
  * bindings. In practice, tuple-destructuring Let nodes in the spec have empty
  * bodies.
  */
object ExpandDestructuringLetPass extends LoweringPass:
  private var counter = 0
  private def freshTuple(): String = { counter += 1; s"_tuple$counter" }

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = transform(a.body)))

  private def transform(instrs: List[Instr]): List[Instr] =
    instrs.flatMap(expandInstr)

  private def expandInstr(instr: Instr): List[Instr] = instr match
    case Instr.Let(Expr.Tuple(elems), expr, body) =>
      val tmp = freshTuple()
      val binding = Instr.Let(Expr.Var(tmp), expr)
      val destructures = elems.zipWithIndex.map { (elem, i) =>
        Instr.Let(elem, Expr.TupleProj(Expr.Var(tmp), i))
      }
      List(binding) ++ destructures ++ transform(body)
    case _ =>
      List(instr.mapBody(transform))
