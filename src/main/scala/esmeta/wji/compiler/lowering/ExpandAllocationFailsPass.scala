package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}

/** Expands a tuple-destructuring `Let` immediately followed by an `If
  * allocation fails, ...` check (`Cond.AllocationFails`, see [[CondParser]])
  * into an explicit check against the `[=error=]` sentinel, mirroring how the
  * corpus's own better-formed sibling idiom already handles a call that can
  * return `tuple | error` — `module_instantiate`'s "Let |result| be
  * [=module_instantiate=](...). If |result| is [=error=], throw ... Let
  * (|store|, |instance|) be |result|." (index.bs:604-609), which checks
  * *before* destructuring. js-api's `mem_alloc`/`table_alloc` call sites
  * (index.bs:879/1051) destructure first and check second instead — "Let
  * (|store|, |memaddr|) be [=mem_alloc=](...). If allocation fails, ..." —
  * with no variable named in the check at all. This pass recovers the
  * missing subject from the immediately preceding `Let`'s own LHS tuple,
  * checking its *last* element (the embedding function's own "real" result —
  * `memaddr`/`tableaddr` — as opposed to `store`, which is present either
  * way) against `[=error=]`.
  *
  * {{{
  *   Let((|store|, |memaddr|), [=mem_alloc=](...))
  *   IfChain([(AllocationFails, throwBody)], Nil)
  * }}}
  * becomes:
  * {{{
  *   Let((|store|, |memaddr|), [=mem_alloc=](...))
  *   IfChain([(Eq(|memaddr|, Case("ERROR", Nil), false), throwBody)], Nil)
  * }}}
  *
  * Neither `mem_alloc` nor `table_alloc` — the only two embedding functions
  * this idiom is ever paired with in the corpus — can actually produce
  * `error` today (see `docs/hardcodes.md`: both defer to `$allocmem`/
  * `$alloctable`, total functions in `.spectec`'s own formalization, so
  * `Interpreter.call_func` always returns `Some`), so this check is always
  * false in practice; it exists so the *shape* is correct — reusing the same
  * generic `[=error=]`-comparison machinery every other embedding call site
  * already relies on — should either embedding function ever grow a real
  * failure path.
  *
  * Category: Structural desugaring — Elimination.
  */
object ExpandAllocationFailsPass extends LoweringPass:

  /** Requires:
    *   - [[GroupIfChainPass]]: needs the check already grouped into an
    *     `Instr.IfChain`, not a raw `If` sibling.
    */
  override def requires: Set[LoweringPass] = Set(GroupIfChainPass)

  /** Must precede:
    *   - [[ExpandDestructuringLetPass]]: needs the preceding `Let`'s LHS
    *     still a raw `Expr.Tuple`, not yet expanded into a fresh `_tupleN`
    *     temp + per-element `TupleProj` extractions — `ExpandDestructuringLetPass`
    *     itself needs nothing back from this pass, so it can't own this
    *     ordering via its own `requires` the way the common case does.
    */
  override def mustPrecede: Set[LoweringPass] = Set(ExpandDestructuringLetPass)

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = transform(a.body)))

  private def transform(instrs: List[Instr]): List[Instr] = instrs match
    case Nil => Nil
    case (let @ Instr.Let(Expr.Tuple(vars), _, _)) ::
        (ifChain @ Instr.IfChain(
          List((Cond.AllocationFails, throwBody)),
          Nil,
        )) :: rest if vars.nonEmpty =>
      // Case("ERROR", Nil), not SpecTerm("error") — this pass runs after
      // NormalizeSpecTecCaseShapePass (see mustPrecede/pipeline position
      // above), so nothing downstream would re-normalize a bare SpecTerm
      // synthesized here into the real runtime tag every other `[=error=]`
      // reference in the corpus already gets.
      val fixedCond =
        Cond.Eq(vars.last, Expr.Case("ERROR", Nil), negated = false)
      let.copy(body = transform(let.body)) ::
      ifChain.copy(branches = List((fixedCond, transform(throwBody)))) ::
      transform(rest)
    case instr :: rest =>
      instr.mapBody(transform) :: transform(rest)
