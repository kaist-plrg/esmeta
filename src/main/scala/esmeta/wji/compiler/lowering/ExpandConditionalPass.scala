package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}

/** Expands `Expr.Conditional` — WebIDL's conditional-value idiom parsed by
  * `ExprParser` (webidl_yet_categorized.md category I-G; see
  * `Expr.Conditional`'s own doc for its two surface forms) — into a real
  * `Instr.IfChain`, one branch per `(cond, expr)` pair plus a fallback branch
  * built from `otherwise`:
  * {{{
  *   IfChain(branches.map((cond, e) => (cond, [<rebuild with e>])), <fallback>)
  * }}}
  * where `<fallback>` is `[<rebuild with otherwise.get>]` if `otherwise` is
  * `Some`, or `[Assert(Cond.Unreachable)]` if `None` — reusing `Unreachable`
  * (already used for the spec's own literal "This step is not reached",
  * `CondParser.UnreachableStep`) rather than inventing a separate "assert
  * false" node, since it already compiles to exactly that
  * (`Compiler.compileCond`'s `Cond.Unreachable => EBool(false)`).
  *
  * Two placements are handled — every real occurrence seen so far is one of
  * these two, e.g. `Let(|modifiable|, Conditional([(|op| is [=unforgeable=],
  * <emu-val>false</emu-val>)], Some(<emu-val>true</emu-val>)))` for the first,
  * and `Perform([=Compute the effective overload set=], [Conditional([(|op| is
  * a regular operation, [=regular operations=]), (|op| is a static operation,
  * [=static operations=])], None), ...], ...)` for the second
  * (webidl/index.bs:12558-12559, docs/hardcodes.md #15):
  *
  *   - `Let(lhs, Conditional(...), body)` — the `Conditional` is the whole RHS;
  *     each branch/fallback instruction is `Let(lhs, <expr>)`.
  *   - `Perform(func, args, outcome, body)` where `args` contains a
  *     `Conditional` at some position — each branch/fallback instruction is
  *     `Perform(func, args.updated(idx, <expr>), outcome)`, i.e. the whole
  *     statement re-run with just that one argument slot swapped.
  *
  * so `Compiler` never needs to know about `Expr.Conditional` at all — like
  * every other `Expr`/`Cond` node with no IR-level equivalent, it's fully
  * eliminated before compilation, not special-cased inside
  * `Compiler.compileInstr`.
  *
  * Category: Structural desugaring — Elimination.
  */
object ExpandConditionalPass extends LoweringPass:

  /** Requires:
    *   - [[GroupIfChainPass]]: this pass emits a ready-made `Instr.IfChain`
    *     directly, rather than a bare `If`/`Else` for a not-yet-run
    *     `GroupIfChainPass` to fold — mirrors `ExpandRemovePass`'s own
    *     precedent.
    */
  override def requires: Set[LoweringPass] = Set(GroupIfChainPass)

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = transform(a.body)))

  private def transform(instrs: List[Instr]): List[Instr] =
    instrs.flatMap(expandInstr)

  /** `Instr.IfChain`'s branches/fallback for a `Conditional`, given how to
    * rebuild the surrounding instruction from one substituted-in `Expr`.
    */
  private def ifChainFor(
    conditional: Expr.Conditional,
    rebuild: Expr => Instr,
  ): Instr.IfChain =
    val Expr.Conditional(branches, otherwise) = conditional
    val ifChainBranches =
      branches.map((cond, e) => (cond, List(rebuild(e))))
    val fallback = otherwise match
      case Some(e) => List(rebuild(e))
      case None    => List(Instr.Assert(Cond.Unreachable))
    Instr.IfChain(ifChainBranches, fallback)

  private def expandInstr(instr: Instr): List[Instr] = instr match
    case Instr.Let(lhs, conditional: Expr.Conditional, body) =>
      ifChainFor(conditional, Instr.Let(lhs, _)) :: transform(body)
    case Instr.Perform(func, args, outcome, body)
        if args.exists(_.isInstanceOf[Expr.Conditional]) =>
      val idx = args.indexWhere(_.isInstanceOf[Expr.Conditional])
      val conditional = args(idx).asInstanceOf[Expr.Conditional]
      ifChainFor(
        conditional,
        e => Instr.Perform(func, args.updated(idx, e), outcome),
      ) :: transform(body)
    case _ => List(instr.mapBody(transform))
