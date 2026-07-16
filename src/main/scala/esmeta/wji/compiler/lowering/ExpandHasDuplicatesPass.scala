package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}

/** Expands a `Cond.HasDuplicates` branch condition of an `Instr.IfChain` (e.g.
  * "If |builtinSetNames| contains any duplicates, return false.",
  * js-api/index.bs:1863) into an explicit pairwise `While` loop that computes a
  * boolean accumulator, mirroring mainline `esmeta.compiler.Compiler`'s
  * `AUX_HAS_DUPLICATE`
  * (`src/main/resources/manuals/funcs/__HAS_DUPLICATE__.ir`) — same `i` from
  * `1`, `j` from `0` to `i-1` bounds — except it can't be a standalone
  * auxiliary function call, since wji's `Compiler.compileCond` is a pure `Cond
  * => ir.Expr` with no side channel for emitting instructions. Instead this
  * hoists the loop as a `Let`/`While` prefix in front of the `IfChain` and
  * rewrites the branch condition into a check of the resulting flag, e.g.
  * {{{
  *   IfChain([(HasDuplicates(list, neg), body)], fallback)
  * }}}
  * becomes
  * {{{
  *   Let(_dupListN, list)
  *   Let(_dupFoundN, false)
  *   Let(_dupIN, 1)
  *   While(_dupIN < length(_dupListN),
  *     Let(_dupJN, 0)
  *     While(_dupJN < _dupIN,
  *       IfChain([(_dupListN[_dupIN] is _dupListN[_dupJN], [Set(_dupFoundN, true)])], [])
  *       Set(_dupJN, _dupJN + 1))
  *     Set(_dupIN, _dupIN + 1))
  *   IfChain([(_dupFoundN is [not] true, body)], fallback)
  * }}}
  * A `Set` (rather than an early `return true`, unlike the mainline aux
  * function) is used inside the innermost check since a branch's `body` here
  * may be arbitrary code, not just "return true".
  *
  * Only fires when `HasDuplicates` is the (sole) top-level condition of an
  * `IfChain` branch — the only shape reached so far.
  */
object ExpandHasDuplicatesPass extends LoweringPass:

  /** Requires:
    *   - [[GroupIfChainPass]]: needs the condition already inside an
    *     `Instr.IfChain` branch, not a raw `If`/`ElseIf` sibling.
    */
  override def requires: Set[LoweringPass] = Set(GroupIfChainPass)

  private var counter = 0
  private def freshList(): String = { counter += 1; s"_dupList$counter" }
  private def freshFound(): String = { counter += 1; s"_dupFound$counter" }
  private def freshI(): String = { counter += 1; s"_dupI$counter" }
  private def freshJ(): String = { counter += 1; s"_dupJ$counter" }

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = transform(a.body)))

  private def transform(instrs: List[Instr]): List[Instr] =
    instrs.flatMap(expandInstr)

  private def expandInstr(instr: Instr): List[Instr] = instr match
    case i: Instr.IfChain =>
      i.branches match
        case (Cond.HasDuplicates(list, neg), body) :: Nil =>
          val newBody = transform(body)
          val newFallback = transform(i.fallback)
          expand(
            list,
            neg,
            c => Instr.IfChain(List((c, newBody)), newFallback),
          )
        case _ =>
          List(
            i.copy(
              branches = i.branches.map((c, b) => (c, transform(b))),
              fallback = transform(i.fallback),
            ),
          )
    case _ => List(instr.mapBody(transform))

  /** Builds the pairwise-comparison loop for `list`, then applies `rebuild` (an
    * `IfChain` template with a hole for the found-flag check) to produce the
    * final branch.
    */
  private def expand(
    list: Expr,
    negated: Boolean,
    rebuild: Cond => Instr.IfChain,
  ): List[Instr] =
    val listVar = Expr.Var(freshList())
    val foundVar = Expr.Var(freshFound())
    val iVar = Expr.Var(freshI())
    val jVar = Expr.Var(freshJ())

    List(
      Instr.Let(listVar, list),
      Instr.Let(foundVar, Expr.Bool(false)),
      Instr.Let(iVar, Expr.Num("1")),
      Instr.While(
        Cond.Compare(iVar, Cond.CompareOp.Lt, Expr.Length(listVar)),
        List(
          Instr.Let(jVar, Expr.Num("0")),
          Instr.While(
            Cond.Compare(jVar, Cond.CompareOp.Lt, iVar),
            List(
              Instr.IfChain(
                List(
                  (
                    Cond
                      .Eq(Expr.Index(listVar, iVar), Expr.Index(listVar, jVar)),
                    List(Instr.Set(foundVar, Expr.Bool(true))),
                  ),
                ),
                Nil,
              ),
              Instr.Set(jVar, Expr.BinOp(jVar, Expr.BOp.Add, Expr.Num("1"))),
            ),
          ),
          Instr.Set(iVar, Expr.BinOp(iVar, Expr.BOp.Add, Expr.Num("1"))),
        ),
      ),
      rebuild(Cond.Eq(foundVar, Expr.Bool(true), negated)),
    )
