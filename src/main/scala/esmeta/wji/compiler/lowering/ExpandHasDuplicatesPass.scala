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
  *
  * Category: Structural desugaring — Elimination.
  */
object ExpandHasDuplicatesPass extends LoweringPass:

  /** Requires:
    *   - [[GroupIfChainPass]]: needs the condition already inside an
    *     `Instr.IfChain` branch, not a raw `If`/`ElseIf` sibling.
    */
  override def requires: Set[LoweringPass] = Set(GroupIfChainPass)

  /** Generates this pass's `_dupList`/`_dupFound`/`_dupI`/`_dupJ` names for a
    * single algorithm. Scoped as a value local to each [[run]] iteration rather
    * than a mutable field on this `object` — the latter is JVM-wide singleton
    * state, so concurrent `run` calls (e.g. multiple ScalaTest suites compiling
    * algorithms in parallel, which sbt's default `Test / parallelExecution`
    * allows) would race on incrementing/resetting a shared counter, producing
    * nondeterministic naming depending on thread interleaving.
    */
  private class Counter:
    private var n = 0
    def freshList(): String = { n += 1; s"_dupList$n" }
    def freshFound(): String = { n += 1; s"_dupFound$n" }
    def freshI(): String = { n += 1; s"_dupI$n" }
    def freshJ(): String = { n += 1; s"_dupJ$n" }

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map { a =>
      val counter = Counter()
      a.copy(body = transform(a.body, counter))
    }

  private def transform(instrs: List[Instr], counter: Counter): List[Instr] =
    instrs.flatMap(expandInstr(_, counter))

  private def expandInstr(instr: Instr, counter: Counter): List[Instr] =
    instr match
      case i: Instr.IfChain =>
        i.branches match
          case (Cond.HasDuplicates(list, neg), body) :: Nil =>
            val newBody = transform(body, counter)
            val newFallback = transform(i.fallback, counter)
            expand(
              list,
              neg,
              c => Instr.IfChain(List((c, newBody)), newFallback),
              counter,
            )
          case _ =>
            List(
              i.copy(
                branches = i.branches.map((c, b) => (c, transform(b, counter))),
                fallback = transform(i.fallback, counter),
              ),
            )
      case _ => List(instr.mapBody(transform(_, counter)))

  /** Builds the pairwise-comparison loop for `list`, then applies `rebuild` (an
    * `IfChain` template with a hole for the found-flag check) to produce the
    * final branch.
    */
  private def expand(
    list: Expr,
    negated: Boolean,
    rebuild: Cond => Instr.IfChain,
    counter: Counter,
  ): List[Instr] =
    val listVar = Expr.Var(counter.freshList())
    val foundVar = Expr.Var(counter.freshFound())
    val iVar = Expr.Var(counter.freshI())
    val jVar = Expr.Var(counter.freshJ())

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
