package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}
import esmeta.wji.lang.Instr.PerformOutcome

/** Hoists conditions that require real computation — a Wasm-matching embedding
  * call ([[Cond.Matches]]) or a "does any element satisfy..." search
  * ([[Cond.Exists]]) — out of a branch/assert/while's condition and into
  * ordinary preceding instructions. `Compiler.compileCond` is a pure `Cond =>
  * ir.Expr` mapping with no way to emit instructions of its own (unlike
  * mainline ESMeta's `FuncBuilder`-threaded compiler, which can interleave
  * instruction emission with expression compilation — see its
  * `ContainsCondition`/`SuchThat` handling, the direct precedent for
  * [[Cond.Exists]]'s compiled shape below), so both have to be reduced to a
  * plain, already-computed boolean before `Compiler` ever sees them.
  *
  * {{{
  *   Assert(Matches(v, "valtype", v128))
  * }}}
  * becomes
  * {{{
  *   Perform("match_valtype", [v, v128], BindResult(_m1))
  *   Assert(Eq(_m1, true))
  * }}}
  *
  * and
  * {{{
  *   If(Exists("t", [parameters, results], Matches(Var("t"), "valtype", v128)))
  * }}}
  * becomes a `found` accumulator plus one `While` loop per collection
  * (mirroring mainline's `ContainsCondition`+`SuchThat` compiled shape —
  * `esmeta.compiler.Compiler`'s `compile(fb, list, tyOpt, x, cond)`):
  * {{{
  *   Let(_found1, false)
  *   Let(_i1, 0)
  *   While(_found1 = false and _i1 < length(parameters)) {
  *     Let(t, parameters[_i1])
  *     Perform("match_valtype", [t, v128], BindResult(_m2))
  *     If(Eq(_m2, true)) { Set(_found1, true) }
  *     Set(_i1, _i1 + 1)
  *   }
  *   Let(_i2, 0)
  *   While(_found1 = false and _i2 < length(results)) { ... same shape ... }
  *   If(Eq(_found1, true)) { ...original body... }
  * }}}
  * Every collection gets its own loop, each guarded by `not found` in its own
  * `While` condition — so once an earlier collection's loop finds a match, a
  * later collection's loop runs zero iterations, without needing an explicit
  * early-exit/break instruction.
  *
  * Only fires for `Instr.Assert`/`Instr.While`, and `Instr.IfChain`s with
  * exactly one branch (every "if VALUE matches/any-in ..." call site reached so
  * far is a plain `If ..., throw ...` with no `ElseIf`/`Else`) — hoisting a
  * later branch's precondition correctly requires nesting it inside the earlier
  * branches' "false" case, which no current call site needs; left unexpanded
  * (so `Compiler` reports `EYet`) until one does.
  */
object ExpandMatchesExistsPass extends LoweringPass:

  /** Requires:
    *   - [[ExpandAbbreviatedCondPass]]: a `Cond.Exists`'s `body`, and any bare
    *     `Cond.Matches`, may still contain `Cond.Abbreviated` until then.
    *   - [[GroupIfChainPass]]: matches on `Instr.IfChain`, not a raw `If`.
    *   - [[NormalizeAlgoCallPass]]: needs any call embedded in a
    *     `Cond.Exists`'s `collections` already hoisted out first — evaluated
    *     once before the generated loop starts, so it's safe to hoist there,
    *     unlike `body` (evaluated once per iteration, deliberately left alone —
    *     see `NormalizeAlgoCallPass.extractFromCond`'s own doc).
    */
  override def requires: Set[LoweringPass] =
    Set(ExpandAbbreviatedCondPass, GroupIfChainPass, NormalizeAlgoCallPass)

  private var counter = 0
  private def fresh(prefix: String): String = {
    counter += 1; s"_$prefix$counter"
  }

  /** `Cond.Matches`'s `matchType` string, mapped to the `WasmHost` embedding
    * function that actually implements it. Only `valtype` is reached so far
    * (`match_externtype` exists in `WasmHost` too but no call site here needs
    * it yet). `matches/reftype`, seen elsewhere in the spec, has no matching
    * embedding function at all — deliberately left unmapped: a `matchType`
    * missing here is left as a bare `Cond.Matches` (see `needsHoist`/`hoist`
    * below), so `Compiler` reports its existing honest `EYet("matches ...")`
    * instead of this pass guessing a plausible-looking but nonexistent
    * embedding name (e.g. `match_reftype`) that would silently miscompile into
    * an ordinary (and wrong) algorithm call.
    */
  private val matchEmbedding: Map[String, String] = Map(
    "valtype" -> "match_valtype",
  )

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = transform(a.body)))

  private def transform(instrs: List[Instr]): List[Instr] =
    instrs.flatMap(expandInstr)

  private def expandInstr(instr: Instr): List[Instr] = instr match
    case Instr.Assert(cond, body) if needsHoist(cond) =>
      val (pre, simplified) = hoist(cond)
      pre :+ Instr.Assert(simplified, transform(body))
    case Instr.While(cond, body) if needsHoist(cond) =>
      val (pre, simplified) = hoist(cond)
      pre :+ Instr.While(simplified, transform(body))
    case Instr.IfChain(List((cond, body)), fallback) if needsHoist(cond) =>
      val (pre, simplified) = hoist(cond)
      pre :+ Instr.IfChain(
        List((simplified, transform(body))),
        transform(fallback),
      )
    case other =>
      List(other.mapBody(transform))

  private def needsHoist(cond: Cond): Boolean = cond match
    case Cond.Matches(_, matchType, _, _) => matchEmbedding.contains(matchType)
    case _: Cond.Exists                   => true
    case Cond.And(l, r)                   => needsHoist(l) || needsHoist(r)
    case Cond.Or(l, r)                    => needsHoist(l) || needsHoist(r)
    case _                                => false

  /** Returns the instructions to run first, and a pure replacement condition
    * (no known-hoistable `Matches`/`Exists` left) to check afterward. A
    * `Matches` with an unmapped `matchType` passes through unchanged (see
    * `matchEmbedding`'s doc) rather than being hoisted.
    */
  private def hoist(cond: Cond): (List[Instr], Cond) = cond match
    case Cond.Matches(lhs, matchType, rhs, neg)
        if matchEmbedding.contains(matchType) =>
      val tmp = fresh("m")
      val call = Instr.Perform(
        matchEmbedding(matchType),
        List(lhs, rhs),
        PerformOutcome.BindResult(tmp),
      )
      (List(call), Cond.Eq(Expr.Var(tmp), Expr.Bool(true), neg))
    case Cond.Exists(binder, collections, body) =>
      val (bodyPre, bodyCond) = hoist(body)
      val found = fresh("found")
      val init = Instr.Let(Expr.Var(found), Expr.Bool(false))
      val loops = collections.map { coll =>
        val idx = fresh("i")
        List(
          Instr.Let(Expr.Var(idx), Expr.Num("0")),
          Instr.While(
            Cond.And(
              Cond.Eq(Expr.Var(found), Expr.Bool(false)),
              Cond.Compare(
                Expr.Var(idx),
                Cond.CompareOp.Lt,
                Expr.Length(coll),
              ),
            ),
            List(
              Instr.Let(Expr.Var(binder), Expr.Index(coll, Expr.Var(idx))),
            ) ++ bodyPre ++ List(
              Instr.IfChain(
                List(
                  (bodyCond, List(Instr.Set(Expr.Var(found), Expr.Bool(true)))),
                ),
                Nil,
              ),
              Instr.Set(
                Expr.Var(idx),
                Expr.BinOp(Expr.Var(idx), Expr.BOp.Add, Expr.Num("1")),
              ),
            ),
          ),
        )
      }.flatten
      (init :: loops, Cond.Eq(Expr.Var(found), Expr.Bool(true)))
    case Cond.And(l, r) =>
      val (lp, lc) = hoist(l)
      val (rp, rc) = hoist(r)
      (lp ++ rp, Cond.And(lc, rc))
    case Cond.Or(l, r) =>
      val (lp, lc) = hoist(l)
      val (rp, rc) = hoist(r)
      (lp ++ rp, Cond.Or(lc, rc))
    case other => (Nil, other)
