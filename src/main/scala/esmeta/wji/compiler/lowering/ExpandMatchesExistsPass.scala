package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}
import esmeta.wji.lang.Instr.PerformOutcome
import esmeta.wji.bridge.host.WasmHost

/** Hoists conditions that require real computation — a Wasm-matching embedding
  * call ([[Cond.Matches]]) or a "does any element satisfy..." search
  * ([[Cond.Any]]) — out of a branch/assert/while's condition and into ordinary
  * preceding instructions. `Compiler.compileCond` is a pure `Cond => ir.Expr`
  * mapping with no way to emit instructions of its own (unlike mainline
  * ESMeta's `FuncBuilder`-threaded compiler, which can interleave instruction
  * emission with expression compilation — see its
  * `ContainsCondition`/`SuchThat` handling, the direct precedent for
  * [[Cond.Any]]'s compiled shape below), so both have to be reduced to a plain,
  * already-computed boolean before `Compiler` ever sees them.
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
  *   If(Any("t", [parameters, results], Matches(Var("t"), "valtype", v128)))
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
  * For `Instr.IfChain`, hoisting a later branch's precondition correctly
  * requires nesting it inside the earlier branches' "false" case rather than
  * flat-hoisting to the top — e.g. index.bs:12051's "declared with the
  * [{{Global}}] extended attribute" sits in the first branch of a 4-way chain,
  * and index.bs:12064's variant sits in a later `Otherwise if`. `buildChain`
  * below rebuilds the branch list back-to-front (mirroring
  * `NormalizeEvaluationOrderPass.buildChain`, which solves the identical
  * ordering problem for hoisted calls — see its own doc): a branch needing
  * nothing hoisted is folded back in as a flat sibling of whatever `rest`
  * already built, so a chain needing no hoisting anywhere round-trips with its
  * original flat shape unchanged; once a branch does need hoisting, its `pre`
  * instructions only belong on the path reached once every earlier branch's
  * condition is already false, so everything from there on nests one level
  * deeper instead of staying a flat sibling.
  *
  * `expandInstr` only dispatches to `buildChain` when [[chainNeedsHoist]] finds
  * a real `Matches`/`Any` somewhere in the chain (including down through a
  * single-branch `IfChain`'s own `fallback`, the shape several earlier passes —
  * `ExpandAbruptPass`/`ExpandThrowsPass`/etc. — routinely build for unrelated
  * reasons) — otherwise it falls through to the generic `mapBody` case below,
  * leaving that `IfChain` node's own flat-vs-nested shape exactly as those
  * earlier passes built it. Without this guard, `buildChain`'s own
  * flatten-when-possible step would still leave runtime behavior unchanged
  * (`Compiler.compileInstr`'s `IfChain` case recurses into a nested
  * single-branch `fallback` exactly as it would a flat multi-branch list, so
  * the compiled `IIf` tree comes out identical either way), but would
  * needlessly re-flatten every such incidental nesting anywhere in the corpus
  * into a differently-shaped (if semantically equivalent) `IfChain`, showing up
  * as unrelated `ir.expected` diff noise ("else { if ... }" reprinted as "else
  * if ...").
  *
  * Category: Spec-dependent — SpecTec.
  */
object ExpandMatchesExistsPass extends LoweringPass:

  /** Requires:
    *   - [[ExpandAbbreviatedCondPass]]: a `Cond.Any`'s `body`, and any bare
    *     `Cond.Matches`, may still contain `Cond.Abbreviated` until then.
    *   - [[GroupIfChainPass]]: matches on `Instr.IfChain`, not a raw `If`.
    *   - [[NormalizeEvaluationOrderPass]]: needs any call embedded in a
    *     `Cond.Any`'s `collections` already hoisted out first — evaluated once
    *     before the generated loop starts, so it's safe to hoist there, unlike
    *     `body` (evaluated once per iteration, deliberately left alone — see
    *     `NormalizeEvaluationOrderPass.extractFromCond`'s own doc).
    */
  override def requires: Set[LoweringPass] =
    Set(
      ExpandAbbreviatedCondPass,
      GroupIfChainPass,
      NormalizeEvaluationOrderPass,
    )

  /** Generates this pass's `_mN`/`_foundN`/`_iN` names for a single algorithm.
    * Scoped as a value local to each [[run]] iteration rather than a mutable
    * field on this `object` — the latter is JVM-wide singleton state, so
    * concurrent `run` calls (e.g. multiple ScalaTest suites compiling
    * algorithms in parallel, which sbt's default `Test / parallelExecution`
    * allows) would race on incrementing/resetting a shared counter, producing
    * nondeterministic naming depending on thread interleaving.
    */
  private class Counter:
    private var n = 0
    def fresh(prefix: String): String = { n += 1; s"_$prefix$n" }

  /** `Cond.Matches`'s `matchType` (e.g. `"valtype"`) mapped to the `WasmHost`
    * embedding function implementing it, normally `s"match_$matchType"` —
    * except `"reftype"`, aliased to `match_valtype` (reftype is just a valtype
    * variant, already handled there). Checked against `WasmHost.names` rather
    * than assumed, so any other `matchType` with no real embedding function is
    * left unhoisted — `Compiler` reports an honest `EYet("matches ...")`
    * instead of miscompiling a guessed, nonexistent call.
    */
  private def matchEmbeddingName(matchType: String): Option[String] =
    val normalizedType = matchType match
      case "reftype" => "valtype"
      case s         => s
    val name = s"match_$normalizedType"
    Option.when(WasmHost.names.contains(name))(name)

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map { a =>
      val counter = Counter()
      a.copy(body = transform(a.body, counter))
    }

  private def transform(instrs: List[Instr], counter: Counter): List[Instr] =
    instrs.flatMap(expandInstr(_, counter))

  private def expandInstr(instr: Instr, counter: Counter): List[Instr] =
    instr match
      case Instr.Assert(cond, body) if needsHoist(cond) =>
        val (pre, simplified) = hoist(cond, counter)
        pre :+ Instr.Assert(simplified, transform(body, counter))
      case Instr.While(cond, body) if needsHoist(cond) =>
        val (pre, simplified) = hoist(cond, counter)
        pre :+ Instr.While(simplified, transform(body, counter))
      case Instr.IfChain(branches, fallback)
          if chainNeedsHoist(branches, fallback) =>
        buildChain(branches, fallback, counter)
      case other =>
        List(other.mapBody(transform(_, counter)))

  /** Whether [[buildChain]] would actually hoist anything out of this chain —
    * checked before calling it, so a chain needing nothing hoisted is left for
    * the generic `mapBody` recursion instead (see class doc). Looks past a
    * single-branch `IfChain`'s own `fallback` (the shape produced by e.g.
    * `ExpandAbruptPass`/`ExpandThrowsPass` for their own, unrelated reasons) to
    * see whether a hoist is needed further down the logical chain, the same
    * shape `buildChain` itself would flatten through to reach it.
    */
  private def chainNeedsHoist(
    branches: List[(Cond, List[Instr])],
    fallback: List[Instr],
  ): Boolean =
    branches.exists((cond, _) => needsHoist(cond)) || (fallback match
      case List(Instr.IfChain(nestedBranches, nestedFallback)) =>
        chainNeedsHoist(nestedBranches, nestedFallback)
      case _ => false
    )

  /** Rebuilds an `IfChain`'s branch list back-to-front, hoisting each branch's
    * own condition via [[hoist]] as it goes — see this pass's class doc.
    */
  private def buildChain(
    branches: List[(Cond, List[Instr])],
    fallback: List[Instr],
    counter: Counter,
  ): List[Instr] = branches match
    case Nil => transform(fallback, counter)
    case (cond, body) :: rest =>
      val (pre, newCond) =
        if needsHoist(cond) then hoist(cond, counter) else (Nil, cond)
      val newBody = transform(body, counter)
      val restInstrs = buildChain(rest, fallback, counter)
      if pre.isEmpty then
        restInstrs match
          case List(Instr.IfChain(restBranches, restFallback)) =>
            List(
              Instr.IfChain((newCond, newBody) :: restBranches, restFallback),
            )
          case other =>
            List(Instr.IfChain(List((newCond, newBody)), other))
      else pre ++ List(Instr.IfChain(List((newCond, newBody)), restInstrs))

  private def needsHoist(cond: Cond): Boolean = cond match
    case Cond.Matches(_, matchType, _, _) =>
      matchEmbeddingName(matchType).isDefined
    case _: Cond.Any    => true
    case Cond.And(l, r) => needsHoist(l) || needsHoist(r)
    case Cond.Or(l, r)  => needsHoist(l) || needsHoist(r)
    case _              => false

  /** Returns the instructions to run first, and a pure replacement condition
    * (no known-hoistable `Matches`/`Any` left) to check afterward. A `Matches`
    * with no real embedding function (see `matchEmbeddingName`'s doc) passes
    * through unchanged rather than being hoisted.
    */
  private def hoist(cond: Cond, counter: Counter): (List[Instr], Cond) =
    cond match
      case Cond.Matches(lhs, matchType, rhs, neg)
          if matchEmbeddingName(matchType).isDefined =>
        val tmp = counter.fresh("m")
        val call = Instr.Perform(
          matchEmbeddingName(matchType).get,
          List(lhs, rhs),
          PerformOutcome.BindResult(tmp),
        )
        (List(call), Cond.Eq(Expr.Var(tmp), Expr.Bool(true), neg))
      case Cond.Any(binder, collections, body, neg) =>
        val (bodyPre, bodyCond) = hoist(body, counter)
        val found = counter.fresh("found")
        val init = Instr.Let(Expr.Var(found), Expr.Bool(false))
        val loops = collections.map { coll =>
          val idx = counter.fresh("i")
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
                    (
                      bodyCond,
                      List(Instr.Set(Expr.Var(found), Expr.Bool(true))),
                    ),
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
        (init :: loops, Cond.Eq(Expr.Var(found), Expr.Bool(true), neg))
      case Cond.And(l, r) =>
        val (lp, lc) = hoist(l, counter)
        val (rp, rc) = hoist(r, counter)
        (lp ++ rp, Cond.And(lc, rc))
      case Cond.Or(l, r) =>
        val (lp, lc) = hoist(l, counter)
        val (rp, rc) = hoist(r, counter)
        (lp ++ rp, Cond.Or(lc, rc))
      case other => (Nil, other)
