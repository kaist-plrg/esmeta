package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Expr, Instr}
import esmeta.wji.lang.Instr.PerformOutcome

/** Mirrors the Infra Standard's "no explicit catch → automatically re-throw"
  * convention for any call to a known completion-returning algorithm (see
  * [[CompletionAlgorithms]]) that the spec prose doesn't mark with `?`/`!` and
  * doesn't otherwise handle itself: synthesizes the exact same
  * `Expr.Abrupt("?", ...)` marker a genuine spec-authored `?` would produce, so
  * [[ExpandAbruptPass]] — which runs immediately after this pass, see
  * `Lowering.pipeline` — is the *one* place in the whole pipeline that ever
  * lowers an abrupt-completion check into real instructions. This pass's own
  * job is entirely detection: *whether* a guard belongs at a given call site,
  * never *how* to build one.
  *
  * {{{
  *   Let(x, AlgoCall(f, args), body)
  *   <no read of x.[[Type]] anywhere in body>
  * }}}
  * becomes:
  * {{{
  *   Let(_guardN, AlgoCall(f, args))
  *   Let(x, Abrupt("?", Var(_guardN)), body)
  * }}}
  * and likewise for the native `Perform(f, args, BindResult(x))` verb-phrase
  * shape (`InstrParser`'s `PerformAndLetSuffix`, e.g. "Perform F(args), and let
  * X be the result." — parses straight to `Perform` without ever going through
  * `Let`/`AlgoCall`), except no fresh var is needed there: `x` is already bound
  * to the raw call result, so the marker can just wrap a reference to `x`
  * itself.
  *
  * *Whether to insert the guard at all* is the real work here: only
  * `completionAlgos` members are targeted, and only when
  * `CompletionAlgorithms.isAbsorbed` says the caller doesn't already handle it
  * — otherwise every single call in the file would grow a dead guard.
  *
  * Category: Completion-record convention.
  */
object PropagateUnguardedCallsPass extends LoweringPass:

  /** Requires:
    *   - [[MarkCompletionAlgorithmsPass]]: needs `returnsCompletion` already
    *     stamped on every `Algorithm`.
    *   - [[NormalizeEvaluationOrderPass]]: needs every `AlgoCall`/`JSCall`
    *     already hoisted to `Let`/`Return` direct-RHS position — the same
    *     guarantee [[ExpandAbruptPass]]/[[ExtractInlineAlgoCallPass]] rely on
    *     for the identical reason.
    *
    * Must precede:
    *   - [[ExpandAbruptPass]]: the synthesized `Expr.Abrupt` marker this pass
    *     produces must be swept up by the very next pass, or it's stranded —
    *     `Expr.Abrupt` has no other consumer anywhere in the pipeline.
    */
  override def requires: Set[LoweringPass] = Set(
    MarkCompletionAlgorithmsPass,
    NormalizeEvaluationOrderPass,
  )
  override def mustPrecede: Set[LoweringPass] = Set(ExpandAbruptPass)

  def run(algos: List[Algorithm]): List[Algorithm] =
    val completionAlgos: Set[String] = algos
      .collect {
        case a if a.returnsCompletion => a.name.orElse(a.id).map(_.toLowerCase)
      }
      .flatten
      .toSet
    algos.map { a =>
      counter = 0
      a.copy(body = transform(a.body, completionAlgos))
    }

  private var counter = 0
  private def fresh(): String = { counter += 1; s"_guard$counter" }

  private def transform(
    instrs: List[Instr],
    completionAlgos: Set[String],
  ): List[Instr] = instrs match
    case Nil => Nil

    // native verb-phrase call ("Perform F(args), and let X be the result.")
    // — x is already bound, so the marker just wraps a reference to it (Set,
    // not Let: this reassigns the same already-existing binding, unlike the
    // Let case below's genuinely-new one).
    case (p @ Instr.Perform(
          func,
          _,
          PerformOutcome.BindResult(rawX),
          pbody,
        )) :: rest
        if completionAlgos.contains(CompletionAlgorithms.normalize(func)) &&
        !CompletionAlgorithms
          .isAbsorbed(Some(stripPipes(rawX)), pbody ++ rest) =>
      val x = stripPipes(rawX)
      val xVar = Expr.Var(x)
      p.copy(body = Nil) :: (guard(Instr.Set(xVar, _, Nil), xVar) ++
      transform(pbody, completionAlgos) ++
      transform(rest, completionAlgos))

    // "Let X be F(args)." — f itself is still a bare AlgoCall/JSCall here
    // (ExtractInlineAlgoCallPass, which converts this to Perform, runs after
    // this pass now) — needs a fresh temp to hold the raw call result, since
    // ExpandAbruptPass requires Expr.Abrupt's inner to always be a bare Var,
    // never the call expression itself (see NormalizeEvaluationOrderPass's
    // identical hoisting for a genuine spec-authored `?`).
    //
    // isBoundForAbrupt guards against a real ambiguity running this early
    // introduces: NormalizeEvaluationOrderPass hoists a genuinely-marked
    // "Let x be ? F(args)." into the exact same shape this case would
    // otherwise match — Let(_tmp, AlgoCall(f,args)) immediately followed by
    // Let(x, Abrupt("?", Var(_tmp)), ...) — before ExpandAbruptPass (which
    // runs right after this pass) ever gets to consume it. Without this
    // check, an explicitly-marked call gets double-guarded: this pass'
    // synthesized guard unwraps _tmp into a bare value first, and
    // ExpandAbruptPass's own expansion of the *real* marker then wrongly
    // re-checks that already-unwrapped value as if it might still be a
    // completion.
    case Instr.Let(lhs @ Expr.Var(x), rhs, body) :: rest
        if isUnguardedCompletionCall(rhs, completionAlgos) &&
        !isBoundForAbrupt(x, body ++ rest) &&
        !CompletionAlgorithms.isAbsorbed(Some(x), body ++ rest) =>
      val tmpVar = Expr.Var(fresh())
      Instr.Let(tmpVar, rhs, Nil) :: (guard(Instr.Let(lhs, _, Nil), tmpVar) ++
      transform(body, completionAlgos) ++
      transform(rest, completionAlgos))

    case i :: rest =>
      i.mapBody(transform(_, completionAlgos)) :: transform(
        rest,
        completionAlgos,
      )

  private def isUnguardedCompletionCall(
    expr: Expr,
    completionAlgos: Set[String],
  ): Boolean = expr match
    case Expr.AlgoCall(link, _) =>
      completionAlgos.contains(CompletionAlgorithms.normalize(link))
    case Expr.JSCall(name, _) =>
      completionAlgos.contains(CompletionAlgorithms.normalize(name))
    case _ => false

  /** Whether `x` is about to be consumed as the `inner` of an `Expr.Abrupt`
    * marker in the very next instruction — see the call site's own doc for why
    * this specific ambiguity only exists once this pass runs before
    * `ExpandAbruptPass`.
    */
  private def isBoundForAbrupt(x: String, next: List[Instr]): Boolean =
    next.headOption.exists {
      case Instr.Let(_, Expr.Abrupt(_, Expr.Var(v)), _)       => v == x
      case Instr.Set(_, Expr.Abrupt(_, Expr.Var(v)), _)       => v == x
      case Instr.Return(Some(Expr.Abrupt(_, Expr.Var(v))), _) => v == x
      case _                                                  => false
    }

  /** Synthesizes the same `Expr.Abrupt("?", ...)` marker a genuine
    * spec-authored `?` produces. `bindLhs` — mirrors
    * [[ExpandAbruptPass.expand]]'s own `bind` parameter exactly — embeds the
    * marker into whichever instruction shape the call site is using
    * (`Instr.Set(x, _, Nil)` to reassign an already-bound `x`, or `Instr.Let(x,
    * _, Nil)` to introduce a genuinely new one); `inner` is the bare `Var`
    * holding the raw call result to check.
    */
  private def guard(bindLhs: Expr => Instr, inner: Expr.Var): List[Instr] =
    List(bindLhs(Expr.Abrupt("?", inner)))

  // see CompletionAlgorithms's identically-named/documented helper
  private def stripPipes(s: String): String =
    s.stripPrefix("|").stripSuffix("|")
