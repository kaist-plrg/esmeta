package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Expr, Instr, WjiParam}

/** Rewrites every `Expr.FollowingSteps(params)` placeholder — "the following
  * steps ...:", wherever it appears as an argument/value (see
  * [[esmeta.wji.lang.Expr.FollowingSteps]]) — into a `Closure` referencing a
  * freshly split-off top-level [[Algorithm]]:
  *
  * {{{
  *   Let(Var("onFulfilledSteps"), FollowingSteps(List("V")), substeps)
  * }}}
  * becomes:
  * {{{
  *   Let(Var("onFulfilledSteps"), Closure(closureName, captured), Nil)
  * }}}
  * and
  * {{{
  *   Perform("[=Queue a task=]", [Var("taskSource"), FollowingSteps(Nil)], Discard, substeps)
  * }}}
  * becomes:
  * {{{
  *   Perform("[=Queue a task=]", [Var("taskSource"), Closure(closureName, captured)], Discard)
  * }}}
  * where `substeps` — always the *owning instruction's* own `body`, since
  * [[esmeta.wji.lang.parser.ExprParser]] only ever sees the one prose string
  * introducing the phrase, never the nested list items it introduces — is split
  * off into a fresh [[Algorithm]] named `closureName` taking `params` as formal
  * parameters, and `captured` is every other free variable `substeps`
  * references (computed by [[FreeVarAnalysis]], excluding `params`).
  * `closureName` is derived from the *enclosing* top-level [[Algorithm]]'s own
  * name/id — the algorithm the "following steps" phrase textually resides in —
  * suffixed with a counter scoped to that one algorithm (`"react_closure1"`,
  * `"react_closure2"`, ...), so it's traceable back to its spec source rather
  * than an opaque global sequence number.
  *
  * Purely a hoisting/naming transform: the resulting `Algorithm` keeps exactly
  * the parameters the spec text itself declared (e.g. just `V`), and its body
  * is otherwise untouched. It has no notion of what calling convention the
  * closure is ultimately invoked under — a closure passed to
  * `CreateBuiltinFunction` additionally needs the fixed 3-argument builtin
  * signature and completion-record-wrapped returns, but that adaptation is a
  * separate concern handled afterward by [[AddBuiltinBehaviourPass]] (see its
  * own doc for why it isn't done here instead).
  *
  * Generic over instruction shape: a `FollowingSteps` may sit directly as a
  * `Let`'s RHS or among a `Perform`'s `args`; adding a third shape (e.g. a
  * `Set`/`Return` RHS, if a future spec phrasing needs it) is a same-shaped
  * addition to `transform` below, not a new pass. [[postconditions]] checks
  * that no `FollowingSteps` remains anywhere once `transform` is done —
  * covering both a third shape like that and the narrower gap of one of the two
  * known shapes with an empty owning `body` (`transform`'s guards require
  * `body.nonEmpty`, since a hoist needs actual substeps to hoist) — so
  * `Lowering.run` throws `UnsupportedSpecShape` instead of silently leaving it
  * for `Compiler`'s much later, less specific `EYet` fallback.
  *
  * Runs late: `body` rides through every earlier pass as ordinary nested
  * `Let.body`/`Perform.body` content (every pass already recurses into it via
  * `Instr.mapBody`), so by the time this pass sees it, it's already fully
  * lowered.
  *
  * Category: Structural desugaring.
  */
object ExpandFollowingStepsPass extends LoweringPass:

  override def postconditions: List[Condition] = List(
    Condition(
      "no FollowingSteps remains unhoisted (only a Let RHS / Perform arg " +
      "with a non-empty owning body are handled)",
      _.forall(a =>
        !AstQuery.existsExpr(a.body)(_.isInstanceOf[Expr.FollowingSteps]),
      ),
    ),
  )

  def run(algos: List[Algorithm]): List[Algorithm] =
    val extra = collection.mutable.ListBuffer.empty[Algorithm]
    val rewritten = algos.map { a =>
      val base = a.name.orElse(a.id).getOrElse("algo")
      var counter = 0
      def freshName(): String =
        counter += 1; s"${base}_closure$counter"
      a.copy(body = transform(a.body, freshName, extra))
    }
    rewritten ++ extra.toList

  private def hoist(
    params: List[String],
    variadicLast: Boolean,
    body: List[Instr],
    freshName: () => String,
    extra: collection.mutable.ListBuffer[Algorithm],
  ): Expr.Closure =
    val name = freshName()
    // `body` may itself contain further-nested `FollowingSteps` (e.g. a
    // `react`-inside-`react` call site) — lower those first, since `extra`
    // algorithms are appended after this pass's single `algos.map` and are
    // never themselves passed back through `transform`.
    val lowered = transform(body, freshName, extra)
    val captured = (FreeVarAnalysis.freeVars(lowered) -- params).toList.sorted
    val wjiParams = params.zipWithIndex.map {
      case (p, i) =>
        WjiParam(s"|$p|", variadic = variadicLast && i == params.size - 1)
    }
    extra += Algorithm(None, Some(name), wjiParams, "", lowered)
    Expr.Closure(name, captured)

  private def transform(
    instrs: List[Instr],
    freshName: () => String,
    extra: collection.mutable.ListBuffer[Algorithm],
  ): List[Instr] = instrs match
    case Nil => Nil
    case Instr.Let(lhs, Expr.FollowingSteps(params, variadicLast), body) ::
        rest if body.nonEmpty =>
      Instr.Let(
        lhs,
        hoist(params, variadicLast, body, freshName, extra),
        Nil,
      ) ::
      transform(rest, freshName, extra)
    case (p: Instr.Perform) :: rest
        if p.body.nonEmpty &&
        p.args.exists {
          case Expr.FollowingSteps(_, _) => true; case _ => false
        } =>
      val (params, variadicLast) =
        p.args.collectFirst { case Expr.FollowingSteps(ps, vl) => (ps, vl) }.get
      val closure = hoist(params, variadicLast, p.body, freshName, extra)
      val newArgs = p.args.map {
        case Expr.FollowingSteps(_, _) => closure
        case other                     => other
      }
      p.copy(args = newArgs, body = Nil) :: transform(rest, freshName, extra)
    case instr :: rest =>
      instr.mapBody(transform(_, freshName, extra)) ::
      transform(rest, freshName, extra)
