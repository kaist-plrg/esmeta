package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Expr, Instr}

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
  * [[esmeta.wji.lang.ExprParser]] only ever sees the one prose string
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
  * Generic over instruction shape: a `FollowingSteps` may sit directly as a
  * `Let`'s RHS or among a `Perform`'s `args`; adding a third shape (e.g. a
  * `Set`/`Return` RHS, if a future spec phrasing needs it) is a same-shaped
  * addition to `transform` below, not a new pass.
  *
  * Runs late, right before [[ReplaceSpaceWithUnderscore]] (and before
  * [[ExpandQueueATaskPass]], which depends on the `Closure` this pass leaves
  * behind in a "queue a task" `Perform`'s `args`): `body` rides through every
  * earlier pass as ordinary nested `Let.body`/`Perform.body` content (every
  * pass already recurses into it via `Instr.mapBody`), so by the time this pass
  * sees it, it's already fully lowered.
  */
object ExpandFollowingStepsPass extends LoweringPass:
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
    body: List[Instr],
    freshName: () => String,
    extra: collection.mutable.ListBuffer[Algorithm],
  ): Expr.Closure =
    val name = freshName()
    val captured = (FreeVarAnalysis.freeVars(body) -- params).toList.sorted
    extra += Algorithm(None, Some(name), params.map(p => s"|$p|"), "", body)
    Expr.Closure(name, captured)

  private def transform(
    instrs: List[Instr],
    freshName: () => String,
    extra: collection.mutable.ListBuffer[Algorithm],
  ): List[Instr] = instrs match
    case Nil => Nil
    case Instr.Let(lhs, Expr.FollowingSteps(params), body) :: rest
        if body.nonEmpty =>
      Instr.Let(lhs, hoist(params, body, freshName, extra), Nil) ::
      transform(rest, freshName, extra)
    case (p: Instr.Perform) :: rest
        if p.body.nonEmpty &&
        p.args.exists {
          case Expr.FollowingSteps(_) => true; case _ => false
        } =>
      val params =
        p.args.collectFirst { case Expr.FollowingSteps(ps) => ps }.get
      val closure = hoist(params, p.body, freshName, extra)
      val newArgs = p.args.map {
        case Expr.FollowingSteps(_) => closure
        case other                  => other
      }
      p.copy(args = newArgs, body = Nil) :: transform(rest, freshName, extra)
    case instr :: rest =>
      instr.mapBody(transform(_, freshName, extra)) ::
      transform(rest, freshName, extra)
