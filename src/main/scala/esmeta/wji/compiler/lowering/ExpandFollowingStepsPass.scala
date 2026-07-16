package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}

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
  * Runs late, right before [[NormalizeAlgoNamePass]] (and before
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

  /** formal parameter names `BuiltinCallOrConstruct`'s hand-patched IR
    * (`manuals/rule.json`'s rule for step-call-builtin-function-result) always
    * calls `func.__CODE__` with, regardless of what parameters the underlying
    * `behaviour` closure itself declares — see [[isBuiltinBehaviour]].
    */
  private val BuiltinParams = List("thisArgument", "argumentsList", "newTarget")

  /** whether the closure being hoisted for `varName` is passed as
    * `CreateBuiltinFunction`'s `behaviour` argument among its hoisting site's
    * sibling steps (`rest`) — the shape every WJI spec text with this pattern
    * uses so far, e.g. WebIDL's `react`: `Let onFulfilledSteps be the following
    * steps given argument V: ...` immediately followed by `Let onFulfilled be
    * CreateBuiltinFunction(onFulfilledSteps, 1, "", « »).` in the very same
    * algorithm.
    *
    * This matters because `CreateBuiltinFunction` (mainline's real algorithm,
    * hand-patched into `manuals/rule.json`) stores the closure verbatim as
    * `func.__CODE__`, but `BuiltinCallOrConstruct` (also mainline, same manual
    * file) always invokes it as `func.__CODE__(thisArgument, argumentsList,
    * newTarget)` — a fixed 3-argument calling convention. A closure hoisted
    * with its own spec-declared parameters (e.g. just `V`) would blow up on
    * that call with a `RemainingArgs` interpreter error. Mirrors mainline
    * `esmeta.compiler.Compiler`'s `fixClosurePrefixAOs` / `getBuiltinPrefix`,
    * which solves the exact same gap for ECMA-262's own Abstract Closures (e.g.
    * `NewPromiseCapability`'s resolve/reject functions) by giving them the
    * builtin signature plus a prefix that unpacks `argumentsList` positionally
    * into the real parameter names.
    */
  private def isBuiltinBehaviour(varName: String, rest: List[Instr]): Boolean =
    rest.exists {
      case Instr.Perform("CreateBuiltinFunction", args, _, _) =>
        args.headOption.contains(Expr.Var(varName))
      case _ => false
    }

  /** the `params.zipWithIndex` prefix instructions that unpack a builtin's
    * `argumentsList` positionally into the Abstract Closure's own declared
    * parameter names, defaulting to `undefined` past the end of the list —
    * mirrors mainline `Compiler.getBuiltinPrefix`'s `Param(name, _, Normal)`
    * case (this pass never sees a WJI closure with a variadic/optional
    * parameter, so there's no need for the other cases that function has).
    */
  private def unpackArgumentsList(params: List[String]): List[Instr] =
    params.zipWithIndex.map {
      case (p, i) =>
        Instr.IfChain(
          List(
            Cond.Compare(
              Expr.Num(i.toString),
              Cond.CompareOp.Lt,
              Expr.Length(Expr.Var("argumentsList")),
            ) -> List(
              Instr.Let(
                Expr.Var(p),
                Expr.Index(Expr.Var("argumentsList"), Expr.Num(i.toString)),
              ),
            ),
          ),
          List(Instr.Let(Expr.Var(p), Expr.SpecTerm("undefined"))),
        )
    }

  private def hoist(
    params: List[String],
    body: List[Instr],
    freshName: () => String,
    extra: collection.mutable.ListBuffer[Algorithm],
    asBuiltinBehaviour: Boolean = false,
  ): Expr.Closure =
    val name = freshName()
    // `body` may itself contain further-nested `FollowingSteps` (e.g. a
    // `react`-inside-`react` call site) — lower those first, since `extra`
    // algorithms are appended after this pass's single `algos.map` and are
    // never themselves passed back through `transform`.
    val lowered = transform(body, freshName, extra)
    val captured = (FreeVarAnalysis.freeVars(lowered) -- params).toList.sorted
    val (formalParams, fullBody) =
      if asBuiltinBehaviour then
        (BuiltinParams, unpackArgumentsList(params) ++ lowered)
      else (params, lowered)
    extra += Algorithm(
      None,
      Some(name),
      formalParams.map(p => s"|$p|"),
      "",
      fullBody,
    )
    Expr.Closure(name, captured)

  private def transform(
    instrs: List[Instr],
    freshName: () => String,
    extra: collection.mutable.ListBuffer[Algorithm],
  ): List[Instr] = instrs match
    case Nil => Nil
    case Instr.Let(lhs, Expr.FollowingSteps(params), body) :: rest
        if body.nonEmpty =>
      val asBuiltinBehaviour = lhs match
        case Expr.Var(name) => isBuiltinBehaviour(name, rest)
        case _              => false
      Instr.Let(
        lhs,
        hoist(params, body, freshName, extra, asBuiltinBehaviour),
        Nil,
      ) :: transform(rest, freshName, extra)
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
