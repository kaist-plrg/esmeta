package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr, WjiParam}
import esmeta.wji.lang.walker.{UnitWalker, Walker}
import esmeta.error.UnsupportedSpecShape

/** Rewrites every `Expr.FollowingSteps(params)` placeholder — "the following
  * steps ...:" (see [[esmeta.wji.lang.Expr.FollowingSteps]]) — into a `Closure`
  * referencing a freshly split-off top-level [[Algorithm]]:
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
  * Assumes at most one `Expr.FollowingSteps` per instruction (throws if
  * [[findOne]] finds more), and makes no assumption about exactly where within
  * that one instruction's own value fields it sits — unlike an earlier version
  * of this pass, which only matched it as the immediate top-level value of a
  * `Let`'s RHS or a `Perform`'s `args`. It's instead searched for recursively
  * through every value-position `Expr`/`Cond` field ([[searchExprs]]), so e.g.
  * one nested inside an as-yet-unexpanded `Expr.Conditional` branch (`Let(lhs,
  * Conditional([(cond, other)], Some(FollowingSteps(...))), body)`, webidl's "X
  * if COND, or the following steps otherwise:" idiom, category I-G/#3-1) is
  * found and hoisted directly, without needing [[ExpandConditionalPass]] to
  * have already unwrapped the `Conditional` into an `Instr.IfChain` first — so,
  * unlike before, this pass no longer needs to run after
  * `ExpandConditionalPass` in [[Lowering.pipeline]] (nor does
  * `ExpandConditionalPass` need to run after this one); the two are now
  * order-independent with respect to each other. (If `ExpandConditionalPass`
  * does still run afterward, its own dedicated `FollowingSteps`-in-`Cond`
  * handling — see its class doc — simply never fires, since by then the
  * `FollowingSteps` has already been replaced by a plain `Closure`.)
  *
  * Only hoists from instructions whose own `body` exists *solely* because such
  * a phrase attached sub-items to that one line — [[hoistIfPresent]] covers
  * `Let`, `Set`, `Return`, `Throw`, `Assert`, `Append`, `Remove`, `Pop`,
  * `Perform`, and `PerformClosure`. Deliberately excludes every control-flow
  * instruction
  * (`If`/`ElseIf`/`Else`/`ForEach`/`For`/`While`/`RunInParallel`/`IfChain`):
  * for those, `body` is a real branch/loop body, and reinterpreting it as
  * closure substeps just because a `FollowingSteps` happened to appear in, say,
  * an `If`'s `cond` would silently discard actual control flow. No real spec
  * occurrence has ever needed this, so rather than let it silently mishandle
  * such a case, [[checkNotInControlFlowHeader]] explicitly checks
  * `If`/`ElseIf`/`While`'s `cond` and `ForEach`/`For`'s `collection` and throws
  * immediately if a `FollowingSteps` turns up there.
  *
  * Also never searches a *binder* position — `Let`/`Set`/`Pop`'s `lhs`,
  * `ForEach`/`For`'s `elem` — since a `FollowingSteps` can never sensibly be
  * one; [[checkNotInBinder]] checks those explicitly and throws immediately and
  * specifically if one ever turns up there, rather than leaving it for the
  * generic postcondition to report only vaguely.
  *
  * Runs right after the other pure structural eliminations, and in particular
  * before [[MarkCompletionAlgorithmsPass]] (see that pass's own `requires`
  * doc): a closure's substeps need to already be split off into their own
  * top-level `Algorithm` for that completion-record analysis to consider them
  * independently of the algorithm they were textually nested in, rather than
  * silently never analyzing them at all. Since `body` isn't fully lowered yet
  * at that point, `hoist`'s own [[FreeVarAnalysis]] call has to be able to
  * compute captured variables correctly against not-yet-desugared idioms that
  * implicitly bind a name (e.g. `Cond.Throws`'s `exception`) — see
  * `FreeVarAnalysis`'s own doc for how it handles that.
  *
  * Category: Structural desugaring — Elimination.
  */
object ExpandFollowingStepsPass extends LoweringPass:

  override def postconditions: List[Condition] = List(
    Condition(
      "no FollowingSteps remains unhoisted (only a statement instruction's " +
      "own value fields, excluding binder positions, with a non-empty " +
      "owning body are handled)",
      _.forall(a =>
        !AstQuery.existsExpr(a.body)(_.isInstanceOf[Expr.FollowingSteps]),
      ),
    ),
  )

  def run(algos: List[Algorithm]): List[Algorithm] =
    val extra = collection.mutable.ListBuffer.empty[Algorithm]
    val rewritten = algos.map { a =>
      val label = a.name.orElse(a.id).getOrElse("algo")
      var counter = 0
      def freshName(): String =
        counter += 1; s"${label}_closure$counter"
      a.copy(body = transform(a.body, label, freshName, extra))
    }
    rewritten ++ extra.toList

  private def hoist(
    params: List[String],
    variadicLast: Boolean,
    body: List[Instr],
    label: String,
    freshName: () => String,
    extra: collection.mutable.ListBuffer[Algorithm],
  ): Expr.Closure =
    val name = freshName()
    // `body` may itself contain further-nested FollowingSteps (e.g. a
    // `react`-inside-`react` call site) — lower those first, since `extra`
    // algorithms are appended after this pass's single `algos.map` and are
    // never themselves passed back through `transform`.
    val lowered = transform(body, label, freshName, extra)
    val captured = (FreeVarAnalysis.freeVars(lowered) -- params).toList.sorted
    val wjiParams = params.zipWithIndex.map {
      case (p, i) =>
        WjiParam(s"|$p|", variadic = variadicLast && i == params.size - 1)
    }
    extra += Algorithm(None, Some(name), wjiParams, "", lowered)
    Expr.Closure(name, captured)

  /** Finds the lone `Expr.FollowingSteps` reachable from `exprs`, at any
    * nesting depth — e.g. inside an `Expr.Conditional` branch — via the same
    * recursive descent [[UnitWalker]] gives every other `Expr` node. Throws if
    * more than one turns up, per this pass's one-per-instruction assumption
    * (see class doc).
    */
  private def findOne(
    exprs: List[Expr],
    label: String,
  ): Option[(List[String], Boolean)] =
    val found = collection.mutable.ListBuffer.empty[(List[String], Boolean)]
    val finder = new UnitWalker:
      override def walk(expr: Expr): Unit = expr match
        case Expr.FollowingSteps(params, variadicLast) =>
          found += ((params, variadicLast))
        case other => super.walk(other)
    exprs.foreach(finder.walk)
    found.toList match
      case Nil        => None
      case one :: Nil => Some(one)
      case _ =>
        throw UnsupportedSpecShape(
          "ExpandFollowingStepsPass",
          s"multiple FollowingSteps found in one instruction (expected at " +
          s"most one), in $label",
        )

  private def findOneInCond(
    cond: Cond,
    label: String,
  ): Option[(List[String], Boolean)] =
    val found = collection.mutable.ListBuffer.empty[(List[String], Boolean)]
    val finder = new UnitWalker:
      override def walk(expr: Expr): Unit = expr match
        case Expr.FollowingSteps(params, variadicLast) =>
          found += ((params, variadicLast))
        case other => super.walk(other)
    finder.walk(cond)
    found.toList match
      case Nil        => None
      case one :: Nil => Some(one)
      case _ =>
        throw UnsupportedSpecShape(
          "ExpandFollowingStepsPass",
          s"multiple FollowingSteps found in one instruction (expected at " +
          s"most one), in $label",
        )

  /** Substitutes the lone `Expr.FollowingSteps` reachable from `expr` with
    * `closure` — safe because a prior [[findOne]]/[[findOneInCond]] call
    * already established exactly one occurrence exists among the fields being
    * rewritten.
    */
  private def substitute(expr: Expr, closure: Expr.Closure): Expr =
    val rewriter = new Walker:
      override def walk(expr: Expr): Expr = expr match
        case _: Expr.FollowingSteps => closure
        case other                  => super.walk(other)
    rewriter.walk(expr)

  private def substituteCond(cond: Cond, closure: Expr.Closure): Cond =
    val rewriter = new Walker:
      override def walk(expr: Expr): Expr = expr match
        case _: Expr.FollowingSteps => closure
        case other                  => super.walk(other)
    rewriter.walk(cond)

  /** Throws if `instr`'s own binder position(s) — `lhs` for `Let`/`Set`/`Pop`,
    * `elem` for `ForEach`/`For` — contain a `FollowingSteps`; see class doc.
    */
  private def checkNotInBinder(instr: Instr, label: String): Unit =
    def check(position: String, e: Expr): Unit =
      if findOne(List(e), label).isDefined then
        throw UnsupportedSpecShape(
          "ExpandFollowingStepsPass",
          s"FollowingSteps found in $position position (never a valid " +
          s"binder), in $label",
        )
    instr match
      case i: Instr.Let     => check("Let's lhs", i.lhs)
      case i: Instr.Set     => check("Set's lhs", i.lhs)
      case i: Instr.Pop     => check("Pop's lhs", i.lhs)
      case i: Instr.ForEach => check("ForEach's elem", i.elem)
      case i: Instr.For     => check("For's elem", i.elem)
      case _                => ()

  /** Throws if a control-flow instruction's own `If`/`ElseIf`/`While`'s `cond`,
    * or `ForEach`/`For`'s `collection`, contains a `FollowingSteps` — that
    * instruction's `body` is the real branch/loop body, not closure substeps,
    * so hoisting it away would silently discard control flow; see class doc.
    */
  private def checkNotInControlFlowHeader(instr: Instr, label: String): Unit =
    def fail(position: String): Nothing =
      throw UnsupportedSpecShape(
        "ExpandFollowingStepsPass",
        s"FollowingSteps found in $position (its body is real branch/loop " +
        s"content, not closure substeps to hoist), in $label",
      )
    def checkExpr(position: String, e: Expr): Unit =
      if findOne(List(e), label).isDefined then fail(position)
    def checkCond(position: String, c: Cond): Unit =
      if findOneInCond(c, label).isDefined then fail(position)
    instr match
      case i: Instr.If      => checkCond("If's cond", i.cond)
      case i: Instr.ElseIf  => checkCond("ElseIf's cond", i.cond)
      case i: Instr.While   => checkCond("While's cond", i.cond)
      case i: Instr.ForEach => checkExpr("ForEach's collection", i.collection)
      case i: Instr.For     => checkExpr("For's collection", i.collection)
      case _                => ()

  /** Searches `instr`'s own value fields (never a binder position, never `body`
    * itself) for a lone `FollowingSteps` and, if `instr.body` is non-empty,
    * hoists it — returning the rewritten instruction with that occurrence
    * replaced by the resulting `Closure` and `body` cleared to `Nil`. `None` if
    * `instr` isn't one of the statement shapes this pass handles, has an empty
    * `body`, or has no `FollowingSteps` in its (searched) fields at all.
    */
  private def hoistIfPresent(
    instr: Instr,
    label: String,
    freshName: () => String,
    extra: collection.mutable.ListBuffer[Algorithm],
  ): Option[Instr] =
    if instr.body.isEmpty then None
    else
      def go(ps: List[String], vl: Boolean) =
        hoist(ps, vl, instr.body, label, freshName, extra)
      instr match
        case i: Instr.Let =>
          findOne(List(i.expr), label).map { (ps, vl) =>
            val c = go(ps, vl)
            i.copy(expr = substitute(i.expr, c), body = Nil)
          }
        case i: Instr.Set =>
          findOne(List(i.expr), label).map { (ps, vl) =>
            val c = go(ps, vl)
            i.copy(expr = substitute(i.expr, c), body = Nil)
          }
        case i: Instr.Return =>
          findOne(i.expr.toList, label).map { (ps, vl) =>
            val c = go(ps, vl)
            i.copy(expr = i.expr.map(substitute(_, c)), body = Nil)
          }
        case i: Instr.Throw =>
          findOne(List(i.target), label).map { (ps, vl) =>
            val c = go(ps, vl)
            i.copy(target = substitute(i.target, c), body = Nil)
          }
        case i: Instr.Assert =>
          findOneInCond(i.cond, label).map { (ps, vl) =>
            val c = go(ps, vl)
            i.copy(cond = substituteCond(i.cond, c), body = Nil)
          }
        case i: Instr.Append =>
          findOne(List(i.item, i.collection), label).map { (ps, vl) =>
            val c = go(ps, vl)
            i.copy(
              item = substitute(i.item, c),
              collection = substitute(i.collection, c),
              body = Nil,
            )
          }
        case i: Instr.Remove =>
          findOne(List(i.list), label).map { (ps, vl) =>
            val c = go(ps, vl)
            i.copy(list = substitute(i.list, c), body = Nil)
          }
        case i: Instr.Pop =>
          findOne(List(i.list), label).map { (ps, vl) =>
            val c = go(ps, vl)
            i.copy(list = substitute(i.list, c), body = Nil)
          }
        case i: Instr.Perform =>
          findOne(i.args, label).map { (ps, vl) =>
            val c = go(ps, vl)
            i.copy(args = i.args.map(substitute(_, c)), body = Nil)
          }
        case i: Instr.Try =>
          findOne(List(i.call), label).map { (ps, vl) =>
            val c = go(ps, vl)
            i.copy(call = substitute(i.call, c), body = Nil)
          }
        case i: Instr.PerformClosure =>
          findOne(i.closure :: i.args, label).map { (ps, vl) =>
            val c = go(ps, vl)
            i.copy(
              closure = substitute(i.closure, c),
              args = i.args.map(substitute(_, c)),
              body = Nil,
            )
          }
        case _ => None

  private def transform(
    instrs: List[Instr],
    label: String,
    freshName: () => String,
    extra: collection.mutable.ListBuffer[Algorithm],
  ): List[Instr] =
    instrs.map { instr =>
      checkNotInBinder(instr, label)
      checkNotInControlFlowHeader(instr, label)
      hoistIfPresent(instr, label, freshName, extra).getOrElse(
        instr.mapBody(transform(_, label, freshName, extra)),
      )
    }
