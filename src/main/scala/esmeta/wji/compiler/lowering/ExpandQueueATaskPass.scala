package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}
import esmeta.wji.lang.Instr.PerformOutcome

/** Rewrites a `"[=Queue a task=] on |taskSource|, if provided, to perform the
  * following steps: ..."` step — parsed by [[InstrParser]] as an ordinary
  * `Instr.Perform` whose `func` names the "queue a task" dfn and whose `body`
  * holds the substeps — into a call to the manual `HostEnqueuePromiseJob` hook
  * (`resources/manuals/funcs/HostEnqueuePromiseJob.ir`), mirroring how ECMA-262
  * itself queues an Abstract Closure as a Job:
  *
  * {{{
  *   Perform("[=Queue a task=]", [|taskSource|], Discard, substeps)
  * }}}
  * becomes:
  * {{{
  *   Perform("HostEnqueuePromiseJob", [Closure(jobName, captured), SpecTerm("current Realm")], Discard)
  * }}}
  * where `substeps` is split off into a fresh 0-parameter top-level
  * [[Algorithm]] named `jobName`, and `captured` is every free variable
  * `substeps` references (computed here, since — unlike ECMA-262's "a new Job
  * Abstract Closure ... that captures X and Y" — the WASM JS-API spec's "queue
  * a task" prose never spells out a capture list). The original `|taskSource|`
  * argument is dropped: `HostEnqueuePromiseJob` has no notion of an HTML task
  * source, so this is only an approximation of "queue a task" (good enough to
  * make the job actually run), not a faithful mechanization of the HTML
  * event-loop task-queueing semantics.
  *
  * Runs after every other lowering pass except [[ReplaceSpaceWithUnderscore]]
  * (which stays last): `substeps` rides through every earlier pass as ordinary
  * nested `Perform.body` content (every pass already recurses into it via
  * `Instr.mapBody`), so by the time this pass sees it, it's already fully
  * lowered — it only needs to be lifted out into its own [[Algorithm]], not
  * lowered itself. Running before [[ReplaceSpaceWithUnderscore]] lets that pass
  * normalize the newly split-off [[Algorithm]] too, the same as any other.
  */
object ExpandQueueATaskPass extends LoweringPass:
  private var counter = 0
  private def freshJobName(): String =
    counter += 1; s"_queuetaskjob$counter"

  def run(algos: List[Algorithm]): List[Algorithm] =
    val extra = collection.mutable.ListBuffer.empty[Algorithm]
    val rewritten = algos.map(a => a.copy(body = transform(a.body, extra)))
    rewritten ++ extra.toList

  private def isQueueATask(func: String): Boolean =
    func
      .stripPrefix("[=")
      .stripSuffix("=]")
      .replace('_', ' ')
      .trim
      .equalsIgnoreCase("queue a task")

  private def transform(
    instrs: List[Instr],
    extra: collection.mutable.ListBuffer[Algorithm],
  ): List[Instr] = instrs match
    case Nil => Nil
    case (p: Instr.Perform) :: rest
        if isQueueATask(p.func) && p.body.nonEmpty =>
      val jobName = freshJobName()
      val captured = freeVars(p.body).toList.sorted
      extra += Algorithm(None, Some(jobName), Nil, "", p.body)
      val job = Expr.Closure(jobName, captured)
      Instr.Perform(
        "HostEnqueuePromiseJob",
        List(job, Expr.SpecTerm("current Realm")),
        PerformOutcome.Discard,
      ) :: transform(rest, extra)
    case instr :: rest =>
      instr.mapBody(transform(_, extra)) :: transform(rest, extra)

  // ── Free-variable analysis ───────────────────────────────────────────────────

  private def stripPipes(s: String): String =
    s.stripPrefix("|").stripSuffix("|")

  /** Every `Var` name `instrs` references that isn't bound somewhere inside
    * `instrs` itself (by a `Let`, a `ForEach` element, or a `Perform`'s
    * `BindResult`). Position-insensitive (a name bound anywhere in `instrs` is
    * treated as bound throughout, not just after its binding site) — spec prose
    * never shadows an outer variable with a same-named local, so this is a safe
    * simplification.
    */
  private def freeVars(instrs: List[Instr]): Set[String] =
    referencedVars(instrs) -- boundVars(instrs)

  private def boundVars(instrs: List[Instr]): Set[String] =
    instrs.flatMap(boundVarsInstr).toSet

  private def bindableNames(expr: Expr): Set[String] = expr match
    case Expr.Var(name)    => Set(name)
    case Expr.Tuple(elems) => elems.flatMap(bindableNames).toSet
    case _                 => Set.empty

  private def boundVarsInstr(instr: Instr): Set[String] =
    val own: Set[String] = instr match
      case Instr.Let(lhs, _, _)      => bindableNames(lhs)
      case Instr.ForEach(elem, _, _) => bindableNames(elem)
      case p: Instr.Perform =>
        p.outcome match
          case PerformOutcome.BindResult(v) => Set(stripPipes(v))
          case _                            => Set.empty
      case _ => Set.empty
    val nested: Set[String] = instr match
      case Instr.IfChain(branches, fallback) =>
        branches.flatMap((_, b) => boundVars(b)).toSet ++ boundVars(fallback)
      case other => boundVars(other.body)
    own ++ nested

  private def referencedVars(instrs: List[Instr]): Set[String] =
    instrs.flatMap(referencedVarsInstr).toSet

  private def referencedVarsInstr(instr: Instr): Set[String] =
    val here: Set[String] = instr match
      case Instr.Let(_, rhs, _)            => varsOf(rhs)
      case Instr.Set(lhs, rhs, _)          => varsOf(lhs) ++ varsOf(rhs)
      case Instr.If(cond, _)               => varsOfCond(cond)
      case Instr.ElseIf(cond, _)           => varsOfCond(cond)
      case Instr.Return(exprOpt, _)        => exprOpt.toSet.flatMap(varsOf)
      case Instr.Assert(cond, _)           => varsOfCond(cond)
      case Instr.ForEach(_, collection, _) => varsOf(collection)
      case Instr.While(cond, _)            => varsOfCond(cond)
      case Instr.Append(item, collection, _) =>
        varsOf(item) ++ varsOf(collection)
      case p: Instr.Perform => p.args.flatMap(varsOf).toSet
      case Instr.IfChain(branches, _) =>
        branches.flatMap((c, _) => varsOfCond(c)).toSet
      case _ => Set.empty
    val nested: Set[String] = instr match
      case Instr.IfChain(branches, fallback) =>
        branches.flatMap((_, b) => referencedVars(b)).toSet ++ referencedVars(
          fallback,
        )
      case other => referencedVars(other.body)
    here ++ nested

  private def varsOf(expr: Expr): Set[String] = expr match
    case Expr.Var(name)         => Set(name)
    case Expr.Field(base, _)    => varsOf(base)
    case Expr.Index(base, key)  => varsOf(base) ++ varsOf(key)
    case Expr.Link(_, args)     => args.flatMap(varsOf).toSet
    case Expr.AlgoCall(_, args) => args.flatMap(varsOf).toSet
    case Expr.JSCall(_, args)   => args.flatMap(varsOf).toSet
    case Expr.Abrupt(_, e)      => varsOf(e)
    case Expr.List_(elems)      => elems.flatMap(varsOf).toSet
    case Expr.Map_(entries) =>
      entries.flatMap((k, v) => varsOf(k) ++ varsOf(v)).toSet
    case Expr.Length(e)            => varsOf(e)
    case Expr.BinOp(l, _, r)       => varsOf(l) ++ varsOf(r)
    case Expr.Pow(base, exp)       => varsOf(base) ++ varsOf(exp)
    case Expr.Neg(e)               => varsOf(e)
    case Expr.AsMath(e)            => varsOf(e)
    case Expr.Tuple(elems)         => elems.flatMap(varsOf).toSet
    case Expr.Closure(_, captured) => captured.toSet
    case _                         => Set.empty

  private def varsOfCond(cond: Cond): Set[String] = cond match
    case Cond.Eq(l, r, _)         => varsOf(l) ++ varsOf(r)
    case Cond.Compare(l, _, r)    => varsOf(l) ++ varsOf(r)
    case Cond.HasField(e, _)      => varsOf(e)
    case Cond.Implements(e, _, _) => varsOf(e)
    case Cond.IsOfForm(e, f, condOpt, _) =>
      varsOf(e) ++ varsOf(f) ++ condOpt.toSet.flatMap(varsOfCond)
    case Cond.Matches(l, _, r, _) => varsOf(l) ++ varsOf(r)
    case Cond.IsMissing(e, _)     => varsOf(e)
    case Cond.HasSlot(e, _, _)    => varsOf(e)
    case Cond.IsType(e, _, _)     => varsOf(e)
    case Cond.And(l, r)           => varsOfCond(l) ++ varsOfCond(r)
    case Cond.Or(l, r)            => varsOfCond(l) ++ varsOfCond(r)
    case Cond.Abbreviated(e)      => varsOf(e)
    case _                        => Set.empty
