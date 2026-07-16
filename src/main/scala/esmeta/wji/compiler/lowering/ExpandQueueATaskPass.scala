package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Expr, Instr}
import esmeta.wji.lang.Instr.PerformOutcome

/** Rewrites a `"[=Queue a task=] on |taskSource|, if provided, to perform the
  * following steps: ..."` step into a call to the manual
  * `HostEnqueuePromiseJob` hook
  * (`resources/manuals/funcs/HostEnqueuePromiseJob.ir`), mirroring how ECMA-262
  * itself queues an Abstract Closure as a Job:
  *
  * {{{
  *   Perform("[=Queue a task=]", [Var("taskSource"), Closure(jobName, captured)], Discard)
  * }}}
  * becomes:
  * {{{
  *   Perform("HostEnqueuePromiseJob", [Closure(jobName, captured), SpecTerm("current Realm")], Discard)
  * }}}
  * The original `|taskSource|` argument is dropped: `HostEnqueuePromiseJob` has
  * no notion of an HTML task source, so this is only an approximation of "queue
  * a task" (good enough to make the job actually run), not a faithful
  * mechanization of the HTML event-loop task-queueing semantics.
  *
  * Purely a syntactic rewrite — the `Closure` (and the [[Algorithm]] it
  * references) already exists by the time this pass runs:
  * [[ExpandFollowingStepsPass]] (which runs immediately before this pass)
  * hoists the step's `"...to perform the following steps:"` clause — parsed as
  * one of this `Perform`'s `args` — into one, so `args` is guaranteed to
  * contain a `Closure` here.
  */
object ExpandQueueATaskPass extends LoweringPass:

  /** Requires:
    *   - [[ExpandFollowingStepsPass]]: needs `args` to already contain a
    *     `Closure`, not a `FollowingSteps` placeholder — `p.args.collectFirst {
    *     case c: Expr.Closure => c }.get` crashes outright if it doesn't.
    */
  override def requires: Set[LoweringPass] = Set(ExpandFollowingStepsPass)
  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = transform(a.body)))

  private def isQueueATask(func: String): Boolean =
    func
      .stripPrefix("[=")
      .stripSuffix("=]")
      .replace('_', ' ')
      .trim
      .equalsIgnoreCase("queue a task")

  private def transform(instrs: List[Instr]): List[Instr] = instrs match
    case Nil => Nil
    case (p: Instr.Perform) :: rest if isQueueATask(p.func) =>
      val closure = p.args.collectFirst { case c: Expr.Closure => c }.get
      Instr.Perform(
        "HostEnqueuePromiseJob",
        List(closure, Expr.SpecTerm("current Realm")),
        PerformOutcome.Discard,
      ) :: transform(rest)
    case instr :: rest => instr.mapBody(transform) :: transform(rest)
