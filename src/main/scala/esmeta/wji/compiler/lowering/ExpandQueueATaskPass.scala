package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Expr, Instr}
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
  *   Perform("HostEnqueuePromiseJob", [Closure(jobName, Nil, captured), SpecTerm("current Realm")], Discard)
  * }}}
  * where `substeps` is split off into a fresh 0-parameter top-level
  * [[Algorithm]] named `jobName`, and `captured` is every free variable
  * `substeps` references (computed by [[FreeVarAnalysis]], since — unlike
  * ECMA-262's "a new Job Abstract Closure ... that captures X and Y" — the WASM
  * JS-API spec's "queue a task" prose never spells out a capture list). The
  * original `|taskSource|` argument is dropped: `HostEnqueuePromiseJob` has no
  * notion of an HTML task source, so this is only an approximation of "queue a
  * task" (good enough to make the job actually run), not a faithful
  * mechanization of the HTML event-loop task-queueing semantics.
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
      val captured = FreeVarAnalysis.freeVars(p.body).toList.sorted
      extra += Algorithm(None, Some(jobName), Nil, "", p.body)
      val job = Expr.Closure(jobName, Nil, captured)
      Instr.Perform(
        "HostEnqueuePromiseJob",
        List(job, Expr.SpecTerm("current Realm")),
        PerformOutcome.Discard,
      ) :: transform(rest, extra)
    case instr :: rest =>
      instr.mapBody(transform(_, extra)) :: transform(rest, extra)
