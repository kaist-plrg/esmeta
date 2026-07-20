package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Instr}

/** Elides WHATWG HTML's job-callback bookkeeping hooks — "Prepare to run
  * script" / "Prepare to run a callback" / "Clean up after running a callback"
  * / "Clean up after running script" — only ever seen wrapping `create a host
  * function`'s inner call (js-api/index.bs:1351-1355).
  *
  * ECMA-262 itself deliberately leaves all of this host-defined (see
  * `ExprParser.CrossSpecRef`'s doc for the "incumbent settings object"/
  * "realm/settings object" values these hooks are given): its own *default*
  * `HostCallJobCallback` implementation — used by any host that isn't a web
  * browser, per ecma262/spec.html's own wording — just invokes the callback
  * directly, with none of this bookkeeping. So these `Perform` steps are
  * dropped entirely here, mirroring that default.
  *
  * Kept as a lowering pass rather than a parser-level special case so the
  * initial (pre-lowering) parse stays a faithful structural mirror of the spec
  * prose — a real `Perform` targeting these names, not already-elided — and
  * this scoping decision (as opposed to a syntax-recognition one) stays visible
  * as its own, separately-inspectable transformation step.
  *
  * Category: Housekeeping.
  */
object ElideHtmlHostHooksPass extends LoweringPass:

  /** Must precede:
    *   - [[NormalizeAlgoNamePass]]: `elided` matches `Instr.Perform.func`
    *     against space-separated phrases; `NormalizeAlgoNamePass` later
    *     replaces every such space with an underscore, which would silently
    *     stop every entry from matching. Unlike [[ExpandQueueATaskPass]]'s
    *     equivalent string match, `elided` isn't defensively normalized against
    *     that substitution, so the order is load-bearing.
    */
  override def mustPrecede: Set[LoweringPass] = Set(NormalizeAlgoNamePass)

  private val elided: Set[String] = Set(
    "prepare to run script",
    "prepare to run a callback",
    "clean up after running a callback",
    "clean up after running script",
  )

  private def stripLink(link: String): String =
    link.stripPrefix("[=").stripSuffix("=]").trim.toLowerCase

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = transform(a.body)))

  private def transform(instrs: List[Instr]): List[Instr] =
    instrs.flatMap {
      case p: Instr.Perform if elided.contains(stripLink(p.func)) =>
        transform(p.body)
      case other => List(other.mapBody(transform))
    }
