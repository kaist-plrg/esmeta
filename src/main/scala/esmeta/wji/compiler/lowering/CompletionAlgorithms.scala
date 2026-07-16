package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}
import esmeta.wji.lang.Instr.PerformOutcome

/** Computes which WJI algorithms must return an ECMA-262 Completion Record,
  * mirroring the Infra Standard's own convention: an operation that can
  * abruptly complete, and isn't caught, automatically re-throws out of its
  * caller too — so "does algorithm A return a completion" isn't a fact about A
  * alone, it's a fixed point over the whole call graph.
  *
  * A name is in the result when, anywhere in its own body (recursively through
  * nested branches, but not through other algorithms):
  *   - it has a `Throw` step, or a `?`/`!`-marked call (`Expr.Abrupt`) — a
  *     direct, local signal the spec text itself gives (no transitivity needed
  *     to know these, unlike the case below); or
  *   - it makes an *unmarked* call to another algorithm already known to be in
  *     the set, and that call's result isn't itself manually handled — either
  *     inspected directly (a `.[[Type]]`/`.Type` field read anywhere afterward,
  *     meaning the algorithm absorbs the completion into its own convention,
  *     e.g. `create a host function`'s hostfunc closure reading
  *     `result.[[Type]]` and returning a plain `result.[[Value]]` itself), or
  *     caught via the "If this throws an exception, catch it, ..." idiom
  *     (`Cond.Throws`, `ExpandThrowsPass`'s pattern, already robust to either
  *     case on its own).
  *
  * This is only ever computed once, from the algorithms as they exist right
  * after `ResolveLinksPass` (so call targets are already resolved names) and
  * before any later pass changes shape — in particular before
  * `GroupIfChainPass` (so an `Cond.Throws` catch is still a bare `Instr.If`
  * sibling, not yet a grouped `IfChain`) and before `ExpandAbruptPass` (so
  * `Expr.Abrupt` markers are still present to detect). See
  * [[MarkCompletionAlgorithmsPass]], which runs this and stamps the result onto
  * each `Algorithm`'s own `returnsCompletion` field, so
  * [[WrapCompletionReturnsPass]] (wraps a member's own `Return`s) and
  * [[PropagateUnguardedCallsPass]] (adds the same runtime propagation `?` gives
  * to an unmarked call site into a member) can each just read that field off
  * whichever `Algorithm` they're handed, rather than needing the computed set
  * threaded through some other way.
  */
object CompletionAlgorithms:

  /** Strips a `[=...=]` link's brackets (if any) and lowercases, matching both
    * an `Algorithm.name`/`.id` (never bracketed) and an
    * `Expr.AlgoCall`/`Instr.Perform`'s call-target string (bracketed until
    * `Compiler.nameFromLink` strips it, well after this pass runs) to the same
    * key. Public: [[PropagateUnguardedCallsPass]] needs the exact same
    * normalization when matching a `Perform.func` against this object's result.
    */
  def normalize(name: String): String =
    name.stripPrefix("[=").stripSuffix("=]").trim.toLowerCase

  /** One call embedded in a single instruction: its target, whether it's
    * `?`/`!`-marked, and the variable (if any) bound to its result.
    */
  private case class CallInfo(
    callee: String,
    marked: Boolean,
    boundTo: Option[String],
  )

  private def callInfo(instr: Instr): Option[CallInfo] =
    def fromExpr(e: Expr, bound: Option[String]): Option[CallInfo] = e match
      case Expr.AlgoCall(link, _) =>
        Some(CallInfo(normalize(link), false, bound))
      case Expr.Abrupt(_, Expr.AlgoCall(link, _)) =>
        Some(CallInfo(normalize(link), true, bound))
      case _ => None
    instr match
      case Instr.Perform(func, _, PerformOutcome.BindResult(v), _) =>
        Some(CallInfo(normalize(func), false, Some(stripPipes(v))))
      case Instr.Perform(func, _, _, _) =>
        Some(CallInfo(normalize(func), false, None))
      case Instr.Let(Expr.Var(v), expr, _) => fromExpr(expr, Some(v))
      case Instr.Set(Expr.Var(v), expr, _) => fromExpr(expr, Some(v))
      case Instr.Return(Some(expr), _)     => fromExpr(expr, None)
      case _                               => None

  /** `PerformOutcome.BindResult`'s string isn't reliably pipe-free — a
    * "verb-phrase, and let |X| be the result." call (e.g. `read the imports`'s
    * own call sites) parses straight to `Instr.Perform(..,
    * BindResult(rawPipedText), ..)` at parse time, unlike `Expr.Var`'s own
    * regex (which always strips the pipes) — see `ExpandThrowsPass`'s own
    * identically-named helper for the same reason.
    */
  private def stripPipes(s: String): String =
    s.stripPrefix("|").stripSuffix("|")

  private def hasOwnAbrupt(instrs: List[Instr]): Boolean =
    instrs.exists {
      // a "the following steps..." closure's substeps are, semantically, a
      // separate algorithm-to-be (ExpandFollowingStepsPass splits them out
      // later) — their own ?/!/Throw usage says nothing about *this*
      // algorithm, so don't recurse into it here (see e.g. `create a host
      // function`, whose own top-level steps never use ?/! at all, only its
      // nested hostfunc closure does — wrongly inherited before this guard).
      case Instr.Let(_, Expr.FollowingSteps(_), _) => false
      case i =>
        callInfo(i).exists(_.marked) || (i match
          case _: Instr.Throw => true
          case c: Instr.IfChain =>
            c.branches.exists((_, b) => hasOwnAbrupt(b)) || hasOwnAbrupt(
              c.fallback,
            )
          case _ => hasOwnAbrupt(i.body)
        )
    }

  /** Whether `x`'s completion-ness is manually handled somewhere in `rest` —
    * see the class doc's "absorbed or caught" cases. Public: shared with
    * [[PropagateUnguardedCallsPass]], which needs the exact same "is this
    * particular call already handled" check at the point it actually inserts
    * the propagation guard.
    */
  def isAbsorbed(x: Option[String], rest: List[Instr]): Boolean =
    x.exists(v => mentionsTypeField(v, rest)) ||
    (rest match
      case Instr.If(Cond.Throws(_), _) :: _ => true
      case _                                => false
    )

  /** Whether `x.[[Type]]`/`x.Type` is read anywhere in `instrs`, recursively —
    * not descending into a closure's own substeps, for the same reason
    * `hasOwnAbrupt` doesn't (a separate algorithm-to-be; `x` isn't even in
    * scope in there).
    */
  def mentionsTypeField(x: String, instrs: List[Instr]): Boolean =
    instrs.exists {
      case Instr.Let(_, Expr.FollowingSteps(_), _) => false
      case i =>
        exprsOf(i).exists(mentionsField(x, _)) ||
        condsOf(i).exists(mentionsField(x, _)) || (i match
          case c: Instr.IfChain =>
            c.branches.exists((cond, b) =>
              mentionsField(x, cond) || mentionsTypeField(x, b),
            ) || mentionsTypeField(x, c.fallback)
          case _ => mentionsTypeField(x, i.body)
        )
    }

  private def exprsOf(instr: Instr): List[Expr] = instr match
    case Instr.Let(_, e, _)           => List(e)
    case Instr.Set(_, e, _)           => List(e)
    case Instr.Return(eo, _)          => eo.toList
    case Instr.Perform(_, args, _, _) => args
    case _                            => Nil

  private def condsOf(instr: Instr): List[Cond] = instr match
    case Instr.Assert(c, _) => List(c)
    case Instr.While(c, _)  => List(c)
    case _                  => Nil

  private def mentionsField(x: String, expr: Expr): Boolean = expr match
    case Expr.Field(Expr.Var(v), "Type") => v == x
    case Expr.Field(base, _)             => mentionsField(x, base)
    case Expr.Index(base, key) =>
      mentionsField(x, base) || mentionsField(x, key)
    case Expr.BinOp(l, _, r)     => mentionsField(x, l) || mentionsField(x, r)
    case Expr.TupleProj(base, _) => mentionsField(x, base)
    case Expr.Length(e)          => mentionsField(x, e)
    case Expr.Case(_, args)      => args.exists(mentionsField(x, _))
    case Expr.AlgoCall(_, args)  => args.exists(mentionsField(x, _))
    case Expr.Abrupt(_, e)       => mentionsField(x, e)
    case _                       => false

  private def mentionsField(x: String, cond: Cond): Boolean = cond match
    case Cond.Eq(l, r, _)         => mentionsField(x, l) || mentionsField(x, r)
    case Cond.And(l, r)           => mentionsField(x, l) || mentionsField(x, r)
    case Cond.Or(l, r)            => mentionsField(x, l) || mentionsField(x, r)
    case Cond.HasField(e, _)      => mentionsField(x, e)
    case Cond.IsType(e, _, _)     => mentionsField(x, e)
    case Cond.Compare(l, _, r)    => mentionsField(x, l) || mentionsField(x, r)
    case Cond.Matches(l, _, r, _) => mentionsField(x, l) || mentionsField(x, r)
    case _                        => false

  private def hasUnguardedCallInto(
    instrs: List[Instr],
    s: Set[String],
  ): Boolean =
    instrs match
      case Nil => false
      case Instr.Let(_, Expr.FollowingSteps(_), _) :: rest =>
        hasUnguardedCallInto(rest, s) // closure content — see hasOwnAbrupt
      case i :: rest =>
        val here = callInfo(i).exists { c =>
          !c.marked && s.contains(c.callee) && !isAbsorbed(c.boundTo, rest)
        }
        here ||
        (i match
          case c: Instr.IfChain =>
            c.branches.exists((_, b) => hasUnguardedCallInto(b, s)) ||
            hasUnguardedCallInto(c.fallback, s)
          case _ => hasUnguardedCallInto(i.body, s)
        ) ||
        hasUnguardedCallInto(rest, s)

  /** The fixed point itself: lowercased names of every algorithm that must
    * return a Completion Record.
    */
  def compute(algos: List[Algorithm]): Set[String] =
    def nameOf(a: Algorithm): Option[String] =
      a.name.orElse(a.id).map(_.toLowerCase)

    var s: Set[String] =
      algos
        .flatMap(a => Option.when(hasOwnAbrupt(a.body))(nameOf(a)).flatten)
        .toSet

    var changed = true
    while changed do
      changed = false
      for a <- algos do
        nameOf(a).foreach { n =>
          if !s.contains(n) && hasUnguardedCallInto(a.body, s) then
            s += n
            changed = true
        }
    s
