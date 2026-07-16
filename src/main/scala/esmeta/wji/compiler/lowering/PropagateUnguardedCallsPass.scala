package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}
import esmeta.wji.lang.Instr.PerformOutcome

/** Mirrors the Infra Standard's "no explicit catch → automatically re-throw"
  * convention for any call to a known completion-returning algorithm (see
  * [[CompletionAlgorithms]]) that the spec prose doesn't mark with `?`/`!` and
  * doesn't otherwise handle itself — inserting the exact same runtime
  * propagation guard `ExpandAbruptPass` gives an explicit `?`.
  *
  * {{{
  *   Perform(f, args, BindResult(x))
  *   <rest, none of which reads x.[[Type]]>
  * }}}
  * becomes:
  * {{{
  *   Perform(f, args, BindResult(x))
  *   If(x is AbruptCompletion, [Return(Some(x))])
  *   Else [Set(x, x.Value)]
  *   <rest>
  * }}}
  *
  * A 2-way check, not `?`'s own defensive 3-way one (which also asks "is this
  * even a completion at all?" before trusting `.Type") — safe *only* because
  * `f` is always a `completionAlgos` member here, and every member's every exit
  * path is provably completion-shaped by construction:
  * `WrapCompletionReturnsPass` wraps every `Instr.Return`/`Instr.Throw` it
  * finds, and `InsertFallthroughReturnPass` (run before it) guarantees one
  * exists even for an algorithm that would otherwise just fall off the end.
  * `ExpandAbruptPass`'s own guard for an explicit `?`/`!` stays 3-way, since
  * `?`/`!` can just as well target a mainline ECMA-262 AO nothing here tracks.
  *
  * *Whether to insert the guard at all* is still significant: only
  * `completionAlgos` members are targeted, and only when
  * `CompletionAlgorithms.isAbsorbed` says the caller doesn't already handle it
  * — otherwise every single `Perform` in the file would grow a dead guard.
  *
  * Must run after [[InsertFallthroughReturnPass]] and
  * [[WrapCompletionReturnsPass]] (both needed for the "every exit path is
  * already completion-shaped" guarantee above),
  * [[MarkCompletionAlgorithmsPass]] (needs `returnsCompletion` already stamped
  * on every `Algorithm`), [[ExtractInlineAlgoCallPass]] (so every call is
  * already a `Perform`), and [[ExpandThrowsPass]] (so a `Cond.Throws`-guarded
  * call is already transformed away and won't be mistaken for unguarded —
  * though `CompletionAlgorithms.isAbsorbed`'s own `Cond.Throws` case makes this
  * pass's outcome the same either way, this ordering is what
  * `CompletionAlgorithms.compute`'s *own* analysis assumed when it observed the
  * pre-`GroupIfChainPass` shape of the same call sites).
  */
object PropagateUnguardedCallsPass extends LoweringPass:

  def run(algos: List[Algorithm]): List[Algorithm] =
    val completionAlgos: Set[String] = algos
      .collect {
        case a if a.returnsCompletion => a.name.orElse(a.id).map(_.toLowerCase)
      }
      .flatten
      .toSet
    algos.map(a => a.copy(body = transform(a.body, completionAlgos)))

  private def transform(
    instrs: List[Instr],
    completionAlgos: Set[String],
  ): List[Instr] = instrs match
    case Nil => Nil
    // a closure's substeps are out of scope here too — see CompletionAlgorithms
    case (i @ Instr.Let(_, Expr.FollowingSteps(_), _)) :: rest =>
      i :: transform(rest, completionAlgos)
    case (p @ Instr.Perform(
          func,
          _,
          PerformOutcome.BindResult(rawX),
          pbody,
        )) :: rest
        if completionAlgos.contains(CompletionAlgorithms.normalize(func)) &&
        !CompletionAlgorithms.isAbsorbed(Some(stripPipes(rawX)), rest) =>
      val x = stripPipes(rawX)
      p.copy(body = Nil) :: (propagate(x) ++ transform(
        pbody,
        completionAlgos,
      ) ++
      transform(rest, completionAlgos))
    case i :: rest =>
      i.mapBody(transform(_, completionAlgos)) :: transform(
        rest,
        completionAlgos,
      )

  // see CompletionAlgorithms's identically-named/documented helper
  private def stripPipes(s: String): String =
    s.stripPrefix("|").stripSuffix("|")

  private def propagate(x: String): List[Instr] =
    val xVar = Expr.Var(x)
    List(
      Instr.IfChain(
        branches = List(
          (
            Cond.IsType(xVar, "AbruptCompletion"),
            List(Instr.Return(Some(xVar))),
          ),
        ),
        fallback = List(Instr.Set(xVar, Expr.Field(xVar, "Value"))),
      ),
    )
