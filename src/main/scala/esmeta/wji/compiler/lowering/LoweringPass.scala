package esmeta.wji.compiler.lowering

import esmeta.wji.lang.Algorithm

/** A single content-level assumption a [[LoweringPass]] relies on or guarantees
  * — as opposed to [[LoweringPass.requires]]/[[LoweringPass.mustPrecede]],
  * which are about pipeline *ordering*, a `Condition` is about the actual
  * `Algorithm`/`Instr` *shape* of the list it's checked against. `description`
  * is surfaced verbatim in the `UnsupportedSpecShape` thrown on violation (see
  * `Lowering.run`), so it should read as a complete, human-readable sentence.
  */
case class Condition(description: String, holds: List[Algorithm] => Boolean)

/** One step of the `esmeta.wji.compiler.lowering` pipeline (see
  * [[Lowering.pipeline]]) — a rewrite over the full `List[Algorithm]` that
  * assumes some earlier steps have already run and some later ones haven't yet.
  * [[requires]]/[[mustPrecede]] make that assumption explicit and
  * machine-checked (`Lowering.run` verifies them before running anything — see
  * its doc), instead of only living as prose that can silently drift from the
  * pipeline's real order.
  */
trait LoweringPass:
  def run(algos: List[Algorithm]): List[Algorithm]

  /** Passes that must already have run — appear earlier in
    * [[Lowering.pipeline]] — before this one: this pass's own patterns assume
    * an AST shape only they produce (e.g. a resolved `Expr.AlgoCall` instead of
    * a raw `Expr.Link`, or an `Instr.IfChain` instead of bare
    * `If`/`ElseIf`/`Else`). The common case — the dependency is owned by
    * whichever pass actually needs something, not by the one that happens to
    * produce it. Document each entry's specific reason in a "Requires:" doc
    * block directly above the override, one bullet per entry: `- [[Pass]]:
    * needs <specific shape/invariant>.`
    */
  def requires: Set[LoweringPass] = Set.empty

  /** Passes that must run after this one — the rarer, opposite relationship:
    * this pass needs to observe or act on a shape that one of these would
    * otherwise already have mutated away, even though that later pass needs
    * nothing from this one in return (so it can't own the dependency via its
    * own [[requires]] the way the common case does). Document each entry in a
    * "Must precede:" doc block directly above the override, same one-bullet-
    * per-entry form as [[requires]].
    */
  def mustPrecede: Set[LoweringPass] = Set.empty

  /** [[Condition]]s this pass's own `run` relies on about its *input* —
    * distinct from [[requires]] (pipeline ordering): a precondition is about
    * the actual `Algorithm`/`Instr` content at that point, not about which
    * other passes already ran. Checked by `Lowering.run` immediately before
    * calling `run`, so a violation fails loudly via `UnsupportedSpecShape`
    * instead of surfacing later as a confusing `MatchError` deep in some
    * private helper, or silently producing wrong output.
    */
  def preconditions: List[Condition] = List.empty

  /** [[Condition]]s this pass's own `run` guarantees about its *output* —
    * checked by `Lowering.run` immediately after calling `run`, for the same
    * reason [[preconditions]] are checked before it.
    */
  def postconditions: List[Condition] = List.empty

  /** Reflection-free display name for error messages — a Scala `object`'s own
    * `getClass.getSimpleName` carries a trailing `$` that's noisy in output.
    */
  def name: String = getClass.getSimpleName.stripSuffix("$")
