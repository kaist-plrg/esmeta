package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Instr}

/** Desugars the `"Assert: This step is not reached."` idiom
  * (`spectec/document/js-api/index.bs`'s `ToValueType`/`ToWebAssemblyValue`/
  * `AddressValueToU64`/`U64ToAddressValue` — the only four occurrences). Each
  * writes it as a *separate*, final numbered step right after an if/else-if
  * chain whose own branches are meant to be exhaustive — not inside an explicit
  * "else" clause of that chain — so `GroupIfChainPass` leaves the chain's own
  * `fallback` empty and leaves this assert as a plain sibling instruction
  * immediately following the whole `IfChain` in the body list:
  * {{{
  *   IfChain([..., (condN, bodyN)], [])
  *   Assert(Unreachable)
  * }}}
  * This pass folds the chain's last real branch into that assert instead of a
  * `Branch`:
  * {{{
  *   IfChain([...], Assert(condN) :: bodyN)
  * }}}
  * (or, if `condN` was the chain's only branch, just `Assert(condN) :: bodyN`
  * with no `IfChain` left at all) — semantically identical (both run `bodyN`
  * exactly when `condN` holds, and both signal failure otherwise), but removes
  * a `Branch` node whose "false" outcome `esmeta.wji.WjiCoverage` confirmed is
  * never, and structurally can never be, taken by any real input (found
  * triaging its own output, 2026-10-07) — the official js-api corpus exercising
  * this branch isn't a coverage gap to fill with a test case; it's genuinely
  * unreachable code to stop asking the corpus to cover.
  *
  * Also fixes a side effect `Instr.alwaysExits` has on these four algorithms
  * before this pass runs: `alwaysExits` doesn't recognize a bare `Assert` as an
  * exit (only `Return`/`Throw`/a result-returning `Perform`), so an `IfChain`
  * with an empty `fallback`, immediately followed by a sibling
  * `Assert(Unreachable)` and nothing else, reads as "doesn't always exit" —
  * `InsertFallthroughReturnPass` then tacks on a second, equally dead
  * `Return(None)` after it. Once `bodyN` (which always itself ends in a real
  * `Return`/`Throw`, per the spec idiom) is spliced into the `fallback` by this
  * pass, `alwaysExits` sees it there directly and no longer needs that extra
  * return either.
  *
  * Deliberately narrow: only fires on an `IfChain` with an empty `fallback` and
  * at least one real branch, directly followed by `Assert(Cond.Unreachable,
  * Nil)` — the sole shape this corpus's four occurrences share. An
  * `Unreachable` assert reached any other way (already inside a real `else`, or
  * with other steps between it and the `IfChain`) is left untouched.
  *
  * Category: Structural desugaring — Elimination.
  */
object EliminateUnreachableFallbackPass extends LoweringPass:

  /** Requires:
    *   - [[GroupIfChainPass]]: needs the if/else-if/else chain already folded
    *     into a single `Instr.IfChain`, not flat `If`/`ElseIf`/`Else` siblings
    *     — this pass's own match is on `IfChain` directly.
    *   - [[ExpandIsOfFormPass]]: the folded branch's own `Cond` (`ToWebAssembly
    *     Value`'s occurrence is a `Cond.IsOfForm`, not a plain `Cond.Eq` like
    *     the other three) must already be reduced to a directly-assertable
    *     shape before this pass moves it out of an `If`'s condition position
    *     into a bare `Assert` — an un-expanded `IsOfForm` relies on running
    *     *inside* an `If` to both test the shape and destructure its payload
    *     into fresh `Let`s at the front of that `If`'s body; asserted bare, it
    *     has no body to prepend those `Let`s to and compiles to an inert `(yet
    *     ...)` instead, silently losing both the destructuring and the check
    *     (caught by a since-discarded first attempt at this pass that ran right
    *     after `GroupIfChainPass`, before `ExpandIsOfFormPass` — the
    *     `ir.expected` diff showed the `let null = type[0]`/`let heaptype =
    *     type[1]` lines vanish entirely). Placed after every other elimination
    *     pass that might similarly rewrite a branch condition in a way that
    *     depends on remaining inside an `If`, not just this one empirically-hit
    *     case — but only `ExpandIsOfFormPass` is in `requires` since it's the
    *     only one actually exercised by this corpus's four occurrences; a
    *     future fifth occurrence whose last condition needs a different
    *     not-yet-run elimination pass would need the same fix applied to it
    *     specifically; this one is not written generically enough to guess at a
    *     shape never observed.
    */
  override def requires: Set[LoweringPass] =
    Set(GroupIfChainPass, ExpandIsOfFormPass)

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = transform(a.body)))

  private def transform(instrs: List[Instr]): List[Instr] =
    instrs match
      case Nil => Nil
      case Instr.IfChain(branches, Nil)
          :: Instr.Assert(Cond.Unreachable, Nil) :: rest if branches.nonEmpty =>
        val (lastCond, lastBody) = branches.last
        val folded = Instr.Assert(lastCond) :: transform(lastBody)
        (branches.init match
          case Nil => folded
          case init =>
            List(
              Instr.IfChain(
                init.map((c, b) => (c, transform(b))),
                folded,
              ),
            )
        ) ++ transform(rest)
      case (i: Instr.IfChain) :: rest =>
        i.copy(
          branches = i.branches.map((c, b) => (c, transform(b))),
          fallback = transform(i.fallback),
        ) :: transform(rest)
      case instr :: rest => instr.mapBody(transform) :: transform(rest)
