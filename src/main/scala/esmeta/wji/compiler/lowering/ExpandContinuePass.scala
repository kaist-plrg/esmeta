package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Instr}

/** Desugars the "if COND, then [=iteration/continue=]." guard-clause idiom
  * (e.g. `find_a_builtin`/`read_the_imports`'s "If |builtinSetName| does not
  * refer to a builtin set, then [=iteration/continue=]." — js-api/index.bs:
  * 488/1848, the only occurrence in this corpus) into ordinary structured
  * control flow, since mainline `esmeta.ir` has no native "continue"
  * instruction (loops are already fully structured, unlike ECMA-262's own
  * pseudocode, which never uses "continue" either).
  *
  * By guard-clause inversion:
  * {{{
  *   IfChain([(cond, [Continue])], [])
  *   rest...
  * }}}
  * becomes
  * {{{
  *   IfChain([(cond, [])], transform(rest))
  * }}}
  * i.e. `rest` (everything that would otherwise run after this iteration's
  * guard) moves into the `IfChain`'s own (until now empty) fallback branch —
  * skipping it exactly when the guard fires, the same effect a real
  * `continue` would have, with `Continue` itself simply dropped (an empty
  * branch body compiles to a no-op `ISeq(Nil)`, see `Compiler.
  * compileInstrs`).
  *
  * Deliberately narrow: only fires on a single-branch, fallback-less
  * `IfChain` whose one branch body is exactly `[Continue(Nil)]` — the sole
  * shape this corpus produces. A `Continue` reached any other way (nested
  * deeper, alongside other statements in its own branch, or under an
  * `ElseIf`/existing `Else`) is left untouched, still compiling to
  * `Compiler`'s `EYet("continue")` placeholder, rather than guessing at a
  * transform for a shape never actually observed.
  *
  * Category: Structural desugaring — Elimination.
  */
object ExpandContinuePass extends LoweringPass:

  /** Requires:
    *   - [[GroupIfChainPass]]: needs the guard already folded into a single
    *     `Instr.IfChain`, not a raw `If` sibling with `rest` following it
    *     unrelated in the list — `GroupIfChainPass`'s own `transform` already
    *     separates a bare `If`'s `body` from what follows it in exactly the
    *     shape this pass matches on.
    */
  override def requires: Set[LoweringPass] = Set(GroupIfChainPass)

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = transform(a.body)))

  private def transform(instrs: List[Instr]): List[Instr] =
    instrs match
      case Nil => Nil
      case Instr.IfChain(
            List((cond, List(Instr.Continue(Nil)))),
            Nil,
          ) :: rest =>
        List(Instr.IfChain(List((cond, Nil)), transform(rest)))
      case (i: Instr.IfChain) :: rest =>
        i.copy(
          branches = i.branches.map((c, b) => (c, transform(b))),
          fallback = transform(i.fallback),
        ) :: transform(rest)
      case instr :: rest => instr.mapBody(transform) :: transform(rest)
