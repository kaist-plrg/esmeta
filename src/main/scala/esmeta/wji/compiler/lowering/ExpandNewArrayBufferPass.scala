package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Expr, Instr}

/** Expands `Expr.NewArrayBuffer` — "a new {{ArrayBuffer}} with the internal
  * slots [[ArrayBufferData]], [[ArrayBufferByteLength]], ..." — into a `JSCall`
  * on ECMA-262's own `AllocateArrayBuffer(constructor, byteLength)`, against
  * the current realm's `%ArrayBuffer%` intrinsic:
  *
  * {{{
  *   Let(x, NewArrayBuffer, body)
  * }}}
  * becomes:
  * {{{
  *   Let(x, [=!=] [$AllocateArrayBuffer$]([=current Realm=].Intrinsics.%ArrayBuffer%, 0))
  *   ...body...
  * }}}
  *
  * `ArrayBuffer` isn't a js-api-defined interface (it's an ECMA-262 primitive
  * js-api merely reuses), so this deliberately does *not* go through
  * `Expr.New`/WebIDL's platform-object construction — see
  * `docs/underspecified-behaviors.md` for why no algorithm is actually named
  * for this phrasing in the spec text itself. The placeholder `byteLength=0` is
  * fine: the spec's own very next steps (`Set buffer.[[ArrayBufferData]] to
  * block`, `Set buffer.[[ArrayBufferByteLength]] to length(block)`) immediately
  * overwrite both fields regardless of what `AllocateArrayBuffer` itself
  * initialized them to.
  *
  * The `!` marker is required, not decorative: `AllocateArrayBuffer` is a
  * completion-returning mainline abstract operation (confirmed by
  * webidl/index.bs:9273's own `[=?=] [$AllocateArrayBuffer$](...)` call), so
  * its raw result is a Completion Record, not the `ArrayBuffer` object
  * directly. Emitting the bare `JSCall` without `Abrupt` here (an earlier
  * version of this pass did) silently bound `target` to the *wrapped*
  * Completion Record itself — the subsequent `Set target.[[ArrayBufferData]]
  * ...` steps then added those internal-slot fields directly onto the
  * Completion Record's own fields (since `Set` doesn't care what shape its
  * target already has), producing a corrupted hybrid object with no
  * `[[Get]]`/`[[Set]]` internal methods at all. Wrapping in `Expr.Abrupt("!",
  * ...)` here routes this through the same `NormalizeEvaluationOrderPass`/
  * `ExpandAbruptPass` unwrapping every other `?`/`!`-marked call already gets,
  * correctly binding `target` to the Completion Record's `.Value` instead.
  * `AllocateArrayBuffer` can only actually fail via a `RangeError` for an
  * oversized `byteLength`; ours is always the literal `0`, so `!`
  * (assert-not-abrupt) rather than `?` (propagate) is the correct marker —
  * there is no enclosing completion-returning context for `?` to propagate into
  * here anyway (`create a fixed length memory buffer`'s own body has no
  * `Throw`/abrupt path at all).
  *
  * Only handles `NewArrayBuffer` in direct `Let` RHS position — the only shape
  * observed in practice (`create a fixed length memory buffer`/ `create a
  * resizable memory buffer`).
  *
  * Category: Structural desugaring — Elimination.
  */
object ExpandNewArrayBufferPass extends LoweringPass:

  /** Must precede:
    *   - [[ExpandInlineAlgoCallPass]]: needs the `Let` RHS shape this pass
    *     produces already in place.
    *   - [[NormalizeEvaluationOrderPass]]: the `Expr.Abrupt("!", JSCall(...))`
    *     this pass produces must exist before Normalize's ANF hoist walk runs,
    *     so it gets rewritten into the canonical `Let(_callN, JSCall(...))`
    *     shape [[ExpandAbruptPass]] (which itself `requires`
    *     [[NormalizeEvaluationOrderPass]]) expects — otherwise the un-hoisted
    *     `JSCall` ends up nested inside a `.Type`/`.Value` field access that
    *     nothing downstream can compile.
    */
  override def mustPrecede: Set[LoweringPass] =
    Set(ExpandInlineAlgoCallPass, NormalizeEvaluationOrderPass)

  private val arrayBufferIntrinsic =
    Expr.Field(
      Expr.Field(Expr.SpecTerm("current Realm"), "Intrinsics"),
      "%ArrayBuffer%",
    )

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = transform(a.body)))

  private def transform(instrs: List[Instr]): List[Instr] =
    instrs.flatMap(expandInstr)

  private def expandInstr(instr: Instr): List[Instr] = instr match
    case Instr.Let(target @ Expr.Var(_), Expr.NewArrayBuffer, body) =>
      Instr.Let(
        target,
        Expr.Abrupt(
          "!",
          Expr.JSCall(
            "AllocateArrayBuffer",
            List(arrayBufferIntrinsic, Expr.Num("0")),
          ),
        ),
      ) :: transform(body)

    case _ =>
      List(instr.mapBody(transform))
