package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}

/** Expands `Expr.Abrupt(marker, inner)` — the spec's `?` (ReturnIfAbrupt) and
  * `!` (assert-not-abrupt) operators — into explicit completion-record
  * inspection, mirroring `esmeta.compiler.Compiler.returnIfAbrupt`'s `check`
  * boolean (`true` for `?`, `false` for `!`): both read the wrapped value's
  * `.Value` once its completion-ness is settled — `?` returns the completion
  * outright when it's abrupt, `!` instead asserts it's `~normal~` (mirroring
  * mainline's own `IAssert(ETypeCheck(xExpr, IRType(NormalT)))` — `!` is not
  * simply a no-op there either).
  *
  * Unlike mainline (where `needRetComp` guarantees every `?`/`!`-reachable
  * callee's `Return` is wrapped in a completion record, computed from the
  * spec's own declared return types), a WJI-compiled algorithm's `Return` is
  * never wrapped — WJI has no such declared-type info to compute an equivalent
  * from. So, mirroring `ExpandThrowsPass`'s three-way completion check, this
  * first tests whether the callee's result even *has* a `.Type` field before
  * ever reading it, rather than assuming every `?`/`!` target returns a
  * completion record the way mainline safely can:
  * {{{
  *   Let(x, Abrupt("?", call), body)
  * }}}
  * becomes:
  * {{{
  *   Let(_compN, call)
  *   IfChain(
  *     [(_compN has field "Type",
  *       [IfChain(
  *           [(IsType(_compN, "AbruptCompletion"), [Return(_compN)])],
  *           fallback = [Let(x, _compN.Value, body)],
  *       )])],
  *     fallback = [Let(x, _compN, body)],   // not a completion at all
  *   )
  * }}}
  * and `Abrupt("!", call)` the same way, except the inner branch is a flat
  * `Assert(_compN.Type is ~normal~)` followed by the `.Value` bind, rather than
  * a further `IfChain`.
  *
  * Handles `?`/`!` in the direct-RHS position of `Let`, `Set`, and `Return`.
  * Nested occurrences (e.g. inside a larger expression) are left as-is — see
  * `Compiler`'s `Expr.Abrupt` fallback.
  */
object ExpandAbruptPass extends LoweringPass:
  private var counter = 0
  private def freshComp(): String = { counter += 1; s"_comp$counter" }

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = transform(a.body)))

  private def transform(instrs: List[Instr]): List[Instr] =
    instrs.flatMap(expandInstr)

  private def expandInstr(instr: Instr): List[Instr] = instr match

    case Instr.Let(lhs, Expr.Abrupt(marker, inner), body) =>
      expand(marker, inner, Instr.Let(lhs, _, transform(body)))

    case Instr.Set(lhs, Expr.Abrupt(marker, inner), body) =>
      expand(marker, inner, Instr.Set(lhs, _, transform(body)))

    case Instr.Return(Some(Expr.Abrupt(marker, inner)), body) =>
      expand(marker, inner, x => Instr.Return(Some(x), transform(body)))

    case _ =>
      List(instr.mapBody(transform))

  /** Shared `?`/`!` expansion. `bind` embeds the final unwrapped value into
    * whichever instruction shape (`Let`/`Set`/`Return`) is using it.
    */
  private def expand(
    marker: String,
    inner: Expr,
    bind: Expr => Instr,
  ): List[Instr] =
    val tmp = freshComp()
    val tmpVar = Expr.Var(tmp)
    val typeField = Expr.Field(tmpVar, "Type")
    val valueField = Expr.Field(tmpVar, "Value")

    val onCompletion: List[Instr] = marker match
      case "?" =>
        List(
          Instr.IfChain(
            branches = List(
              (
                Cond.IsType(tmpVar, "AbruptCompletion"),
                List(Instr.Return(Some(tmpVar))),
              ),
            ),
            fallback = List(bind(valueField)),
          ),
        )
      case _ => // "!" — assert it's Normal (never abrupt), then unwrap
        List(
          Instr.Assert(Cond.Eq(typeField, Expr.SpecTerm("normal"))),
          bind(valueField),
        )

    List(
      Instr.Let(Expr.Var(tmp), inner),
      Instr.IfChain(
        branches = List((Cond.HasField(typeField), onCompletion)),
        fallback = List(bind(tmpVar)), // not a completion at all — a bare value
      ),
    )
