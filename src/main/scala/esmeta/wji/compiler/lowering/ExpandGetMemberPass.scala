package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}

/** Expands `Expr.GetMember(definition, kind)` — webidl/index.bs's "the [=list=]
  * of [=regular attributes=]/[=static attributes=]/[=regular
  * operations=]/[=static operations=] that are [=members=] of |definition|"
  * idiom (e.g. index.bs:12299, `define the regular attributes`) — into an
  * explicit filter loop over `definition`'s `members` field:
  *
  * {{{
  *   Let(lhs, GetMember(definition, kind, unforgeable), body)
  * }}}
  * becomes
  * {{{
  *   Let(lhs, «»)
  *   Let(_getMemberIdxN, 0)
  *   While(_getMemberIdxN < Length(definition.members),
  *     If(definition.members[_getMemberIdxN].kind is kind.toString,
  *         Append(definition.members[_getMemberIdxN], lhs))
  *     Set(_getMemberIdxN, _getMemberIdxN + 1)
  *   )
  *   ...body...
  * }}}
  *
  * A `ForEach` directly over a `GetMember` (e.g. `define the constants`' "For
  * each [=constant=] |const| that is a [=const=] of |definition|", see
  * `InstrParser.ForEachConstantOf`) first binds the list the same way, to
  * `_getMemberList`, then iterates over that variable instead.
  *
  * With `unforgeable` set ("the [=list=] of [=unforgeable=] [=regular
  * attributes=] ...", index.bs:12316/12514), the `If` condition additionally
  * requires the member to be declared with `[LegacyUnforgeable]`
  * (index.bs:11334-11337) — a search over its `extendedAttributes` list, as in
  * `CondParser.declaredWithAttr`.
  *
  * `kind` compiles to a literal `Str` compared against each runtime member
  * record's own `kind` field — see `esmeta.wji.Initialize.seedHostDefined`,
  * which populates every `operation`/`attribute` record's `kind` field with
  * exactly `MemberKind#toString` (e.g. `"RegularAttribute"`).
  *
  * Category: Structural desugaring — Elimination.
  */
object ExpandGetMemberPass extends LoweringPass:

  /** Requires:
    *   - [[GroupIfChainPass]]: this pass emits a ready-made `Instr.IfChain`
    *     directly (mirroring [[ExpandHasDuplicatesPass]]'s own precedent),
    *     rather than a bare `If` for a not-yet-run `GroupIfChainPass` to fold —
    *     it runs among the eliminations, after `GroupIfChainPass`'s single pass
    *     over the original (pre-lowering) `If`/`ElseIf`/`Else` siblings.
    */
  override def requires: Set[LoweringPass] = Set(GroupIfChainPass)

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map { a =>
      a.copy(body = transform(a.body))
    }

  private def transform(instrs: List[Instr]): List[Instr] =
    instrs.flatMap(expandInstr)

  private def expandInstr(instr: Instr): List[Instr] = instr match
    case Instr.Let(lhs, Expr.GetMember(definition, kind, unforgeable), body) =>
      val idx = Expr.Var("_getMemberIdx")
      val members = Expr.Field(definition, "members")
      val elem = Expr.Index(members, idx)
      val isKind =
        Cond.Eq(Expr.Field(elem, "kind"), Expr.Enum(kind.toString))
      val cond =
        if (!unforgeable) isKind
        else
          Cond.And(
            isKind,
            Cond.Any(
              "ea",
              List(Expr.Field(elem, "extendedAttributes")),
              Cond.Eq(
                Expr.Field(Expr.Var("ea"), "id"),
                Expr.Str("LegacyUnforgeable"),
              ),
            ),
          )
      List(
        Instr.Let(lhs, Expr.List_(Nil)),
        Instr.Let(idx, Expr.Num("0")),
        Instr.While(
          Cond.Compare(idx, Cond.CompareOp.Lt, Expr.Length(members)),
          List(
            Instr.IfChain(
              List(
                (cond, List(Instr.Append(elem, lhs))),
              ),
              Nil,
            ),
            Instr.Set(idx, Expr.BinOp(idx, Expr.BOp.Add, Expr.Num("1"))),
          ),
        ),
      ) ::: transform(body)
    case Instr.ForEach(elem, getMember: Expr.GetMember, body) =>
      val list = Expr.Var("_getMemberList")
      expandInstr(Instr.Let(list, getMember)) :+
      Instr.ForEach(elem, list, transform(body))
    case _ => List(instr.mapBody(transform))
