package esmeta.wji.compiler.lowering

import esmeta.error.UnsupportedSpecShape
import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}

/** Expands `Expr.ExtAttrArgument(attr)` — webidl/index.bs's "the identifier
  * argument of the [{{ATTR}}] [=extended attribute=]" (index.bs:808 `qualified
  * name`, 12428 `attribute setter`) — into an explicit search over the
  * subject's `extendedAttributes` list for the entry whose `id` is `attr`,
  * reading its `value` (`esmeta.wji.Initialize.seedHostDefined`'s
  * `extAttrRecord`).
  *
  * The spec never says whose extended attribute it is; both occurrences sit
  * inside an "If |X| has a/is declared with the [{{ATTR}}] [=extended
  * attribute=]" guard, and "the [{{ATTR}}] extended attribute" refers back to
  * the one that guard just found. So the subject is taken from the nearest
  * enclosing `IfChain` branch whose condition (or one of its `And` conjuncts)
  * is `CondParser.declaredWithAttr`'s search for that same `attr`:
  *
  * {{{
  *   IfChain([(Any(ea, [X.extendedAttributes], ea.id is ATTR), [
  *     Let(lhs, ExtAttrArgument(ATTR), body)
  *   ])])
  * }}}
  * becomes
  * {{{
  *   IfChain([(Any(ea, [X.extendedAttributes], ea.id is ATTR), [
  *     Let(lhs, undefined)
  *     Let(_extAttrIdx, 0)
  *     While(_extAttrIdx < Length(X.extendedAttributes),
  *       If(X.extendedAttributes[_extAttrIdx].id is ATTR,
  *           Set(lhs, X.extendedAttributes[_extAttrIdx].value))
  *       Set(_extAttrIdx, _extAttrIdx + 1)
  *     )
  *     ...body...
  *   ])])
  * }}}
  *
  * Only the `Let` RHS position is handled (both real occurrences); an
  * `ExtAttrArgument` with no such guard in scope throws
  * [[esmeta.error.UnsupportedSpecShape]].
  *
  * Category: Structural desugaring — Elimination.
  */
object ExpandExtAttrArgumentPass extends LoweringPass:

  /** Requires:
    *   - [[GroupIfChainPass]]: guards are read off `Instr.IfChain` branches,
    *     and the expansion emits a ready-made `IfChain` (same as
    *     [[ExpandGetMemberPass]]).
    */
  override def requires: Set[LoweringPass] = Set(GroupIfChainPass)

  /** Must precede:
    *   - [[ExpandMatchesExistsPass]]: it hoists the guard's `Cond.Any` out of
    *     the branch condition into preceding instructions, after which the
    *     guard's subject can no longer be read off the condition.
    */
  override def mustPrecede: Set[LoweringPass] = Set(ExpandMatchesExistsPass)

  /** extended attribute name -> the subject whose extended attribute an
    * enclosing guard tested
    */
  private type Guards = Map[String, Expr]

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = transform(Map.empty)(a.body)))

  private def transform(guards: Guards)(instrs: List[Instr]): List[Instr] =
    instrs.flatMap(expandInstr(guards))

  private def expandInstr(guards: Guards)(instr: Instr): List[Instr] =
    instr match
      case Instr.Let(lhs, Expr.ExtAttrArgument(attr, subject), body) =>
        val subj = subject.orElse(guards.get(attr)).getOrElse {
          throw UnsupportedSpecShape(
            name,
            s"no enclosing guard tests the [$attr] extended attribute that " +
            "\"the identifier argument of\" it refers to",
          )
        }
        val idx = Expr.Var("_extAttrIdx")
        val eas = Expr.Field(subj, "extendedAttributes")
        val elem = Expr.Index(eas, idx)
        List(
          Instr.Let(lhs, Expr.SpecTerm("undefined")),
          Instr.Let(idx, Expr.Num("0")),
          Instr.While(
            Cond.Compare(idx, Cond.CompareOp.Lt, Expr.Length(eas)),
            List(
              Instr.IfChain(
                List(
                  (
                    Cond.Eq(Expr.Field(elem, "id"), Expr.Str(attr)),
                    List(Instr.Set(lhs, Expr.Field(elem, "value"))),
                  ),
                ),
                Nil,
              ),
              Instr.Set(idx, Expr.BinOp(idx, Expr.BOp.Add, Expr.Num("1"))),
            ),
          ),
        ) ::: transform(guards)(body)
      case Instr.IfChain(branches, fallback) =>
        List(
          Instr.IfChain(
            branches.map((c, b) => (c, transform(guards ++ guardsOf(c))(b))),
            transform(guards)(fallback),
          ),
        )
      case _ => List(instr.mapBody(transform(guards)))

  /** the extended attributes `cond` guarantees, keyed by name — the shape
    * `CondParser.declaredWithAttr` builds, possibly as one `And` conjunct
    */
  private def guardsOf(cond: Cond): Guards = cond match
    case Cond.Any(
          binder,
          List(Expr.Field(subj, "extendedAttributes")),
          Cond.Eq(Expr.Field(Expr.Var(b), "id"), Expr.Str(attr), false),
          false,
        ) if b == binder =>
      Map(attr -> subj)
    case Cond.And(l, r) => guardsOf(l) ++ guardsOf(r)
    case _              => Map.empty
