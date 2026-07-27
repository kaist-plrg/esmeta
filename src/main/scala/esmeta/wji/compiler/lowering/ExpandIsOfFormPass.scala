package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}

/** Expands a `Cond.IsOfForm` branch condition of an `Instr.IfChain` (e.g. "If
  * |externtype| is of the form [=external-type/func=] |functype|, ...",
  * js-api/index.bs:506) into a runtime tag check plus `Let`-bindings for the
  * form's positional variables — purely structurally, recursing into any nested
  * `Expr.Case` in `form`'s own args:
  * {{{
  *   IfChain([(IsOfForm(e, Case(tag, [Var(v1), Case(subTag, [Var(v2)]), ...]), None, neg), body)], fallback)
  * }}}
  * becomes
  * {{{
  *   IfChain([(Eq(CaseTag(e), Str(runtimeTag(tag)), neg) `And`/`Or`
  *             Eq(CaseTag(TupleProj(e, 1)), Str(runtimeTag(subTag)), neg) `And`/`Or` ...,
  *             Let(v1, TupleProj(e, 0)) :: Let(v2, TupleProj(TupleProj(e, 1), 0)) :: ... :: body)], fallback)
  * }}}
  * — a form check is just a string-equality once each tag is read out, so this
  * reuses the existing [[Cond.Eq]] rather than a dedicated condition node; `Or`
  * instead of `And` when negated, De Morgan's over however many tag checks the
  * recursion produced (`NOT(A AND B AND ...) = NOT(A) OR NOT(B) OR ...`, each
  * individual `Eq` already carrying `neg`).
  *
  * No SpecTec-specific knowledge lives here — no per-link tag table, no
  * hand-written nesting paths: `form` is trusted to already be shaped exactly
  * like the real runtime `ALValue.CaseV` it's checked against (`tag` need not
  * even be pre-translated — `Expr.runtimeCaseTag` is idempotent on an
  * already-final tag like `"CONST"`/`"I32"`). Producing that correctly-shaped
  * `form` — including cases where spec prose writes it flatter than the runtime
  * actually is, e.g. a numeric const's nested numtype tag or
  * `external-type/global`'s nested `mut`/`valuetype` pair — is entirely
  * `ResolveLinksPass`'s job (see its `numConstTags`/`nestedFormLinks`).
  *
  * Only fires when every non-`Case` arg is a bare `Expr.Var` (never seen
  * otherwise) and there's no `where` clause. The `where`-guarded, untagged form
  * (`GetGlobalValue`'s "If |globaltype| is of the form <var ignore>mut</var>
  * |valuetype| where ...", index.bs:1212) is handled separately, below — see
  * that case's own doc comment. A `form` that isn't `Expr.Case` at all (e.g.
  * index.bs:1249's `{type ..., hostcode |hostfunc|}` record shape, which never
  * parses to one) is left as `Cond.IsOfForm` for `Compiler` to report as
  * `EYet("is of form")`, until a concrete need for it is actually reached.
  *
  * Category: Structural desugaring.
  */
object ExpandIsOfFormPass extends LoweringPass:

  /** Requires:
    *   - [[ResolveLinksPass]]: needs the form already resolved to `Expr.Case`,
    *     not a raw `Expr.Link`.
    *   - [[GroupIfChainPass]]: needs the condition already inside an
    *     `Instr.IfChain` branch.
    */
  override def requires: Set[LoweringPass] =
    Set(ResolveLinksPass, GroupIfChainPass)

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = transform(a.body)))

  private def transform(instrs: List[Instr]): List[Instr] =
    instrs.map {
      case i: Instr.IfChain =>
        i.copy(
          branches = i.branches.map((c, b) => expandBranch(c, b)),
          fallback = transform(i.fallback),
        )
      case other => other.mapBody(transform)
    }

  /** Recursively builds the tag-check(s) and binding(s) for matching `e`
    * against `pattern` — see class doc. Each positional arg of `pattern` is
    * either a bare `Expr.Var` (bind `TupleProj(e, i)` to it) or itself an
    * `Expr.Case` (recurse into `TupleProj(e, i)`, folding its own tag check(s)
    * into the result via `And`/`Or`); any other arg shape is dropped from the
    * check/bind results entirely; a bare `Cond.IsOfForm` whose `form` contains
    * one is left for `expandBranch`'s catch-all to report as `EYet` instead of
    * silently mismatching.
    */
  private def buildFormMatch(
    e: Expr,
    pattern: Expr.Case,
    neg: Boolean,
  ): (Cond, List[Instr]) =
    val Expr.Case(tag, args) = pattern: @unchecked
    val tagCheck =
      Cond.Eq(Expr.CaseTag(e), Expr.Str(Expr.runtimeCaseTag(tag)), neg)
    val (nestedChecks, binds) = args.zipWithIndex.foldRight(
      (List.empty[Cond], List.empty[Instr]),
    ) {
      case ((v: Expr.Var, i), (checks, binds)) =>
        (checks, Instr.Let(v, Expr.TupleProj(e, i)) :: binds)
      case ((c: Expr.Case, i), (checks, binds)) =>
        val (subCheck, subBinds) = buildFormMatch(Expr.TupleProj(e, i), c, neg)
        (subCheck :: checks, subBinds ++ binds)
      case (_, acc) => acc
    }
    val combine: (Cond, Cond) => Cond =
      if neg then Cond.Or.apply else Cond.And.apply
    ((tagCheck :: nestedChecks).reduce(combine), binds)

  private def expandBranch(cond: Cond, body: List[Instr]): (Cond, List[Instr]) =
    cond match
      // the `where`-guarded, untagged-form idiom (`GetGlobalValue`'s "If
      // |globaltype| is of the form <var ignore>mut</var> |valuetype| where
      // |valuetype| [=matches/valtype|matches=] [=v128=] or [=exnref=], throw
      // ...", index.bs:1212 — the only occurrence in the corpus today, hence
      // `neg = false` only: a negated compound condition would need De
      // Morgan's over an arbitrary `whereCond`, and there's no generic
      // `Cond.Not` to express that with). Unlike `buildFormMatch`, the match
      // itself is unconditional (an untagged `Case("", ...)`'s runtime tag is
      // always `""` — see `ExprParser.parseUntaggedForm`/
      // `ExpandDestructuringLetPass`'s identical reasoning for `Let`), so
      // there's nothing to guard the bindings on: they're hoisted out
      // unconditionally, and only `whereCond` itself becomes a real (nested)
      // branch condition.
      case Cond.IsOfForm(e, Expr.Case("", args), Some(whereCond), false)
          if args.forall(_.isInstanceOf[Expr.Var]) =>
        val binds = args.zipWithIndex.collect {
          case (v: Expr.Var, i) => Instr.Let(v, Expr.TupleProj(e, i))
        }
        val alwaysTrue = Cond.Eq(Expr.CaseTag(e), Expr.Str(""))
        val guarded = Instr.IfChain(
          branches = List((whereCond, transform(body))),
          fallback = Nil,
        )
        (alwaysTrue, binds :+ guarded)
      case Cond.IsOfForm(e, form: Expr.Case, None, neg) if form.args.forall {
            case _: Expr.Var  => true
            case _: Expr.Case => true
            case _            => false
          } =>
        val (check, binds) = buildFormMatch(e, form, neg)
        (check, binds ++ transform(body))
      case _ => (cond, transform(body))
