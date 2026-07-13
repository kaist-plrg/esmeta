package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}

/** Expands a `Cond.IsOfForm` branch condition of an `Instr.IfChain` (e.g.
  * "If |externtype| is of the form [=external-type/func=] |functype|, ...",
  * js-api/index.bs:506) into a runtime tag check plus `Let`-bindings for the
  * form's positional variables.
  *
  * A decoded WebAssembly value like `externtype` arrives as a SpecTec
  * `ALValue.CaseV(tag, args)` (see `construct.ml`'s `al_of_externtype`) —
  * "is of the form [=LINK=] |v1| |v2| ..." checks `tag` against the variant
  * `LINK` names, then binds each `|vN|` to the `N`th positional payload, i.e.
  * {{{
  *   IfChain([(IsOfForm(e, Case(link, [Var(v1), Var(v2), ...]), None, neg), body)], fallback)
  * }}}
  * becomes
  * {{{
  *   IfChain([(Eq(CaseTag(e), Str(tag), neg),
  *             Let(v1, TupleProj(e, 0)) :: Let(v2, TupleProj(e, 1)) :: ... :: body)], fallback)
  * }}}
  * — a form check is just a string-equality once the tag is read out, so this
  * reuses the existing [[Cond.Eq]] rather than a dedicated condition node.
  *
  * Only fires when `link` is a known entry of [[formTag]] and every binding is
  * a bare `Expr.Var` (never seen otherwise) with no `where` clause. Every
  * other shape — the nested variant destructuring GLOBAL/TABLE/MEM/TAG need
  * (their payload is itself another `CaseV`/tuple, not flat positional args;
  * see `al_of_globaltype`/`al_of_tabletype`/`al_of_memorytype`, and they're
  * version-dependent besides), the `where`-guarded form (index.bs:1212), and
  * the `{type ..., hostcode |hostfunc|}` record-shaped form (index.bs:1249) —
  * is left as `Cond.IsOfForm` for `Compiler` to report as `EYet("is of
  * form")`, until a concrete need for it is actually reached.
  *
  * Must run after `ResolveLinksPass` (so the form is already a resolved
  * `Expr.Case`, not a raw `Expr.Link`) and `GroupIfChainPass` (so the
  * condition is already inside an `IfChain` branch).
  */
object ExpandIsOfFormPass extends LoweringPass:

  /** `Cond.IsOfForm`'s link text, mapped to the runtime `ALValue.CaseV` tag
    * SpecTec's `construct.ml` (`al_of_externtype`) actually produces. Only
    * the one variant reached so far — the function-import branch of "read
    * the imports" (js-api/index.bs:506) — is covered; extend as more are
    * reached.
    */
  private val formTag: Map[String, String] = Map(
    "external-type/func" -> "FUNC",
  )

  private def stripLink(link: String): String =
    link.stripPrefix("[=").stripSuffix("=]").trim.toLowerCase

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

  private def expandBranch(cond: Cond, body: List[Instr]): (Cond, List[Instr]) =
    cond match
      case Cond.IsOfForm(e, Expr.Case(tag, args), None, neg)
          if args.forall(_.isInstanceOf[Expr.Var]) &&
            formTag.contains(stripLink(tag)) =>
        val runtimeTag = formTag(stripLink(tag))
        val binds = args.zipWithIndex.collect { case (v: Expr.Var, idx) =>
          Instr.Let(v, Expr.TupleProj(e, idx))
        }
        val check = Cond.Eq(Expr.CaseTag(e), Expr.Str(runtimeTag), neg)
        (check, binds ++ transform(body))
      case _ => (cond, transform(body))
