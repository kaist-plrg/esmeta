package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}

/** Expands a `Cond.IsOfForm` branch condition of an `Instr.IfChain` (e.g. "If
  * |externtype| is of the form [=external-type/func=] |functype|, ...",
  * js-api/index.bs:506) into a runtime tag check plus `Let`-bindings for the
  * form's positional variables.
  *
  * A decoded WebAssembly value like `externtype` arrives as a SpecTec
  * `ALValue.CaseV(tag, args)` (see `construct.ml`'s `al_of_externtype`) — "is
  * of the form [=LINK=] |v1| |v2| ..." checks `tag` against the variant `LINK`
  * name, then binds each `|vN|` to where that payload actually lives. Most
  * variants are flat (`|vN|` is directly the `N`th positional arg of the
  * `CaseV`), but some nest a sub-tuple (e.g. GLOBAL's sole positional arg is
  * itself a `CaseV("", [mut, valtype])` — see [[FormSpec.argPaths]]):
  * {{{
  *   IfChain([(IsOfForm(e, Case(link, [Var(v1), Var(v2), ...]), None, neg), body)], fallback)
  * }}}
  * becomes
  * {{{
  *   IfChain([(Eq(CaseTag(e), Str(tag), neg),
  *             Let(v1, TupleProj(e, path1)) :: Let(v2, TupleProj(e, path2)) :: ... :: body)], fallback)
  * }}}
  * — a form check is just a string-equality once the tag is read out, so this
  * reuses the existing [[Cond.Eq]] rather than a dedicated condition node.
  *
  * Only fires when `link` is a known entry of [[forms]] and every binding is a
  * bare `Expr.Var` (never seen otherwise) with no `where` clause. Every other
  * shape — TAG's form (its payload's real `construct.ml` shape doesn't even
  * match what the spec prose asks for — a spec bug, not a nesting gap), the
  * `where`-guarded form (index.bs:1212), and the `{type ..., hostcode
  * |hostfunc|}` record-shaped form (index.bs:1249) — is left as `Cond.IsOfForm`
  * for `Compiler` to report as `EYet("is of form")`, until a concrete need for
  * it is actually reached.
  *
  * Must run after `ResolveLinksPass` (so the form is already a resolved
  * `Expr.Case`, not a raw `Expr.Link`) and `GroupIfChainPass` (so the condition
  * is already inside an `IfChain` branch).
  */
object ExpandIsOfFormPass extends LoweringPass:

  /** @param runtimeTag
    *   the `ALValue.CaseV` tag SpecTec's `construct.ml` actually produces for
    *   this variant.
    * @param argPaths
    *   for each bound `|vN|` in prose order, the chain of `TupleProj` indices
    *   from the matched value `e` down to that binding's actual payload. `None`
    *   means the common case — every variant is flat, so `|vN|` defaults to
    *   `TupleProj(e, N)` — and only needs overriding when a variant's payload
    *   nests a sub-tuple, as GLOBAL's does.
    */
  private case class FormSpec(
    runtimeTag: String,
    argPaths: Option[List[List[Int]]] = None,
  )

  /** `Cond.IsOfForm`'s link text, mapped to how SpecTec actually represents
    * that variant. Only the variants reached so far, in "read the imports"
    * (js-api/index.bs:506-538), are covered; extend as more are reached.
    */
  private val forms: Map[String, FormSpec] = Map(
    "external-type/func" -> FormSpec("FUNC"),
    "external-type/mem" -> FormSpec("MEM"),
    "external-type/table" -> FormSpec("TABLE"),
    // globaltype = CaseV("", [mut, valtype]) is itself externtype's sole
    // positional arg (al_of_externtype / al_of_globaltype), so both bindings
    // nest one level under index 0 rather than sitting flat at 0 and 1.
    "external-type/global" -> FormSpec(
      "GLOBAL",
      Some(List(List(0, 0), List(0, 1))),
    ),
    "external-type/tag" -> FormSpec("TAG"),
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
          forms.contains(stripLink(tag)) =>
        val spec = forms(stripLink(tag))
        val paths = spec.argPaths.getOrElse(args.indices.map(List(_)).toList)
        val binds = args.zip(paths).collect {
          case (v: Expr.Var, path) =>
            Instr.Let(v, path.foldLeft(e)(Expr.TupleProj(_, _)))
        }
        val check = Cond.Eq(Expr.CaseTag(e), Expr.Str(spec.runtimeTag), neg)
        (check, binds ++ transform(body))
      case _ => (cond, transform(body))
