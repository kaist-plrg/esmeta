package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}

/** Reshapes an `Expr.Case` built from a SpecTec Wasm Core Spec link (by
  * [[ResolveLinksPass]], the pass right before this one) whose real runtime
  * `ALValue.CaseV` nests more deeply than spec prose writes it, wherever such
  * a `Case` shows up — a value under construction, or a `Cond.IsOfForm`'s
  * `form` pattern.
  *
  * Two known mismatches, both `construct.ml`-confirmed:
  *   - a numeric const (`i32.const`/`i64.const`/`f32.const`/`f64.const`):
  *     `al_to_num` only recognizes `CaseV("CONST", [CaseV(numtypeTag, []),
  *     payload])` — a nested numtype tag, not the flat `CaseV("I32.CONST",
  *     [payload])` `ResolveLinksPass` would otherwise leave in place.
  *   - `external-type/global`: `al_of_externtype`/`al_of_globaltype` nest
  *     `mut`/`valuetype` one level under externtype's GLOBAL variant's sole
  *     arg, even though index.bs writes both components flat directly under
  *     `external-type/global`.
  * {{{
  *   Case("[=i32.const=]", [Var(u32)])
  * }}}
  * becomes
  * {{{
  *   Case("CONST", [Case("I32", []), Var(u32)])
  * }}}
  * and
  * {{{
  *   Case("[=external-type/global=]", [Var(mut), Var(valuetype)])
  * }}}
  * becomes
  * {{{
  *   Case("[=external-type/global=]", [Case("", [Var(mut), Var(valuetype)])])
  * }}}
  *
  * Every other `Case` (an already-correctly-flat variant like
  * `external-type/func`, or one [[ExprParser]] built directly rather than via
  * a link, like `CompTypeArrow`'s `"->"` or `parseUntaggedForm`'s `""`) just
  * has its args recursed into unchanged — this pass only ever *adds*
  * structure for the two mappings above, never removes or reinterprets
  * anything else. Once this runs, every downstream pass (`ExpandIsOfFormPass`,
  * `Compiler`) can treat any `Expr.Case` it sees as already shaped exactly
  * like the real `ALValue.CaseV` it corresponds to, with zero SpecTec
  * knowledge of its own.
  *
  * Category: SpecTec dependent.
  */
object NormalizeSpecTecCaseShapePass extends LoweringPass:

  /** Requires:
    *   - [[ResolveLinksPass]]: needs every `Expr.Link` already resolved to
    *     `Expr.Case`/`Expr.AlgoCall`/`Expr.SpecTerm` — nothing here resolves a
    *     raw `Link` itself, just reshapes an already-built `Case`.
    */
  override def requires: Set[LoweringPass] = Set(ResolveLinksPass)

  /** `Link`/`Case`'s tag text may still carry its `[=...=]` delimiters (see
    * `ResolveLinksPass.stripLink`) — strip them the same way before comparing
    * against [[numConstTags]]/[[nestedFormLinks]]'s (bare) keys.
    */
  private def stripLink(tag: String): String =
    tag.stripPrefix("[=").stripSuffix("=]").trim

  /** A numeric-const link (e.g. `[=i32.const=]`), mapped to the nested numtype
    * tag `construct.ml`'s `al_to_num` actually expects at position 0 of a
    * `CaseV("CONST", [nested, payload])`.
    */
  private val numConstTags: Map[String, String] = Map(
    "i32.const" -> "I32",
    "i64.const" -> "I64",
    "f32.const" -> "F32",
    "f64.const" -> "F64",
  )

  /** Link names whose args should be wrapped in one extra untagged `Case("",
    * args)` — see class doc's `external-type/global` example. A genuine
    * *value* is never constructed against this link in the corpus today (only
    * matched against, via `Cond.IsOfForm`), but the reshaping itself doesn't
    * need to care which context a matching `Case` shows up in.
    */
  private val nestedFormLinks: Set[String] = Set("external-type/global")

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = a.body.map(reshapeInstr)))

  private def reshape(expr: Expr): Expr = expr match
    case Expr.Case(tag, args) =>
      val reshapedArgs = args.map(reshape)
      numConstTags.get(stripLink(tag).toLowerCase) match
        case Some(numTag) =>
          Expr.Case("CONST", Expr.Case(numTag, Nil) :: reshapedArgs)
        case None =>
          if nestedFormLinks.contains(stripLink(tag).toLowerCase) then
            Expr.Case(tag, List(Expr.Case("", reshapedArgs)))
          else Expr.Case(tag, reshapedArgs)
    case Expr.AlgoCall(link, args) => Expr.AlgoCall(link, args.map(reshape))
    case Expr.JSCall(name, args)   => Expr.JSCall(name, args.map(reshape))
    case Expr.Field(base, name)    => Expr.Field(reshape(base), name)
    case Expr.Index(base, key)     => Expr.Index(reshape(base), reshape(key))
    case Expr.Abrupt(check, e)     => Expr.Abrupt(check, reshape(e))
    case Expr.List_(elems)         => Expr.List_(elems.map(reshape))
    case Expr.Map_(entries) =>
      Expr.Map_(entries.map((k, v) => (reshape(k), reshape(v))))
    case Expr.Length(e)       => Expr.Length(reshape(e))
    case Expr.BinOp(l, op, r) => Expr.BinOp(reshape(l), op, reshape(r))
    case Expr.Pow(base, exp)  => Expr.Pow(reshape(base), reshape(exp))
    case Expr.Neg(e)          => Expr.Neg(reshape(e))
    case Expr.AsMath(e)       => Expr.AsMath(reshape(e))
    case Expr.Tuple(elems)    => Expr.Tuple(elems.map(reshape))
    case Expr.NewByteSequence(length) => Expr.NewByteSequence(reshape(length))
    case Expr.Range(low, high)        => Expr.Range(reshape(low), reshape(high))
    case Expr.ClosureCall(closure, args) =>
      Expr.ClosureCall(reshape(closure), args.map(reshape))
    case other => other

  private def reshapeCond(cond: Cond): Cond = cond match
    case Cond.Eq(l, r, neg)     => Cond.Eq(reshape(l), reshape(r), neg)
    case Cond.Compare(l, op, r) => Cond.Compare(reshape(l), op, reshape(r))
    case Cond.HasField(ex, neg) => Cond.HasField(reshape(ex), neg)
    case Cond.Implements(ex, iface, neg) =>
      Cond.Implements(reshape(ex), iface, neg)
    case Cond.IsOfForm(ex, form, condOpt, neg) =>
      Cond.IsOfForm(reshape(ex), reshape(form), condOpt.map(reshapeCond), neg)
    case Cond.Matches(l, t, r, neg) =>
      Cond.Matches(reshape(l), t, reshape(r), neg)
    case Cond.IsMissing(ex, neg) => Cond.IsMissing(reshape(ex), neg)
    case Cond.IsType(ex, t, neg) => Cond.IsType(reshape(ex), t, neg)
    case Cond.And(l, r)          => Cond.And(reshapeCond(l), reshapeCond(r))
    case Cond.Or(l, r)           => Cond.Or(reshapeCond(l), reshapeCond(r))
    case Cond.Abbreviated(ex)    => Cond.Abbreviated(reshape(ex))
    case Cond.Exists(binder, collections, body) =>
      Cond.Exists(binder, collections.map(reshape), reshapeCond(body))
    case other => other

  private def reshapeInstr(instr: Instr): Instr =
    val rewritten: Instr = instr match
      case i: Instr.Let    => i.copy(lhs = reshape(i.lhs), expr = reshape(i.expr))
      case i: Instr.Set    => i.copy(lhs = reshape(i.lhs), expr = reshape(i.expr))
      case i: Instr.If     => i.copy(cond = reshapeCond(i.cond))
      case i: Instr.ElseIf => i.copy(cond = reshapeCond(i.cond))
      case i: Instr.Return => i.copy(expr = i.expr.map(reshape))
      case i: Instr.Assert => i.copy(cond = reshapeCond(i.cond))
      case i: Instr.While  => i.copy(cond = reshapeCond(i.cond))
      case i: Instr.Append =>
        i.copy(item = reshape(i.item), collection = reshape(i.collection))
      case i: Instr.ForEach =>
        i.copy(elem = reshape(i.elem), collection = reshape(i.collection))
      case i: Instr.Perform => i.copy(args = i.args.map(reshape))
      case i: Instr.IfChain =>
        i.copy(branches = i.branches.map((cond, body) => (reshapeCond(cond), body)))
      case other => other
    rewritten.mapBody(_.map(reshapeInstr))
