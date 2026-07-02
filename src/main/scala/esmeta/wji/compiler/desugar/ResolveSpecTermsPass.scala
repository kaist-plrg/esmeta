package esmeta.wji.compiler.desugar

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}

/** Reclassifies `[=...=]` links that don't name an extracted algorithm as
  * [[Expr.SpecTerm]] instead of [[Expr.AlgoCall]].
  *
  * A Bikeshed `[=...=]` autolink can point to a `<div algorithm>` (an
  * actually callable operation, e.g. `[=module_decode=]`) or to a plain
  * `<dfn>`/prose definition (e.g. `[=current Realm=]`, which links to
  * ECMA-262's "Execution Contexts" section, not an algorithm). [[ExprParser]]
  * can't tell these apart — it only ever sees one link string at a time — so
  * every bracket-link initially parses as a zero-arg `AlgoCall`. This pass
  * runs once every algorithm has been extracted, when the full set of real
  * algorithm names is known, and downgrades links that don't name one of them
  * into `SpecTerm`.
  *
  * Calls with arguments are left untouched: a link used with an argument list
  * is unambiguously a call.
  */
object ResolveSpecTermsPass extends DesugarPass:
  def run(algos: List[Algorithm]): List[Algorithm] =
    val known = algos.flatMap(_.name).toSet
    algos.map(a => a.copy(body = a.body.map(rewriteInstr(known))))

  private def rewriteInstr(known: Set[String])(instr: Instr): Instr =
    val e = rewriteExpr(known)
    val c = rewriteCond(known)
    val rewritten: Instr = instr match
      case i: Instr.Let     => i.copy(lhs = e(i.lhs), expr = e(i.expr))
      case i: Instr.Set     => i.copy(lhs = e(i.lhs), expr = e(i.expr))
      case i: Instr.If      => i.copy(cond = c(i.cond))
      case i: Instr.ElseIf  => i.copy(cond = c(i.cond))
      case i: Instr.Return  => i.copy(expr = i.expr.map(e))
      case i: Instr.Assert  => i.copy(cond = c(i.cond))
      case i: Instr.While   => i.copy(cond = c(i.cond))
      case i: Instr.Append  => i.copy(item = e(i.item), collection = e(i.collection))
      case i: Instr.Perform => i.copy(args = i.args.map(e))
      case i: Instr.IfChain =>
        i.copy(branches = i.branches.map((cond, body) => (c(cond), body)))
      case other => other
    rewritten.mapBody(_.map(rewriteInstr(known)))

  /** `AlgoCall.link` is stored with its `[=...=]` delimiters (see
    * `Compiler.nameFromLink`); `Algorithm.name` is not, so the two must be
    * normalized to the same form before comparing.
    */
  private def stripLink(link: String): String =
    link.stripPrefix("[=").stripSuffix("=]").trim

  private def rewriteExpr(known: Set[String])(expr: Expr): Expr =
    val go = rewriteExpr(known)
    expr match
      case Expr.AlgoCall(link, Nil) if !known.contains(stripLink(link)) =>
        Expr.SpecTerm(stripLink(link))
      case Expr.AlgoCall(link, args) => Expr.AlgoCall(link, args.map(go))
      case Expr.JSCall(name, args)   => Expr.JSCall(name, args.map(go))
      case Expr.Slot(base, slot)     => Expr.Slot(go(base), slot)
      case Expr.Index(base, key)     => Expr.Index(go(base), go(key))
      case Expr.Abrupt(check, e)     => Expr.Abrupt(check, go(e))
      case Expr.List_(elems)         => Expr.List_(elems.map(go))
      case Expr.Map_(entries) =>
        Expr.Map_(entries.map((k, v) => (go(k), go(v))))
      case Expr.Length(e)      => Expr.Length(go(e))
      case Expr.BinOp(l, op, r) => Expr.BinOp(go(l), op, go(r))
      case Expr.Pow(base, exp) => Expr.Pow(go(base), go(exp))
      case Expr.Neg(e)         => Expr.Neg(go(e))
      case Expr.AsMath(e)      => Expr.AsMath(go(e))
      case Expr.Tuple(elems)   => Expr.Tuple(elems.map(go))
      case other               => other

  private def rewriteCond(known: Set[String])(cond: Cond): Cond =
    val e = rewriteExpr(known)
    val go = rewriteCond(known)
    cond match
      case Cond.Eq(l, r, neg)     => Cond.Eq(e(l), e(r), neg)
      case Cond.Compare(l, op, r) => Cond.Compare(e(l), op, e(r))
      case Cond.MapExists(ex, neg) => Cond.MapExists(e(ex), neg)
      case Cond.Implements(ex, iface, neg) =>
        Cond.Implements(e(ex), iface, neg)
      case Cond.IsOfForm(ex, form, condOpt, neg) =>
        Cond.IsOfForm(e(ex), e(form), condOpt.map(go), neg)
      case Cond.Matches(l, t, r, neg) => Cond.Matches(e(l), t, e(r), neg)
      case Cond.IsMissing(ex, neg)    => Cond.IsMissing(e(ex), neg)
      case Cond.IsType(ex, t, neg)    => Cond.IsType(e(ex), t, neg)
      case Cond.And(l, r)             => Cond.And(go(l), go(r))
      case Cond.Or(l, r)              => Cond.Or(go(l), go(r))
      case Cond.Abbreviated(ex)       => Cond.Abbreviated(e(ex))
      case other                      => other
