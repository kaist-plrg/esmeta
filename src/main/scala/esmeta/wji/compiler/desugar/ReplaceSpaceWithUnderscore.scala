package esmeta.wji.compiler.desugar

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}

/** Replaces every space in each algorithm's `name` with an underscore, so it
  * can be used as a valid function identifier, and does the same inside every
  * [[Expr.AlgoCall]] link and [[Instr.Perform]]'s `func` link so calls keep
  * matching their (renamed) target.
  *
  * This isn't a desugaring (it changes no control-flow/expression shape) but
  * lives alongside the other passes since it needs to run over the same
  * `List[Algorithm]` before/after which the rest of the pipeline runs. It
  * runs right after [[ResolveSpecTermsPass]], before any pass that would
  * otherwise convert `AlgoCall`s into `Perform`s.
  */
object ReplaceSpaceWithUnderscore extends DesugarPass:
  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map { a =>
      a.copy(name = a.name.map(underscore), body = a.body.map(rewriteInstr))
    }

  private def underscore(s: String): String = s.replace(' ', '_')

  private def rewriteInstr(instr: Instr): Instr =
    val e = rewriteExpr
    val c = rewriteCond
    val rewritten: Instr = instr match
      case i: Instr.Let    => i.copy(lhs = e(i.lhs), expr = e(i.expr))
      case i: Instr.Set    => i.copy(lhs = e(i.lhs), expr = e(i.expr))
      case i: Instr.If     => i.copy(cond = c(i.cond))
      case i: Instr.ElseIf => i.copy(cond = c(i.cond))
      case i: Instr.Return => i.copy(expr = i.expr.map(e))
      case i: Instr.Assert => i.copy(cond = c(i.cond))
      case i: Instr.While  => i.copy(cond = c(i.cond))
      case i: Instr.Append =>
        i.copy(item = e(i.item), collection = e(i.collection))
      case i: Instr.ForEach =>
        i.copy(elem = e(i.elem), collection = e(i.collection))
      case i: Instr.Perform =>
        i.copy(func = underscore(i.func), args = i.args.map(e))
      case i: Instr.IfChain =>
        i.copy(branches = i.branches.map((cond, body) => (c(cond), body)))
      case other => other
    rewritten.mapBody(_.map(rewriteInstr))

  private def rewriteExpr(expr: Expr): Expr =
    val go = rewriteExpr
    expr match
      case Expr.AlgoCall(link, args) =>
        Expr.AlgoCall(underscore(link), args.map(go))
      case Expr.JSCall(name, args) => Expr.JSCall(name, args.map(go))
      case Expr.Field(base, name)  => Expr.Field(go(base), name)
      case Expr.Index(base, key)   => Expr.Index(go(base), go(key))
      case Expr.Abrupt(check, e)   => Expr.Abrupt(check, go(e))
      case Expr.List_(elems)       => Expr.List_(elems.map(go))
      case Expr.Map_(entries) =>
        Expr.Map_(entries.map((k, v) => (go(k), go(v))))
      case Expr.Length(e)       => Expr.Length(go(e))
      case Expr.BinOp(l, op, r) => Expr.BinOp(go(l), op, go(r))
      case Expr.Pow(base, exp)  => Expr.Pow(go(base), go(exp))
      case Expr.Neg(e)          => Expr.Neg(go(e))
      case Expr.AsMath(e)       => Expr.AsMath(go(e))
      case Expr.Tuple(elems)    => Expr.Tuple(elems.map(go))
      case other                => other

  private def rewriteCond(cond: Cond): Cond =
    val e = rewriteExpr
    val go = rewriteCond
    cond match
      case Cond.Eq(l, r, neg)      => Cond.Eq(e(l), e(r), neg)
      case Cond.Compare(l, op, r)  => Cond.Compare(e(l), op, e(r))
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
