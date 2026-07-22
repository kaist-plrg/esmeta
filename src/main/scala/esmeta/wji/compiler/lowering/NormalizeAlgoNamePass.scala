package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, AlgorithmKind, Cond, Expr, Instr}

/** Normalizes each `Plain`/`Method`-kind algorithm's `name` —
  * space-to-underscore *and* lower-cased, so it's both a valid function
  * identifier and matches how `esmeta.wji.compiler.Compiler.compileAlgo`
  * registers `Func` names — and does the same inside every [[Expr.Closure]]
  * reference to a (possibly synthetic, lowering-pass-generated) algorithm name,
  * so a closure reference always matches its target's registered name exactly
  * (`cfg.fnameMap` lookups are case-sensitive even though the Bikeshed prose
  * these names ultimately derive from is not).
  *
  * A Getter/Setter/Constructor-kind algorithm's `name` is left exactly as
  * extracted instead — `Compiler.compileAlgo` registers those case-preserved
  * (see `AddInterfaceMemberBuiltinBehaviourPass`), matching the real,
  * case-sensitive JS property names `manuals/intrinsics` expects (e.g.
  * `Global.value`'s setter) — every one of those kinds is only ever reached via
  * real property/call access, never via a `[=link=]`-style reference
  * `nameFromLink`'s Bikeshed case-insensitivity exists for in the first place.
  * `Method` is still normalized/lowercased like `Plain` for now — it's
  * deliberately excluded from that case-preserved builtin treatment (TODO, see
  * `AddInterfaceMemberBuiltinBehaviourPass`'s doc), so it's still only ever
  * reached via `[=link=]`-style references today.
  *
  * [[Expr.AlgoCall]]'s `link` and [[Instr.Perform]]'s `func` are deliberately
  * left on space-only normalization here (not lower-cased): unlike
  * `Algorithm.name`/`Closure.name`, a `func`/`link` can also be a literal
  * reference to a manually-registered native hook (e.g.
  * `"HostEnqueuePromiseJob"`) whose real name is case-sensitive, not a
  * lower-cased Bikeshed dfn — lower-casing those is instead handled
  * conditionally at compile time by `Compiler.nameFromLink`, which only folds
  * case for `[=...=]`-wrapped (i.e. genuinely Bikeshed-derived) references.
  *
  * This isn't a desugaring/lowering (it changes no control-flow/expression
  * shape) but lives alongside the other passes since it needs to run over the
  * same `List[Algorithm]` before/after which the rest of the pipeline runs.
  *
  * Deliberately positioned last, not because it `requires` any one specific
  * pass, but because it needs to see the *final* set of `Algorithm.name`/
  * `Expr.Closure`/`Expr.AlgoCall`/`Instr.Perform.func` occurrences — several
  * earlier passes (`ExpandFollowingStepsPass`, `ExpandQueueATaskPass`,
  * `WrapCompletionReturnsPass`, ...) each introduce more of these as they run.
  * Normalization is idempotent and order-independent given the final tree, so
  * running last (rather than declaring a `requires` on every producer) is both
  * correct and simpler — no `LoweringPass` before it needs anything back *from*
  * it, so nothing stops it running anywhere after all of them; last just
  * guarantees it's really seen everything.
  *
  * Category: Housekeeping.
  */
object NormalizeAlgoNamePass extends LoweringPass:
  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map { a =>
      val name = a.kind match
        case AlgorithmKind.Plain | AlgorithmKind.Method(_) =>
          a.name.map(normalize)
        case _ => a.name
      a.copy(name = name, body = a.body.map(rewriteInstr))
    }

  private def underscore(s: String): String = s.replace(' ', '_')
  private def normalize(s: String): String = underscore(s).toLowerCase

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
      // unlike AlgoCall's link, a Case's tag is never looked up as a function
      // name (it compiles to a literal runtime CaseV tag string, see
      // Compiler), so it's left untouched — only its args are rewritten.
      case Expr.Case(tag, args)    => Expr.Case(tag, args.map(go))
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
      case Expr.Closure(name, captured) =>
        Expr.Closure(normalize(name), captured)
      case other => other

  private def rewriteCond(cond: Cond): Cond =
    val e = rewriteExpr
    val go = rewriteCond
    cond match
      case Cond.Eq(l, r, neg)     => Cond.Eq(e(l), e(r), neg)
      case Cond.Compare(l, op, r) => Cond.Compare(e(l), op, e(r))
      case Cond.HasField(ex, neg) => Cond.HasField(e(ex), neg)
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
