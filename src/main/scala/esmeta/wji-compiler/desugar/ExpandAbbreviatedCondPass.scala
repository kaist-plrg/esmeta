package esmeta.wji.compiler.desugar

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}

/** Expands [[Cond.Abbreviated]] — abbreviated RHS of `or`/`and` — into full
  * conditions by inheriting the subject and operator from the left sibling.
  *
  * Examples:
  * {{{
  *   |f32| is [=+∞=] or [=−∞=]
  *     Or(Eq(f32, +∞), Abbreviated(-∞))
  *   → Or(Eq(f32, +∞), Eq(f32, -∞))
  *
  *   |valtype| [=matches/valtype=] [=v128=] or [=exnref=]
  *     Or(Matches(valtype, "valtype", v128), Abbreviated(exnref))
  *   → Or(Matches(valtype, "valtype", v128), Matches(valtype, "valtype", exnref))
  * }}}
  *
  * The template (subject + operator) is taken from the leftmost concrete
  * condition in the `Or`/`And` chain and applied to every `Abbreviated` on
  * the right.
  */
object ExpandAbbreviatedCondPass extends DesugarPass:

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = transformInstrs(a.body)))

  // ── Instr traversal ──────────────────────────────────────────────────────────

  private def transformInstrs(instrs: List[Instr]): List[Instr] =
    instrs.map(transformInstr)

  private def transformInstr(instr: Instr): Instr = instr match
    case i: Instr.If     => i.copy(cond = expandCond(i.cond),  body = transformInstrs(i.body))
    case i: Instr.ElseIf => i.copy(cond = expandCond(i.cond),  body = transformInstrs(i.body))
    case i: Instr.While  => i.copy(cond = expandCond(i.cond),  body = transformInstrs(i.body))
    case i: Instr.Assert => i.copy(cond = expandCond(i.cond),  body = transformInstrs(i.body))
    case i: Instr.IfChain =>
      i.copy(
        branches = i.branches.map((c, b) => (expandCond(c), transformInstrs(b))),
        fallback = transformInstrs(i.fallback),
      )
    case other => other.mapBody(transformInstrs)

  // ── Cond expansion ───────────────────────────────────────────────────────────

  private def expandCond(cond: Cond): Cond = cond match
    case Cond.Or(left, right) =>
      val expandedLeft = expandCond(left)
      val tmpl         = leftmostTemplate(expandedLeft)
      Cond.Or(expandedLeft, applyTemplate(right, tmpl))
    case Cond.And(left, right) =>
      val expandedLeft = expandCond(left)
      val tmpl         = leftmostTemplate(expandedLeft)
      Cond.And(expandedLeft, applyTemplate(right, tmpl))
    case other => other

  /** Extract a fill-in template `Expr => Cond` from the leftmost concrete
    * condition (skipping Or/And wrappers on the way down the left spine).
    */
  private def leftmostTemplate(cond: Cond): Option[Expr => Cond] = cond match
    case Cond.Eq(subject, _, neg)          => Some(expr => Cond.Eq(subject, expr, neg))
    case Cond.Matches(subj, mt, _, neg)    => Some(expr => Cond.Matches(subj, mt, expr, neg))
    case Cond.Or(left, _)                  => leftmostTemplate(left)
    case Cond.And(left, _)                 => leftmostTemplate(left)
    case _                                 => None

  /** Apply a template to every [[Cond.Abbreviated]] inside `cond`, while also
    * updating the template when a new concrete condition is encountered.
    */
  private def applyTemplate(cond: Cond, tmpl: Option[Expr => Cond]): Cond = cond match
    case Cond.Abbreviated(expr) =>
      tmpl.map(_(expr)).getOrElse(cond)
    case Cond.Or(left, right) =>
      val expandedLeft = applyTemplate(left, tmpl)
      val innerTmpl    = leftmostTemplate(expandedLeft).orElse(tmpl)
      Cond.Or(expandedLeft, applyTemplate(right, innerTmpl))
    case Cond.And(left, right) =>
      val expandedLeft = applyTemplate(left, tmpl)
      val innerTmpl    = leftmostTemplate(expandedLeft).orElse(tmpl)
      Cond.And(expandedLeft, applyTemplate(right, innerTmpl))
    case other =>
      expandCond(other)
