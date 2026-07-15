package esmeta.wji.lang

import Cond.*

object CondPrinter:
  def render(cond: Cond): String = cond match
    case Eq(lhs, rhs, false) =>
      s"${ExprPrinter.render(lhs)} is ${ExprPrinter.render(rhs)}"
    case Eq(lhs, rhs, true) =>
      s"${ExprPrinter.render(lhs)} is not ${ExprPrinter.render(rhs)}"
    case Compare(lhs, op, rhs) =>
      s"${ExprPrinter.render(lhs)} ${renderOp(op)} ${ExprPrinter.render(rhs)}"
    case HasField(expr, false) => s"${ExprPrinter.render(expr)} [=map/exists=]"
    case HasField(expr, true) =>
      s"${ExprPrinter.render(expr)} [=map/doesn't exist=]"
    case Implements(expr, face, false) =>
      s"${ExprPrinter.render(expr)} [=implements=] {{$face}}"
    case Implements(expr, face, true) =>
      s"${ExprPrinter.render(expr)} does not [=implement=] {{$face}}"
    case IsOfForm(expr, form, cond, neg) =>
      val base = s"${ExprPrinter.render(expr)} ${if neg then "is not"
      else "is"} of the form ${ExprPrinter.render(form)}"
      cond.fold(base)(c => s"$base where ${render(c)}")
    case Matches(lhs, t, rhs, false) =>
      s"${ExprPrinter.render(lhs)} [=matches/$t=] ${ExprPrinter.render(rhs)}"
    case Matches(lhs, t, rhs, true) =>
      s"${ExprPrinter.render(lhs)} does not [=matches/$t=] ${ExprPrinter.render(rhs)}"
    case IsMissing(expr, false) => s"${ExprPrinter.render(expr)} is missing"
    case IsMissing(expr, true)  => s"${ExprPrinter.render(expr)} is not missing"
    case HasSlot(expr, slot, false) =>
      s"${ExprPrinter.render(expr)} has a [[$slot]] internal slot"
    case HasSlot(expr, slot, true) =>
      s"${ExprPrinter.render(expr)} does not have a [[$slot]] internal slot"
    case HasDuplicates(expr, false) =>
      s"${ExprPrinter.render(expr)} contains any duplicates"
    case HasDuplicates(expr, true) =>
      s"${ExprPrinter.render(expr)} does not contain any duplicates"
    case IsType(expr, t, neg) =>
      val article = if "aeiouAEIOU".contains(t.head) then "an" else "a"
      val verb = if neg then "is not" else "is"
      s"${ExprPrinter.render(expr)} $verb $article $t"
    case And(left, right)        => s"${render(left)}, and ${render(right)}"
    case Or(left, right)         => s"${render(left)} or ${render(right)}"
    case Abbreviated(expr)       => ExprPrinter.render(expr)
    case Cond.Unreachable        => "Unreachable"
    case Cond.Throws(None)       => "Throws"
    case Cond.Throws(Some(kind)) => s"Throws($kind)"
    case Cond.Unknown(text)      => s"?($text)"

  private def renderOp(op: CompareOp): String = op match
    case CompareOp.Lt => "<"
    case CompareOp.Le => "<="
    case CompareOp.Gt => ">"
    case CompareOp.Ge => ">="
