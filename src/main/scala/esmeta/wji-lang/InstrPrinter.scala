package esmeta.wji.lang

import Instr.*
import Instr.PerformOutcome.*

/** Renders the full AST ([[Algorithm]] → [[Instr]]) to a human-readable,
  * indented form for visual inspection.
  *
  * Sub-expression rendering delegates to [[ExprPrinter]] and [[CondPrinter]].
  */
object InstrPrinter:

  def render(algo: Algorithm): String =
    val sb = new StringBuilder
    sb.append(s"# ${algo.name.orElse(algo.id).getOrElse("<unnamed>")}\n")
    algo.id.foreach(id => sb.append(s"  id: $id\n"))
    if algo.params.nonEmpty then
      sb.append(s"  params: ${algo.params.mkString(", ")}\n")
    if algo.head.nonEmpty then sb.append(s"  head: ${algo.head}\n")
    renderInstrs(sb, algo.body, indent = 1)
    sb.toString

  private def renderInstrs(
    sb: StringBuilder,
    instrs: List[Instr],
    indent: Int,
  ): Unit =
    for instr <- instrs do
      sb.append("  " * indent).append(render(instr)).append("\n")
      renderInstrs(sb, instr.body, indent + 1)

  private def renderCall(func: String, args: List[Expr]): String =
    if args.isEmpty then func
    else s"$func(${args.map(ExprPrinter.render).mkString(", ")})"

  def render(instr: Instr): String = instr match
    case Let(lhs, expr, _) =>
      s"Let(${ExprPrinter.render(lhs)}, ${ExprPrinter.render(expr)})"
    case Set(lhs, expr, _) =>
      s"Set(${ExprPrinter.render(lhs)}, ${ExprPrinter.render(expr)})"
    case If(cond, _)     => s"If(${CondPrinter.render(cond)})"
    case ElseIf(cond, _) => s"ElseIf(${CondPrinter.render(cond)})"
    case Else(_)         => "Else"
    case Return(expr, _) =>
      s"Return(${expr.map(ExprPrinter.render).getOrElse("")})"
    case Assert(cond, _)        => s"Assert(${CondPrinter.render(cond)})"
    case Throw(target, _)       => s"Throw($target)"
    case ForEach(elem, coll, _) => s"ForEach($elem, $coll)"
    case While(cond, _)         => s"While(${CondPrinter.render(cond)})"
    case RunInParallel(_)       => "RunInParallel"
    case Append(item, coll, _) =>
      s"Append(${ExprPrinter.render(item)}, ${ExprPrinter.render(coll)})"
    case Continue(_) => "Continue"
    case Perform(func, args, Discard, _) =>
      s"Perform(${renderCall(func, args)})"
    case Perform(func, args, ReturnResult, _) =>
      s"PerformAndReturn(${renderCall(func, args)})"
    case Perform(func, args, BindResult(v), _) =>
      s"PerformAndLet(${renderCall(func, args)}, $v)"
    case Note(text, _)          => s"Note($text)"
    case Instr.Unknown(text, _) => s"?($text)"
