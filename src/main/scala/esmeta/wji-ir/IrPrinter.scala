package esmeta.wji.ir

/** Renders IR nodes to a human-readable, indented text format. */
object IrPrinter:

  def render(program: Program): String =
    program.funcs.map(render).mkString("\n\n")

  def render(func: Func): String =
    val params = func.params
      .map(p => p.name.name + (if p.optional then "?" else ""))
      .mkString(", ")
    val header = s"# ${func.name}($params)"
    val body = renderInst(func.body, indent = 1)
    s"$header\n$body"

  // ── Instructions ─────────────────────────────────────────────────────────────

  def renderInst(inst: Inst, indent: Int): String =
    val pad = "  " * indent
    inst match
      case INop()          => s"${pad}nop"
      case ISeq(Nil)       => s"${pad}nop"
      case ISeq(insts)     => insts.map(renderInst(_, indent)).mkString("\n")
      case ILet(lhs, expr) => s"${pad}let ${lhs.name} = ${renderExpr(expr)}"
      case IAssign(ref, expr) =>
        s"${pad}${renderRef(ref)} = ${renderExpr(expr)}"
      case IDelete(ref) => s"${pad}delete ${renderRef(ref)}"
      case IPush(elem, list, true) =>
        s"${pad}prepend ${renderExpr(elem)} to ${renderExpr(list)}"
      case IPush(elem, list, false) =>
        s"${pad}append ${renderExpr(elem)} to ${renderExpr(list)}"
      case IPop(lhs, list, true) =>
        s"${pad}let ${lhs.name} = pop front from ${renderExpr(list)}"
      case IPop(lhs, list, false) =>
        s"${pad}let ${lhs.name} = pop back from ${renderExpr(list)}"
      case IReturn(expr) => s"${pad}return ${renderExpr(expr)}"
      case IAssert(expr) => s"${pad}assert ${renderExpr(expr)}"
      case IExpr(expr)   => s"${pad}${renderExpr(expr)}"
      case ICall(lhs, fexpr, args) =>
        val argStr = args.map(renderExpr).mkString(", ")
        s"${pad}let ${lhs.name} = ${renderExpr(fexpr)}($argStr)"
      case ICallEmbed(lhs, fname, args) =>
        val argStr = args.map(renderExpr).mkString(", ")
        s"${pad}let ${lhs.name} = <embed $fname>($argStr)"
      case IIf(cond, thenI, INop()) =>
        s"${pad}if ${renderExpr(cond)}:\n${renderInst(thenI, indent + 1)}"
      case IIf(cond, thenI, elseI) =>
        s"${pad}if ${renderExpr(cond)}:\n${renderInst(thenI, indent + 1)}" +
        s"\n${pad}else:\n${renderInst(elseI, indent + 1)}"
      case IWhile(cond, body) =>
        s"${pad}while ${renderExpr(cond)}:\n${renderInst(body, indent + 1)}"

  // ── Expressions ──────────────────────────────────────────────────────────────

  def renderExpr(expr: Expr): String = expr match
    case ERef(ref)           => renderRef(ref)
    case EMath(n)            => n.toString
    case ENum(n)             => n.toString
    case EStr(s)             => s""""$s""""
    case EBool(b)            => b.toString
    case ENull               => "null"
    case EUndef              => "undefined"
    case EEnum(name)         => s"~$name~"
    case EUnary(UOp.Neg, e)  => s"-${renderExpr(e)}"
    case EUnary(UOp.Not, e)  => s"!${renderExpr(e)}"
    case EUnary(UOp.BNot, e) => s"~${renderExpr(e)}"
    case EBinary(op, l, r) =>
      s"(${renderExpr(l)} ${renderBOp(op)} ${renderExpr(r)})"
    case EExists(ref)        => s"exists(${renderRef(ref)})"
    case ETypeOf(e)          => s"typeof(${renderExpr(e)})"
    case ETypeCheck(e, ty)   => s"${renderExpr(e)} is $ty"
    case ERecord(tname, Nil) => s"new $tname {}"
    case ERecord(tname, flds) =>
      val fs = flds.map((k, v) => s"[[${k}]]: ${renderExpr(v)}").mkString(", ")
      s"new $tname { $fs }"
    case EList(Nil)    => "[]"
    case EList(elems)  => s"[${elems.map(renderExpr).mkString(", ")}]"
    case ELen(e)       => s"len(${renderExpr(e)})"
    case EClo(fname)   => s"@$fname"
    case EUnknown(raw) => s"?($raw)"
    case EYet(msg)     => s"!($msg)"

  // ── References ───────────────────────────────────────────────────────────────

  def renderRef(ref: Ref): String = ref match
    case Name(name)           => name
    case Temp(idx)            => s"_t$idx"
    case Global(name)         => s"%$name"
    case Field(base, EStr(k)) => s"${renderRef(base)}.[[${k}]]"
    case Field(base, key)     => s"${renderRef(base)}[${renderExpr(key)}]"

  // ── Operators ────────────────────────────────────────────────────────────────

  private def renderBOp(op: BOp): String = op match
    case BOp.Add  => "+"
    case BOp.Sub  => "-"
    case BOp.Mul  => "*"
    case BOp.Div  => "/"
    case BOp.Mod  => "%"
    case BOp.Pow  => "**"
    case BOp.Eq   => "=="
    case BOp.NEq  => "!="
    case BOp.Lt   => "<"
    case BOp.Le   => "<="
    case BOp.Gt   => ">"
    case BOp.Ge   => ">="
    case BOp.And  => "&&"
    case BOp.Or   => "||"
    case BOp.BAnd => "&"
    case BOp.BOr  => "|"
    case BOp.BXOr => "^"
