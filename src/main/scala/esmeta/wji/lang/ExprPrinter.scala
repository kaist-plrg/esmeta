package esmeta.wji.lang

import Expr.*

object ExprPrinter:
  def render(expr: Expr): String = expr match
    case Var(name)            => s"|$name|"
    case This                 => "**this**"
    case GivenValue           => "**the given value**"
    case Num(value)           => value
    case Byte(v)              => s"0x$v"
    case Bool(v)              => v.toString
    case Str(v)               => s""""$v""""
    case SpecTerm(name)       => name
    case Field(base, name)    => s"${render(base)}.[[${name}]]"
    case Index(base, key)     => s"${render(base)}[${render(key)}]"
    case Link(link, Nil)      => link
    case Link(link, args)     => s"$link(${args.map(render).mkString(", ")})"
    case AlgoCall(link, Nil)  => link
    case AlgoCall(link, args) => s"$link(${args.map(render).mkString(", ")})"
    case Case(tag, Nil)       => tag
    case Case(tag, args)      => s"$tag(${args.map(render).mkString(", ")})"
    case JSCall(name, Nil)    => s"[$$${name}$$]()"
    case JSCall(name, args) =>
      s"[$$${name}$$](${args.map(render).mkString(", ")})"
    case Abrupt(check, e) => s"[=$check=] ${render(e)}"
    case New(iface)       => s"new {{$iface}}"
    case UnknownNew(raw)  => s"?($raw)"
    case Length(e)        => s"length(${render(e)})"
    case BinOp(lhs, op, rhs) =>
      s"${render(lhs)} ${renderBOp(op)} ${render(rhs)}"
    case Pow(base, exp) => s"${render(base)} ** ${render(exp)}"
    case Neg(Num(v))    => s"-$v"
    case Neg(e)         => s"-(${render(e)})"
    case AsMath(e)      => s"AsMath(${render(e)})"
    case AsNumber(e)    => s"AsNumber(${render(e)})"
    case Tuple(elems)   => s"(${elems.map(render).mkString(", ")})"
    case List_(Nil)     => "« »"
    case List_(elems)   => s"« ${elems.map(render).mkString(", ")} »"
    case Map_(Nil)      => "«[ ]»"
    case Map_(entries) =>
      s"«[ ${entries.map((k, v) => s"${render(k)} → ${render(v)}").mkString(", ")} ]»"
    case Described(link, desc)   => s"$link which $desc"
    case SuchThat(desc, cond)    => s"$desc such that $cond"
    case NewByteSequence(length) => s"ByteSequence(${render(length)})"
    case Range(low, high)        => s"Range(${render(low)}, ${render(high)})"
    case IndexOf(list, elem) =>
      s"the index of ${render(list)} where ${render(elem)} is found"
    case Expr.Unknown(raw) => s"?($raw)"
    case Closure(name, captured) =>
      s"closure $name captures(${captured.mkString(", ")})"
    case FollowingSteps(params) =>
      s"followingSteps(${params.mkString(", ")})"
    case ClosureCall(closure, args) =>
      s"${render(closure)}(${args.map(render).mkString(", ")})"
    case TupleProj(base, idx) => s"${render(base)}.$idx"
    case CaseTag(base)        => s"case-tag(${render(base)})"

  private def renderBOp(op: BOp): String = op match
    case BOp.Add => "+"
    case BOp.Sub => "-"
    case BOp.Mul => "*"
    case BOp.Div => "/"
    case BOp.Mod => "modulo"
