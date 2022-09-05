package esmeta.ty.util

import esmeta.state.Grammar
import esmeta.ty.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.BasicParsers

/** metalanguage parser */
object Parser extends Parsers
trait Parsers extends BasicParsers {
  // types
  given ty: Parser[Ty] = {
    unknownTy |
    valueTy
  }.named("ty.Ty")

  lazy val unknownTy: Parser[UnknownTy] = {
    "Unknown" ~> opt("[" ~> str <~ "]") ^^ { UnknownTy(_) }
  }.named("ty.UnknownTy")

  lazy val valueTy: Parser[ValueTy] = {
    rep1sep(singleValueTy, "|") ^^ { case ts => ts.foldLeft(ValueTy())(_ | _) }
  }.named("ty.ValueTy")

  private lazy val singleValueTy: Parser[ValueTy] = {
    singleCompTy ^^ { case t => ValueTy(comp = t) } |
    singleSubMapTy ^^ { case t => ValueTy(subMap = t) } |
    singlePureValueTy ^^ { case t => ValueTy(pureValue = t) }
  }.named("ty.ValueTy (single)")

  /** completion record types */
  given compTy: Parser[CompTy] = {
    rep1sep(singleCompTy, "|") ^^ { case ts => ts.foldLeft(CompTy())(_ | _) }
  }.named("ty.CompTy")

  private lazy val singleCompTy: Parser[CompTy] = {
    "Normal[" ~> pureValueTy <~ "]" ^^ { case v => CompTy(normal = v) } |
    "Abrupt" ^^^ CompTy(abrupt = true)
  }.named("ty.CompTy (single)")

  /** pure value types (non-completion record types) */
  given pureValueTy: Parser[PureValueTy] = {
    rep1sep(singlePureValueTy, "|") ^^ {
      case ts => ts.foldLeft(PureValueTy())(_ | _)
    }
  }.named("ty.PureValueTy")

  private lazy val singlePureValueTy: Parser[PureValueTy] = {
    // ECMAScript value
    "ESValue" ^^^ ESValueT.pureValue |
    // closure
    "Clo[" ~> rep1sep(str, ",") <~ "]" ^^ {
      case s => PureValueTy(clo = Fin(s.toSet))
    } | "Clo" ^^^ PureValueTy(clo = Inf) |
    // continuation
    "Cont[" ~> rep1sep(int, ",") <~ "]" ^^ {
      case s => PureValueTy(cont = Fin(s.toSet))
    } | "Cont" ^^^ PureValueTy(cont = Inf) |
    // record
    singleRecordTy ^^ { case r => PureValueTy(record = r) } |
    // list
    singleListTy ^^ { case l => PureValueTy(list = l) } |
    // symbol
    "Symbol" ^^^ PureValueTy(symbol = true) |
    // AST value
    "Ast[" ~> rep1sep(word, ",") <~ "]" ^^ {
      case s => PureValueTy(astValue = Fin(s.toSet))
    } | "Ast" ^^^ PureValueTy(astValue = Inf) |
    // grammar
    "Grammar[" ~> rep1sep(grammar, ",") <~ "]" ^^ {
      case s => PureValueTy(grammar = Fin(s.toSet))
    } | "Grammar" ^^^ PureValueTy(grammar = Inf) |
    // code unit
    "CodeUnit" ^^^ PureValueTy(codeUnit = true) |
    // constant
    "Const[" ~> rep1sep(const, ",") <~ "]" ^^ {
      case s => PureValueTy(const = s.toSet)
    } |
    // mathematical value
    "Math" ^^^ PureValueTy(math = true) |
    // number
    "Number" ^^^ PureValueTy(number = true) |
    // big integer
    "BigInt" ^^^ PureValueTy(bigInt = true) |
    // string
    "String[" ~> rep1sep(str, ",") <~ "]" ^^ {
      case s => PureValueTy(str = Fin(s.toSet))
    } | "String" ^^^ PureValueTy(str = Inf) |
    // boolean
    "Boolean" ^^^ PureValueTy(bool = Set(true, false)) |
    "True" ^^^ PureValueTy(bool = Set(true)) |
    "False" ^^^ PureValueTy(bool = Set(false)) |
    // undefined
    "Undefined" ^^^ PureValueTy(undef = true) |
    // null
    "Null" ^^^ PureValueTy(nullv = true) |
    // absent
    "Absent" ^^^ PureValueTy(absent = true) |
    // name
    camel ^^ { case n => PureValueTy(names = Set(n)) }
  }.named("ty.PureValueTy (single)")

  private lazy val grammar: Parser[Grammar] =
    ("|" ~> word <~ "|") ~ opt(parseParams) ^^ {
      case x ~ ps => Grammar(x, ps.getOrElse(Nil))
    }
  private lazy val parseParams: Parser[List[Boolean]] =
    opt("[" ~> rep(simpleBool) <~ "]") ^^ { _.getOrElse(Nil) }
  private lazy val simpleBool: Parser[Boolean] =
    "T" ^^^ true | "F" ^^^ false
  private lazy val const: Parser[String] =
    "~" ~> "[^~]+".r <~ "~"
  private lazy val str: Parser[String] =
    """"[^"]*"""".r ^^ { case s => s.substring(1, s.length - 1) }

  /** record types */
  given recordTy: Parser[RecordTy] = {
    rep1sep(singleRecordTy, "|") ^^ {
      case ts => ts.foldLeft(RecordTy())(_ | _)
    }
  }.named("ty.RecordTy")

  private lazy val singleRecordTy: Parser[RecordTy] = {
    "{" ~> rep1sep(field, ",") <~ "}" ^^ {
      case pairs => RecordTy(pairs.toMap)
    }
  }.named("ty.RecordTy (single)")

  private lazy val field: Parser[(String, Option[ValueTy])] =
    ("[[" ~> word <~ "]]") ~ opt(":" ~> valueTy) ^^ { case k ~ v => (k, v) }

  /** list types */
  given listTy: Parser[ListTy] = {
    rep1sep(singleListTy, "|") ^^ { case ts => ts.foldLeft(ListTy())(_ | _) }
  }.named("ty.ListTy")

  private lazy val singleListTy: Parser[ListTy] = {
    "List[" ~> valueTy <~ "]" ^^ { case v => ListTy(Some(v)) } |
    "Nil" ^^^ ListTy(Some(ValueTy()))
  }.named("ty.ListTy (single)")

  /** sub map types */
  given subMapTy: Parser[SubMapTy] = {
    rep1sep(singleSubMapTy, "|") ^^ {
      case ts => ts.foldLeft(SubMapTy())(_ | _)
    }
  }.named("ty.SubMapTy")

  private lazy val singleSubMapTy: Parser[SubMapTy] = {
    "SubMap[" ~> pureValueTy ~
    ("|->" ~> pureValueTy) <~ "]" ^^ { case k ~ v => SubMapTy(k, v) }
  }.named("ty.SubMapTy (single)")
}