package esmeta.wji.compiler

import esmeta.wji.lang as metalang
import esmeta.wji.lang.{Algorithm, Cond, Instr}
import esmeta.wji.lang.Instr.PerformOutcome
import esmeta.wji.ir
import esmeta.wji.ir.*

/** Compiles a list of [[metalang.Algorithm]]s into an IR [[Program]].
  *
  * The overall pipeline:
  *   Algorithm  →  Func        (one per algorithm)
  *   Instr      →  List[Inst]  (may expand to multiple; see If-chain grouping)
  *   metalang.Expr / Cond  →  ir.Expr
  */
object Compiler:

  def compile(algos: List[Algorithm]): Program =
    Program(algos.flatMap(compileAlgo))

  // ── Algorithm → Func ────────────────────────────────────────────────────────

  private def compileAlgo(algo: Algorithm): Option[Func] =
    algo.name.orElse(algo.id).map { name =>
      val params = algo.params.map(p => Param(Name(stripPipes(p))))
      Func(name, params, compileInstrs(algo.body))
    }

  // ── Instruction sequence ─────────────────────────────────────────────────────

  /** Compiles a flat metalang instruction list into a single [[ISeq]].
    *
    * `If`/`ElseIf`/`Else` siblings are collected and folded into a nested
    * [[IIf]] chain before individual instructions are compiled.
    */
  private def compileInstrs(instrs: List[Instr]): Inst =
    ISeq(compileSeq(instrs))

  private def compileSeq(instrs: List[Instr]): List[Inst] = instrs match
    case Nil          => Nil
    case instr :: rest => compileInstr(instr) ::: compileSeq(rest)

  // ── Single instruction ───────────────────────────────────────────────────────

  private def compileInstr(instr: Instr): List[Inst] = instr match

    case Instr.Let(lhs, expr, body) =>
      val binding: Inst = lhs match
        case metalang.Expr.Var(name)      => ILet(Name(name), compileExpr(expr))
        case metalang.Expr.Tuple(elems)   => ILet(Name("_tuple"), compileExpr(expr)) // TODO: destructure
        case _                            => ILet(Name("_"), EYet(s"unsupported Let lhs: $lhs"))
      binding :: compileSeq(body)

    case Instr.Set(lhs, expr, body) =>
      IAssign(compileRef(lhs), compileExpr(expr)) :: compileSeq(body)

    case Instr.Return(Some(expr), body) =>
      compileSeq(body) :+ IReturn(compileExpr(expr))

    case Instr.Return(None, body) =>
      compileSeq(body) :+ IReturn(EUndef)

    case Instr.Assert(cond, body) =>
      IAssert(compileCond(cond)) :: compileSeq(body)

    case Instr.Throw(target, body) =>
      compileSeq(body) :+ IExpr(EYet(s"throw $target"))  // TODO: proper throw

    case Instr.While(cond, body) =>
      IWhile(compileCond(cond), compileInstrs(body)) :: Nil

    case Instr.ForEach(elem, collection, body) =>
      // TODO: proper foreach — needs ERef of collection + loop var
      IExpr(EYet(s"foreach $elem in $collection")) :: compileSeq(body)

    case Instr.Perform(func, args, outcome, body) =>
      val callArgs = args.map(compileExpr)
      val callee   = compileFuncRef(func)
      val bodyInsts = compileSeq(body)
      outcome match
        case PerformOutcome.Discard =>
          ICall(Name("_"), callee, callArgs) :: bodyInsts
        case PerformOutcome.BindResult(v) =>
          ICall(Name(stripPipes(v)), callee, callArgs) :: bodyInsts
        case PerformOutcome.ReturnResult =>
          // should have been eliminated by ExpandPerformReturnResultPass
          val tmp = Name("_ret")
          bodyInsts :+ ICall(tmp, callee, callArgs) :+ IReturn(ERef(tmp))

    case Instr.Append(item, collection, body) =>
      IPush(compileExpr(item), compileExpr(collection), front = false) :: compileSeq(body)

    case Instr.Continue(_) =>
      IExpr(EYet("continue")) :: Nil   // TODO: represent loop continue

    case Instr.RunInParallel(body) =>
      compileSeq(body)                 // TODO: concurrency

    case Instr.Note(_, _) => Nil       // notes are informational only

    case Instr.Unknown(text, body) =>
      IExpr(EUnknown(text)) :: compileSeq(body)

    case Instr.IfChain(branches, fallback) =>
      def buildChain(bs: List[(Cond, List[Instr])]): Inst = bs match
        case Nil              => if fallback.isEmpty then INop() else compileInstrs(fallback)
        case (c, b) :: rest   => IIf(compileCond(c), compileInstrs(b), buildChain(rest))
      buildChain(branches) :: Nil

    case _: Instr.If | _: Instr.ElseIf | _: Instr.Else =>
      Nil  // should have been grouped by GroupIfChainPass

  // ── Expression ───────────────────────────────────────────────────────────────

  private def compileExpr(expr: metalang.Expr): ir.Expr = expr match
    case metalang.Expr.Var(name)          => ERef(Name(name))
    case metalang.Expr.This               => ERef(Global("this"))
    case metalang.Expr.Num(s)             => compileNum(s)
    case metalang.Expr.Bool(b)            => EBool(b)
    case metalang.Expr.Str(s)             => EStr(s)
    case metalang.Expr.SpecConst("null")  => ENull
    case metalang.Expr.SpecConst("undefined") => EUndef
    case metalang.Expr.SpecConst(s)       => EEnum(s)
    case metalang.Expr.Slot(base, slot)   => ERef(Field(compileRef(base), EStr(slot)))
    case metalang.Expr.Index(base, key)   => ERef(Field(compileRef(base), compileExpr(key)))
    case metalang.Expr.New(iface)         => ERecord(iface, Nil)
    case metalang.Expr.List_(elems)       => EList(elems.map(compileExpr))
    case metalang.Expr.Length(e)          => ELen(compileExpr(e))
    case metalang.Expr.BinOp(l, op, r)    => compileBinOp(op, l, r)
    case metalang.Expr.Pow(base, exp)     => EBinary(BOp.Pow, compileExpr(base), compileExpr(exp))
    case metalang.Expr.Neg(e)             => EUnary(UOp.Neg, compileExpr(e))
    case metalang.Expr.AsMath(e)          => compileExpr(e)
    case metalang.Expr.Abrupt("!", e)     => compileExpr(e)
    case metalang.Expr.Abrupt(_, e)       => EYet(s"? ${e}")  // TODO: abrupt completion check
    case metalang.Expr.AlgoCall(link, args) =>
      // zero-arg AlgoCall used as an expression value (e.g. [=error=])
      if args.isEmpty then ERef(Global(nameFromLink(link)))
      else EYet(s"call $link")          // TODO: inline call-as-expr
    case metalang.Expr.JSCall(name, args) => EYet(s"$$${name}(${args.mkString})")  // TODO
    case metalang.Expr.Tuple(elems)       => EYet(s"tuple(${elems.mkString})")     // TODO
    case metalang.Expr.Map_(entries)      => EYet("map literal")                   // TODO
    case metalang.Expr.UnknownNew(raw)    => EUnknown(raw)
    case metalang.Expr.Unknown(raw)       => EUnknown(raw)

  // ── Condition ────────────────────────────────────────────────────────────────

  private def compileCond(cond: Cond): ir.Expr = cond match
    case Cond.Eq(l, r, false)        => EBinary(BOp.Eq,  compileExpr(l), compileExpr(r))
    case Cond.Eq(l, r, true)         => EBinary(BOp.NEq, compileExpr(l), compileExpr(r))
    case Cond.Compare(l, op, r)      => EBinary(compileCompOp(op), compileExpr(l), compileExpr(r))
    case Cond.And(l, r)              => EBinary(BOp.And, compileCond(l), compileCond(r))
    case Cond.Or(l, r)               => EBinary(BOp.Or,  compileCond(l), compileCond(r))
    case Cond.MapExists(e, false)    => EExists(compileRef(e))
    case Cond.MapExists(e, true)     => EUnary(UOp.Not, EExists(compileRef(e)))
    case Cond.IsType(e, t, false)    => ETypeCheck(compileExpr(e), t)
    case Cond.IsType(e, t, true)     => EUnary(UOp.Not, ETypeCheck(compileExpr(e), t))
    case Cond.Abbreviated(e)         => compileExpr(e)
    case Cond.Unreachable            => EBool(false)
    case Cond.IsMissing(e, false)    => EUnary(UOp.Not, EExists(compileRef(e)))
    case Cond.IsMissing(e, true)     => EExists(compileRef(e))
    case Cond.Implements(e, iface, neg) => EYet(s"implements $iface")  // TODO
    case Cond.IsOfForm(e, f, _, neg) => EYet(s"is of form")            // TODO
    case Cond.Matches(l, t, r, neg)  => EYet(s"matches $t")            // TODO
    case Cond.Unknown(raw)           => EUnknown(raw)

  // ── Ref ──────────────────────────────────────────────────────────────────────

  /** Converts a metalang Expr that appears in a writable position to an IR Ref. */
  private def compileRef(expr: metalang.Expr): Ref = expr match
    case metalang.Expr.Var(name)        => Name(name)
    case metalang.Expr.This             => Global("this")
    case metalang.Expr.Slot(base, slot) => Field(compileRef(base), EStr(slot))
    case metalang.Expr.Index(base, key) => Field(compileRef(base), compileExpr(key))
    case metalang.Expr.AlgoCall(link, Nil) => Global(nameFromLink(link))
    case other                          => Name(s"_ref_${other.getClass.getSimpleName}")

  // ── Helpers ──────────────────────────────────────────────────────────────────

  private def compileNum(s: String): ir.Expr =
    val norm = s.replace("−", "-")
    val (neg, raw) = if norm.startsWith("-") then (true, norm.drop(1)) else (false, norm)
    val bd =
      if raw.startsWith("0x") then BigDecimal(java.lang.Long.parseLong(raw.drop(2), 16))
      else BigDecimal(raw)
    EMath(if neg then -bd else bd)

  private def compileFuncRef(link: String): ir.Expr =
    EClo(nameFromLink(link))

  private def nameFromLink(link: String): String =
    link.stripPrefix("[=").stripSuffix("=]").trim

  private def stripPipes(s: String): String =
    s.stripPrefix("|").stripSuffix("|")

  private def compileBinOp(op: String, l: metalang.Expr, r: metalang.Expr): ir.Expr =
    val bop: Option[BOp] = op match
      case "+"      => Some(BOp.Add)
      case "-"      => Some(BOp.Sub)
      case "*"      => Some(BOp.Mul)
      case "&div;"  => Some(BOp.Div)
      case "&minus;" => Some(BOp.Sub)
      case "modulo" => Some(BOp.Mod)
      case _        => None
    bop match
      case Some(b) => EBinary(b, compileExpr(l), compileExpr(r))
      case None    => EYet(s"${l} $op ${r}")

  private def compileCompOp(op: String): BOp = op match
    case "<"  => BOp.Lt
    case "<=" => BOp.Le
    case ">"  => BOp.Gt
    case ">=" => BOp.Ge
    case _    => BOp.Eq
