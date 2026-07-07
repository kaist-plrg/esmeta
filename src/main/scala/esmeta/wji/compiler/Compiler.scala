package esmeta.wji.compiler

import esmeta.wji.lang as metalang
import esmeta.wji.lang.{Algorithm, Cond, Instr}
import esmeta.wji.lang.Instr.PerformOutcome
import esmeta.wji.bridge.host.WasmHost
import esmeta.ir
import esmeta.ir.*

/** Compiles a list of [[metalang.Algorithm]]s into a real ESMeta IR [[Program]]
  * (the same `esmeta.ir` types the ECMA-262 spec compiles to), so compiled WJI
  * functions can be merged into the same `CFG` and run by the same
  * `esmeta.interpreter.Interpreter` as ordinary ES abstract operations.
  *
  * The overall pipeline: Algorithm → Func (one per algorithm) Instr →
  * List[Inst] (may expand to multiple; see If-chain grouping) metalang.Expr /
  * Cond → ir.Expr
  */
object Compiler:

  def compile(algos: List[Algorithm]): Program =
    Program(algos.flatMap(compileAlgo))

  // ── Algorithm → Func ────────────────────────────────────────────────────────

  private def compileAlgo(algo: Algorithm): Option[Func] =
    algo.name.orElse(algo.id).map { name =>
      val params = algo.params.map(p => Param(Name(stripPipes(p))))
      Func(
        main = false,
        kind = FuncKind.AbsOp,
        // lower-cased to match `nameFromLink`: Bikeshed link matching is
        // case-insensitive (e.g. a sentence-initial "Read the imports" links
        // to a dfn written "read the imports"), but Scala map lookups
        // (`cfg.fnameMap`) aren't, so both the registered name and every
        // reference to it are normalized to the same case.
        name = name.toLowerCase,
        params = params,
        retTy = UnknownType,
        body = compileInstrs(algo.body),
      )
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
    case Nil           => Nil
    case instr :: rest => compileInstr(instr) ::: compileSeq(rest)

  // ── Single instruction ───────────────────────────────────────────────────────

  private def compileInstr(instr: Instr): List[Inst] = instr match

    case Instr.Let(lhs, expr, body) =>
      val binding: Inst = lhs match
        case metalang.Expr.Var(name) => ILet(Name(name), compileExpr(expr))
        case metalang.Expr.Tuple(elems) =>
          ILet(Name("_tuple"), compileExpr(expr)) // TODO: destructure
        case _ => ILet(Name("_"), EYet(s"unsupported Let lhs: $lhs"))
      binding :: compileSeq(body)

    case Instr.Set(lhs, expr, body) =>
      IAssign(compileRef(lhs), compileExpr(expr)) :: compileSeq(body)

    case Instr.Return(Some(expr), body) =>
      compileSeq(body) :+ IReturn(compileExpr(expr))

    case Instr.Return(None, body) =>
      compileSeq(body) :+ IReturn(EUndef())

    case Instr.Assert(cond, body) =>
      IAssert(compileCond(cond)) :: compileSeq(body)

    case Instr.Throw(target, body) =>
      compileSeq(body) :+ IExpr(EYet(s"throw $target")) // TODO: proper throw

    case Instr.While(cond, body) =>
      IWhile(compileCond(cond), compileInstrs(body)) :: Nil

    case Instr.ForEach(elem, collection, body) =>
      // TODO: proper foreach — needs ERef of collection + loop var
      IExpr(
        EYet(
          s"foreach ${metalang.ExprPrinter
            .render(elem)} in ${metalang.ExprPrinter.render(collection)}",
        ),
      ) :: compileSeq(body)

    case Instr.Perform(func, args, outcome, body) =>
      val callArgs = args.map(compileExpr)
      val name = nameFromLink(func)
      val bodyInsts = compileSeq(body)
      // a Wasm Embedding function (e.g. `module_imports`) isn't a WJI/ES
      // function in `cfg.fnameMap` — it's dispatched to the live WasmHost via
      // a dedicated IR node instead of an ordinary closure call.
      def mkCall(lhs: Name): Inst =
        if WasmHost.names.contains(name) then ICallEmbed(lhs, name, callArgs)
        else ICall(lhs, EClo(name, Nil), callArgs)
      outcome match
        case PerformOutcome.Discard =>
          mkCall(Name("_")) :: bodyInsts
        case PerformOutcome.BindResult(v) =>
          mkCall(Name(stripPipes(v))) :: bodyInsts
        case PerformOutcome.ReturnResult =>
          // should have been eliminated by ExpandPerformReturnResultPass
          val tmp = Name("_ret")
          bodyInsts :+ mkCall(tmp) :+ IReturn(ERef(tmp))

    case Instr.Append(item, collection, body) =>
      IPush(
        compileExpr(item),
        compileExpr(collection),
        front = false,
      ) :: compileSeq(body)

    case Instr.Continue(_) =>
      IExpr(EYet("continue")) :: Nil // TODO: represent loop continue

    case Instr.RunInParallel(body) => compileSeq(body) // Parallele steps are simply executed immediately

    case Instr.Note(_, _) => Nil // notes are informational only

    case Instr.Unknown(text, body) =>
      IExpr(EYet(text)) :: compileSeq(body)

    case Instr.IfChain(branches, fallback) =>
      def buildChain(bs: List[(Cond, List[Instr])]): Inst = bs match
        case Nil => if fallback.isEmpty then INop() else compileInstrs(fallback)
        case (c, b) :: rest =>
          IIf(compileCond(c), compileInstrs(b), buildChain(rest))
      buildChain(branches) :: Nil

    case _: Instr.If | _: Instr.ElseIf | _: Instr.Else =>
      Nil // should have been grouped by GroupIfChainPass

  // ── Expression ───────────────────────────────────────────────────────────────

  private def compileExpr(expr: metalang.Expr): ir.Expr = expr match
    case metalang.Expr.Var(name)             => ERef(Name(name))
    case metalang.Expr.This                  => ERef(Global("this"))
    case metalang.Expr.Num(s)                => compileNum(s)
    case metalang.Expr.Bool(b)               => EBool(b)
    case metalang.Expr.Str(s)                => EStr(s)
    case metalang.Expr.SpecTerm("null")      => ENull()
    case metalang.Expr.SpecTerm("undefined") => EUndef()
    // "the current Realm Record" (ECMA-262 9.4 Execution Contexts): the Realm
    // component of the running execution context, i.e. the top frame of the
    // execution context stack. Mirrors `esmeta.compiler.currentRealm`.
    case metalang.Expr.SpecTerm("current Realm") =>
      ERef(Field(GLOBAL_CONTEXT, EStr("Realm")))
    case metalang.Expr.SpecTerm(s) => EEnum(s)
    case metalang.Expr.Field(base, name) =>
      ERef(Field(compileRef(base), EStr(name)))
    case metalang.Expr.Index(base, key) =>
      ERef(Field(compileRef(base), compileExpr(key)))
    case metalang.Expr.New(iface)      => ERecord(iface, Nil)
    case metalang.Expr.List_(elems)    => EList(elems.map(compileExpr))
    case metalang.Expr.Length(e)       => ESizeOf(compileExpr(e))
    case metalang.Expr.BinOp(l, op, r) => compileBinOp(op, l, r)
    case metalang.Expr.Pow(base, exp) =>
      EBinary(BOp.Pow, compileExpr(base), compileExpr(exp))
    case metalang.Expr.Neg(e)         => EUnary(UOp.Neg, compileExpr(e))
    case metalang.Expr.AsMath(e)      => compileExpr(e)
    case metalang.Expr.Abrupt("!", e) => compileExpr(e)
    case metalang.Expr.Abrupt(_, e) =>
      EYet(s"? ${e}") // TODO: abrupt completion check
    case metalang.Expr.AlgoCall(link, args) =>
      // zero-arg AlgoCall used as an expression value (e.g. [=error=])
      if args.isEmpty then ERef(Global(nameFromLink(link)))
      else EYet(s"call $link") // TODO: inline call-as-expr
    case metalang.Expr.JSCall(name, args) =>
      EYet(s"$$${name}(${args.mkString})") // TODO
    case metalang.Expr.Tuple(elems) => EYet(s"tuple(${elems.mkString})") // TODO
    case metalang.Expr.Map_(entries) =>
      EMap(
        (UnknownType, UnknownType),
        entries.map((k, v) => (compileExpr(k), compileExpr(v))),
      )
    case metalang.Expr.UnknownNew(raw) => EYet(raw)
    case metalang.Expr.Described(link, desc) =>
      EYet(s"$link which $desc") // TODO: relative-clause construction
    case metalang.Expr.SuchThat(desc, cond) =>
      EYet(
        s"$desc such that $cond",
      ) // TODO: existential/definite-description search
    case metalang.Expr.Unknown(raw) => EYet(raw)

  // ── Condition ────────────────────────────────────────────────────────────────

  private def compileCond(cond: Cond): ir.Expr = cond match
    case Cond.Eq(l, r, false) => EBinary(BOp.Eq, compileExpr(l), compileExpr(r))
    case Cond.Eq(l, r, true) =>
      EUnary(UOp.Not, EBinary(BOp.Eq, compileExpr(l), compileExpr(r)))
    case Cond.Compare(l, op, r) =>
      compileCompare(op, compileExpr(l), compileExpr(r))
    case Cond.And(l, r) => EBinary(BOp.And, compileCond(l), compileCond(r))
    case Cond.Or(l, r)  => EBinary(BOp.Or, compileCond(l), compileCond(r))
    case Cond.HasField(e, false)  => EExists(compileRef(e))
    case Cond.HasField(e, true)   => EUnary(UOp.Not, EExists(compileRef(e)))
    case Cond.IsType(e, t, false) => ETypeCheckName(compileExpr(e), t)
    case Cond.IsType(e, t, true) =>
      EUnary(UOp.Not, ETypeCheckName(compileExpr(e), t))
    case Cond.Abbreviated(e)      => compileExpr(e)
    case Cond.Unreachable         => EBool(false)
    case Cond.IsMissing(e, false) => EUnary(UOp.Not, EExists(compileRef(e)))
    case Cond.IsMissing(e, true)  => EExists(compileRef(e))
    case Cond.Implements(e, iface, neg) => EYet(s"implements $iface") // TODO
    case Cond.IsOfForm(e, f, _, neg)    => EYet(s"is of form") // TODO
    case Cond.Matches(l, t, r, neg)     => EYet(s"matches $t") // TODO
    case Cond.Throws(kind) =>
      EYet(s"throws${kind.fold("")(k => s" $k")}") // TODO
    case Cond.Unknown(raw) => EYet(raw)

  // ── Ref ──────────────────────────────────────────────────────────────────────

  /** Converts a metalang Expr that appears in a writable position to an IR Ref.
    */
  private def compileRef(expr: metalang.Expr): Ref = expr match
    case metalang.Expr.Var(name)         => Name(name)
    case metalang.Expr.This              => Global("this")
    case metalang.Expr.Field(base, name) => Field(compileRef(base), EStr(name))
    case metalang.Expr.Index(base, key) =>
      Field(compileRef(base), compileExpr(key))
    case metalang.Expr.AlgoCall(link, Nil) => Global(nameFromLink(link))
    case other => Name(s"_ref_${other.getClass.getSimpleName}")

  // ── Helpers ──────────────────────────────────────────────────────────────────

  private def compileNum(s: String): ir.Expr =
    val norm = s.replace("−", "-")
    val (neg, raw) =
      if norm.startsWith("-") then (true, norm.drop(1)) else (false, norm)
    val bd =
      if raw.startsWith("0x") then
        BigDecimal(java.lang.Long.parseLong(raw.drop(2), 16))
      else BigDecimal(raw)
    EMath(if neg then -bd else bd)

  private def nameFromLink(link: String): String =
    val name = link.stripPrefix("[=").stripSuffix("=]").trim
    // only `[=...=]` WJI links are case-insensitive by Bikeshed convention;
    // a bare name (as JSCall's `[$...$]` extracts it, with no brackets here)
    // is an exact ECMA-262 AO name and must keep its case to match
    // `cfg.fnameMap`.
    if link.startsWith("[=") then name.toLowerCase else name

  private def stripPipes(s: String): String =
    s.stripPrefix("|").stripSuffix("|")

  private def compileBinOp(
    op: String,
    l: metalang.Expr,
    r: metalang.Expr,
  ): ir.Expr =
    val bop: Option[BOp] = op match
      case "+"       => Some(BOp.Add)
      case "-"       => Some(BOp.Sub)
      case "*"       => Some(BOp.Mul)
      case "&div;"   => Some(BOp.Div)
      case "&minus;" => Some(BOp.Sub)
      case "modulo"  => Some(BOp.Mod)
      case _         => None
    bop match
      case Some(b) => EBinary(b, compileExpr(l), compileExpr(r))
      case None    => EYet(s"${l} $op ${r}")

  /** ESMeta's `BOp` only has `Lt` among ordering operators (no `Le`/`Gt`/`Ge`,
    * unlike WJI's own former operator set), so the other three are desugared
    * here via negation / operand-swap: `l<=r` is `!(r<l)`, `l>r` is `r<l`,
    * `l>=r` is `!(l<r)`.
    */
  private def compileCompare(op: String, l: ir.Expr, r: ir.Expr): ir.Expr =
    op match
      case "<"  => EBinary(BOp.Lt, l, r)
      case "<=" => EUnary(UOp.Not, EBinary(BOp.Lt, r, l))
      case ">"  => EBinary(BOp.Lt, r, l)
      case ">=" => EUnary(UOp.Not, EBinary(BOp.Lt, l, r))
      case _    => EBinary(BOp.Eq, l, r)
