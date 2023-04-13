package esmeta.interpreter

import esmeta.EVAL_LOG_DIR
import esmeta.cfg.*
import esmeta.error.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.es.*
import esmeta.parser.{ESParser, ESValueParser}
import esmeta.state.*
import esmeta.spec.{
  SyntaxDirectedOperationHead,
  AbstractOperationHead,
  BuiltinHead,
}
import esmeta.ty.*
import esmeta.util.BaseUtils.{error => _, *}
import esmeta.util.SystemUtils.*
import esmeta.TEST_MODE
import java.io.PrintWriter
import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap}
import scala.math.{BigInt => SBigInt}

/** extensible helper of IR interpreter with a CFG */
class Interpreter(
  val st: State,
  val log: Boolean = false,
  val logDir: String = EVAL_LOG_DIR,
  val timeLimit: Option[Int] = None,
  val keepProvenance: Boolean = false,
) {
  import Interpreter.*

  /** evaluation start time */
  lazy val startTime: Long = System.currentTimeMillis

  /** final state */
  lazy val result: State =
    startTime
    while (step) {}
    if (log) pw.close
    st

  /** control flow graphs */
  inline given cfg: CFG = st.cfg

  /** ECMAScript parser */
  lazy val esParser: ESParser = cfg.esParser

  /** step */
  def step: Boolean =
    try {
      // text-based logging
      if (log)
        pw.println(st.getCursorString)
        pw.flush

      // garbage collection
      iter += 1
      if (iter % 100000 == 0) {
        for (limit <- timeLimit)
          val duration = System.currentTimeMillis - startTime
          if (duration / 1000 > limit) throw TimeoutException("interp")
        GC(st)
      }

      // cursor
      eval(st.context.cursor)
    } catch case ReturnValue(value) => { setReturn(value); true }

  /** transition for cursors */
  def eval(cursor: Cursor): Boolean = cursor match
    case NodeCursor(_, node, _) => eval(node); true
    case ExitCursor(func) =>
      st.callStack match
        case Nil =>
          st.context.retVal.map(v => st.globals += GLOBAL_RESULT -> v)
          false
        case CallContext(retId, ctxt) :: rest =>
          val value = st.context.retVal.getOrElse(throw NoReturnValue)
          st.context = ctxt
          st.callStack = rest
          setCallResult(retId, value)
          true

  /** transition for nodes */
  def eval(node: Node): Unit =
    node match {
      case Block(_, insts, _) =>
        for (inst <- insts) {
          eval(inst)
          st.context.step
        }
        st.context.moveNext
      case branch: Branch =>
        eval(branch.cond) match
          case Bool(bool) => moveBranch(branch, bool)
          case v          => throw NoBoolean(branch.cond, v)
      case call: Call => eval(call, call.callInst)
    }

  /** transition for normal instructions */
  def eval(inst: NormalInst): Unit = inst match {
    case IExpr(expr)        => eval(expr)
    case ILet(lhs, expr)    => st.context.locals += lhs -> eval(expr)
    case IAssign(ref, expr) => st.update(eval(ref), eval(expr))
    case IDelete(ref)       => st.delete(eval(ref))
    case IPush(from, to, front) =>
      eval(to) match {
        case (addr: Addr) =>
          if (front) st.prepend(addr, eval(from).toPureValue)
          else st.append(addr, eval(from).toPureValue)
        case v => throw NoAddr(to, v)
      }
    case IRemoveElem(list, elem) =>
      eval(list) match
        case (addr: Addr) => st.remove(addr, eval(elem).toPureValue)
        case v            => throw NoAddr(list, v)
    case IReturn(expr)    => throw ReturnValue(eval(expr))
    case IAssert(_: EYet) => /* skip not yet compiled assertions */
    case IAssert(expr) =>
      eval(expr) match {
        case Bool(true) =>
        case v          => throw AssertionFail(expr)
      }
    case IPrint(expr) =>
      val v = eval(expr)
      if (!TEST_MODE) println(st.getString(v))
    case INop() => /* do nothing */
  }

  /** transition for calls */
  def eval(call: Call, callInst: CallInst): Unit = callInst match {
    case ICall(lhs, fexpr, args) =>
      eval(fexpr) match
        case Clo(func, captured) =>
          val vs = args.map(eval)
          if (func.name == "ResolveBinding" && vs == List(Str("await"))) {
            /* XXX DISABLE AWAIT AS IDENTIFIER XXX */
            throw NotSupported("await as identifier")
          }
          val newLocals = getLocals(func.irFunc.params, vs) ++ captured
          st.callStack ::= CallContext(lhs, st.context)
          st.context = createContext(call, func, newLocals, st.context)
        case Cont(func, captured, callStack) => {
          val needWrapped = st.context.func.isReturnComp
          val vs =
            args
              .map(eval)
              .map(v => if (needWrapped) v.wrapCompletion else v)
          val newLocals =
            getLocals(func.irFunc.params, vs, cont = true) ++ captured
          st.callStack = callStack.map(_.copied)
          val prevCtxt = st.callStack.headOption.map(_.context)
          st.context = createContext(call, func, newLocals, prevCtxt)
        }
        case v => throw NoFunc(fexpr, v)
    case IMethodCall(lhs, base, method, args) =>
      val bv = st(eval(base))
      // TODO do not explicitly store methods in object but use a type model
      // when accessing methods
      st(bv, Str(method)) match
        case Clo(func, _) =>
          val vs = args.map(eval)
          val newLocals = getLocals(func.irFunc.params, bv :: vs)
          st.callStack ::= CallContext(lhs, st.context)
          st.context = createContext(call, func, newLocals, st.context)
        case v => throw NoFunc(callInst.fexpr, v)
    case ISdoCall(lhs, base, method, args) =>
      eval(base).asAst match
        case syn: Syntactic =>
          getSDO((syn, method)) match
            case Some((ast0, sdo)) =>
              val vs = args.map(eval)
              val newLocals =
                getLocals(sdo.irFunc.params, AstValue(ast0) :: vs)
              st.callStack ::= CallContext(lhs, st.context)
              st.context = createContext(call, sdo, newLocals, st.context)
            case None => throw InvalidAstProp(syn, Str(method))
        case lex: Lexical =>
          setCallResult(lhs, Interpreter.eval(lex, method))
  }

  /** transition for expresssions */
  def eval(expr: Expr): Value = expr match {
    case EComp(tyExpr, valExpr, tgtExpr) =>
      val y = eval(tyExpr)
      val t = eval(tgtExpr)
      val v = eval(valExpr).toPureValue
      (y, t) match
        case (y: Const, Str(t))      => Comp(y, v, Some(t))
        case (y: Const, CONST_EMPTY) => Comp(y, v, None)
        case (y: Const, t)           => throw InvalidCompTarget(y)
        case (y, t)                  => throw InvalidCompType(t)
    case EIsCompletion(expr) =>
      Bool(eval(expr).isCompletion)
    case riaExpr @ EReturnIfAbrupt(ERef(ref), check) =>
      val refV = eval(ref)
      val value = returnIfAbrupt(riaExpr, st(refV), check)
      st.update(refV, value)
      value
    case riaExpr @ EReturnIfAbrupt(expr, check) =>
      returnIfAbrupt(riaExpr, eval(expr), check)
    case EPop(list, front) =>
      eval(list) match
        case (addr: Addr) => st.pop(addr, front)
        case v            => throw NoAddr(list, v)
    case EParse(code, rule) =>
      val (str, args, locOpt) = eval(code) match
        case Str(s) => (s, List(), None)
        case AstValue(syn: Syntactic) =>
          (syn.toString(grammar, st.sourceText), syn.args, syn.loc)
        case AstValue(lex: Lexical) => (lex.str, List(), lex.loc)
        case v                      => throw InvalidParseSource(code, v)
      try {
        (eval(rule), st.sourceText, st.cachedAst) match
          // optimize the initial parsing using the given cached AST
          case (Nt("Script", Nil), Some(source), Some(ast)) if str == source =>
            AstValue(ast)
          case (Nt(name, params), _, _) =>
            val ast =
              esParser(name, if (params.isEmpty) args else params).from(str)
            // handle span of re-parsed ast
            locOpt.fold(ast.clearLoc)(ast.setBaseLoc)
            AstValue(ast)
          case (r, _, _) => throw NoNt(rule, r)
      } catch {
        case e: Throwable => st.allocList(Nil) // NOTE: throw a List of errors
      }
    case ENt(name, params) => Nt(name, params)
    case ESourceText(expr) =>
      val ast = eval(expr).asAst
      Str(ast.toString(grammar, st.sourceText).trim)
    case EGetChildren(kindOpt, ast) =>
      val kOpt = kindOpt.map(kind =>
        eval(kind) match
          case Nt(name, _) => name
          case v           => throw NoNt(kind, v),
      )
      val a = eval(ast).asAst
      (a, kOpt) match
        case (_, Some(k)) => st.allocList(a.getChildren(k).map(AstValue(_)))
        case (syn: Syntactic, None) =>
          st.allocList(syn.children.flatten.map(AstValue(_)).toList)
        case _ => throw InvalidASTChildren(a)
    case EYet(msg) =>
      throw NotSupported(msg)
    case EContains(list, elem, field) =>
      val l = eval(list).getList(list, st)
      val e = eval(elem)
      Bool(field match {
        case Some((_, f)) => l.values.exists(x => st(PropValue(x, Str(f))) == e)
        case None         => l.values.contains(e)
      })
    case ESubstring(expr, from, to) =>
      val s = eval(expr).asStr
      val f = eval(from).asInt
      Str(to.fold(s.substring(f))(eval(_) match
        case Math(n) if s.length < n => s.substring(f)
        case v                       => s.substring(f, v.asInt),
      ))
    case ERef(ref) =>
      st(eval(ref))
    case EUnary(uop, expr) =>
      val x = eval(expr)
      Interpreter.eval(uop, x)
    case EBinary(BOp.And, left, right) => shortCircuit(BOp.And, left, right)
    case EBinary(BOp.Or, left, right)  => shortCircuit(BOp.Or, left, right)
    case EBinary(BOp.Eq, ERef(ref), EAbsent()) => Bool(!st.exists(eval(ref)))
    case EBinary(bop, left, right) =>
      val l = eval(left)
      val r = eval(right)
      Interpreter.eval(bop, l, r)
    case EVariadic(vop, exprs) =>
      val vs = for (e <- exprs) yield eval(e).toPureValue
      Interpreter.eval(vop, vs)
    case EClamp(target, lower, upper) =>
      val tv = eval(target)
      val lv = eval(lower)
      val uv = eval(upper)
      (tv, lv, uv) match {
        // mathematical value operations
        case (Math(t), Math(l), Math(u)) =>
          if (t < l) Math(l)
          else if (t > u) Math(u)
          else Math(t)

        // extended mathematical value operations
        case (POS_INF, Math(l), Math(u)) => Math(u)
        case (NEG_INF, Math(l), Math(u)) => Math(l)

        case (_, _, _) =>
          throw InvalidClampOp(tv, lv, uv)
      }
    case EMathOp(mop, exprs) =>
      val vs = for (e <- exprs) yield eval(e).toPureValue
      Interpreter.eval(mop, st, vs)
    case EConvert(cop, expr) =>
      import COp.*
      (eval(expr), cop) match {
        // code unit
        case (CodeUnit(c), ToMath) => Math(BigDecimal.exact(c.toInt))
        // mathematical value
        case (Math(n), ToApproxNumber) => Number(n.toDouble) // TODO
        case (Math(n), ToNumber)       => Number(n.toDouble)
        case (Math(n), ToBigInt)       => BigInt(n.toBigInt)
        case (Math(n), ToMath)         => Math(n)
        // string
        case (Str(s), ToNumber) => Number(ESValueParser.str2Number(s))
        case (Str(s), ToBigInt) => ESValueParser.str2bigInt(s)
        case (Str(s), _: ToStr) => Str(s)
        // numbers
        case (Number(d), ToMath) => Math(BigDecimal.exact(d))
        case (Number(d), ToStr(radixOpt)) =>
          val radix = radixOpt.fold(10)(e => eval(e).asInt)
          Str(toStringHelper(d, radix))
        case (Number(d), ToNumber) => Number(d)
        case (Number(n), ToBigInt) => BigInt(BigDecimal.exact(n).toBigInt)
        // big integer
        case (BigInt(n), ToMath) => Math(BigDecimal.exact(n))
        case (BigInt(n), ToStr(radixOpt)) =>
          val radix = radixOpt.fold(10)(e => eval(e).asInt)
          Str(n.toString(radix))
        case (BigInt(n), ToBigInt) => BigInt(n)
        // invalid cases
        case (v, cop) => throw InvalidConversion(cop, expr, v)
      }
    case ETypeOf(base) =>
      Str(eval(base) match
        case n: Number => "Number"
        case b: BigInt => "BigInt"
        case s: Str    => "String"
        case b: Bool   => "Boolean"
        case Undef     => "Undefined"
        case Null      => "Null"
        case addr: Addr =>
          st(addr) match
            case m: MapObj =>
              if (tyModel.isSubTy(m.ty, "Object")) "Object"
              else m.ty
            case _: ListObj   => "List"
            case _: SymbolObj => "Symbol"
            case v            => ???
        case v => ???,
      )
    case ETypeCheck(expr, tyExpr) =>
      val v = eval(expr)
      val tyName = eval(tyExpr) match
        case Str(s)   => s
        case Nt(s, _) => s
        case v        => throw InvalidTypeExpr(expr, v)
      Bool(v match
        case _: Number => tyName == "Number"
        case _: BigInt => tyName == "BigInt"
        case _: Str    => tyName == "String"
        case _: Bool   => tyName == "Boolean"
        case _: Const  => tyName == "Constant"
        case _: Comp   => tyName == "CompletionRecord"
        case Undef     => tyName == "Undefined"
        case Null      => tyName == "Null"
        case AstValue(ast) =>
          tyName == "ParseNode" || (ast.types contains tyName)
        case _: Clo => tyName == "AbstractClosure"
        case addr: Addr =>
          st(addr) match
            case m: MapObj    => tyModel.isSubTy(m.ty, tyName)
            case _: ListObj   => tyName contains "List"
            case _: SymbolObj => tyName == "Symbol"
            case _            => ???
        case v => ???,
      )
    case EClo(fname, captured) =>
      val func = cfg.fnameMap.getOrElse(fname, throw UnknownFunc(fname))
      Clo(func, Map.from(captured.map(x => x -> st(x))))
    case ECont(fname) =>
      val func = cfg.fnameMap.getOrElse(fname, throw UnknownFunc(fname))
      val captured = st.context.locals.collect { case (x: Name, v) => x -> v }
      Cont(func, Map.from(captured), st.callStack)
    case ESyntactic(name, args, rhsIdx, children) =>
      val asts = children.map(childOpt =>
        childOpt.map(child =>
          eval(child) match {
            case AstValue(ast) => ast
            case v             => throw NoAst(child, v)
          },
        ),
      )
      AstValue(Syntactic(name, args, rhsIdx, asts))
    case ELexical(name, expr) =>
      val str = eval(expr).asStr
      AstValue(Lexical(name, str))
    case EMap("Completion", props) =>
      val map = (for {
        (kexpr, vexpr) <- props
        k = eval(kexpr)
        v = eval(vexpr)
      } yield k -> v).toMap
      (
        map.get(Str("Type")),
        map.get(Str("Value")),
        map.get(Str("Target")),
      ) match
        case (Some(ty: Const), Some(value), Some(target)) =>
          val targetOpt = target match
            case Str(target) => Some(target)
            case CONST_EMPTY => None
            case v           => throw InvalidCompTarget(v)
          Comp(ty, value.toPureValue, targetOpt)
        case _ => throw InvalidComp
    case EMap(tname, props) =>
      val addr = st.allocMap(tname)
      for ((kexpr, vexpr) <- props)
        val k = eval(kexpr).toPureValue
        val v = eval(vexpr)
        st.update(addr, k, v)
      addr
    case EList(exprs) =>
      st.allocList(exprs.map(expr => eval(expr).toPureValue))
    case EListConcat(exprs) =>
      val ls = exprs.map(e => eval(e).getList(e, st).values).flatten
      st.allocList(ls)
    case ESymbol(desc) =>
      eval(desc) match
        case (str: Str) => st.allocSymbol(str)
        case Undef      => st.allocSymbol(Undef)
        case v          => throw NoString(desc, v)
    case ECopy(obj) =>
      eval(obj) match
        case addr: Addr => st.copyObj(addr)
        case v          => throw NoAddr(obj, v)
    case EKeys(map, intSorted) =>
      eval(map) match
        // XXX WARNING - PLEASE DO NOT MERGE IT
        // XXX WARNING - PLEASE DO NOT MERGE IT
        // XXX WARNING - PLEASE DO NOT MERGE IT
        case DynamicAddr(1379) => throw NotSupported("GLOBAL OBJECT KEYS")
        case addr: Addr        => st.keys(addr, intSorted)
        case v                 => throw NoAddr(map, v)
    case EDuplicated(expr) =>
      val vs = eval(expr).getList(expr, st).values
      Bool(vs.toSet.size != vs.length)
    case EIsArrayIndex(expr) =>
      eval(expr) match
        case Str(s) =>
          val d = ESValueParser.str2Number(s)
          val ds = toStringHelper(d)
          val UPPER = (1L << 32) - 1
          val l = d.toLong
          Bool(ds == s && 0 <= l && d == l && l < UPPER)
        case _ => Bool(false)
    case EMathVal(n)           => Math(n)
    case ENumber(n) if n.isNaN => Number(Double.NaN)
    case ENumber(n)            => Number(n)
    case EBigInt(n)            => BigInt(n)
    case EStr(str)             => Str(str)
    case EBool(b)              => Bool(b)
    case EUndef()              => Undef
    case ENull()               => Null
    case EAbsent()             => Absent
    case EConst(name)          => Const(name)
    case ECodeUnit(c)          => CodeUnit(c)
  }

  /** short circuit evaluation */
  def shortCircuit(bop: BOp, left: Expr, right: Expr): Value =
    val l = eval(left)
    (bop, l) match
      case (BOp.And, Bool(false)) => Bool(false)
      case (BOp.Or, Bool(true))   => Bool(true)
      case _ =>
        val r = eval(right)
        Interpreter.eval(bop, l, r)

  /** get initial local variables */
  def getLocals(
    params: List[Param],
    args: List[Value],
    cont: Boolean = false,
  ): MMap[Local, Value] = {
    val map = MMap[Local, Value]()
    @tailrec
    def aux(ps: List[Param], as: List[Value]): Unit = (ps, as) match {
      case (Nil, Nil)                              =>
      case (Param(lhs, _, optional, _) :: pl, Nil) =>
        // TODO parameter type check
        if (optional) {
          map += lhs -> Absent
          aux(pl, Nil)
        } else RemainingParams(ps)
      case (Nil, args) =>
        // XXX Handle GeneratorStart <-> GeneratorResume arith mismatch
        if (!cont) throw RemainingArgs(args)
      case (param :: pl, arg :: al) =>
        map += param.lhs -> arg
        aux(pl, al)
    }
    aux(params, args)
    map
  }

  /** helper for return-if-abrupt cases */
  def returnIfAbrupt(
    riaExpr: EReturnIfAbrupt,
    value: Value,
    check: Boolean,
  ): Value = value match
    case NormalComp(value) => value
    case comp: Comp =>
      if (check) throw ReturnValue(value)
      else throw UncheckedAbrupt(comp)
    case pure: PureValue => pure // XXX remove?

  /** transition for references */
  def eval(ref: Ref): RefValue = ref match
    case x: Id => IdValue(x)
    case Prop(ref, expr) =>
      var base = st(eval(ref))
      val p = eval(expr)
      PropValue(base, p.toPureValue)

  /** set return value and move to the exit node */
  def setReturn(value: Value): Unit =
    // set type map
    (value, setTypeMap.get(st.context.name)) match
      case (addr: Addr, Some(tname))             => st.setType(addr, tname)
      case (NormalComp(addr: Addr), Some(tname)) => st.setType(addr, tname)
      case _                                     => /* do nothing */
    st.context.retVal = Some(
      // wrap completion by conditions specified in
      // [5.2.3.5 Implicit Normal Completion]
      // (https://tc39.es/ecma262/#sec-implicit-normal-completion)
      if (st.context.func.isReturnComp) value.wrapCompletion else value,
    )
    moveExit

  /** define call result to state and move to next */
  def setCallResult(id: Id, value: Value): Unit =
    st.define(id, value)
    st.context.moveNext

  /** sdo with default case */
  val defaultCases = List(
    "Contains",
    "AllPrivateIdentifiersValid",
    "ContainsArguments",
  )

  /** set return value and move to the exit node */
  def moveBranch(branch: Branch, cond: Boolean): Unit =
    st.context.cursor = Cursor(
      if (cond) branch.thenNode
      else branch.elseNode,
      st.func,
    )

  /** set return value and move to the exit node */
  def moveExit: Unit =
    st.context.cursor = ExitCursor(st.func)

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  /** type model */
  private def tyModel = cfg.tyModel

  /** grammar */
  private def grammar = cfg.grammar

  /** itereration count */
  private var iter = 0

  /** logging */
  private lazy val pw: PrintWriter =
    println(s"[Interpreter] Logging into $logDir...")
    mkdir(logDir)
    getPrintWriter(s"$logDir/log")

  /** get syntax-directed operation(SDO) */
  private val getSDO = cached[(Ast, String), Option[(Ast, Func)]] {
    case (ast, operation) =>
      val fnameMap = cfg.fnameMap
      ast.chains.foldLeft[Option[(Ast, Func)]](None) {
        case (None, ast0) =>
          val subIdx = getSubIdx(ast0)
          val fname = s"${ast0.name}[${ast0.idx},${subIdx}].$operation"
          fnameMap.get(fname) match
            case Some(sdo) => Some(ast0, sdo)
            case None if defaultCases contains operation =>
              Some(ast0, fnameMap(s"<DEFAULT>.$operation"))
            case _ => None
        case (res: Some[_], _) => res
      }
  }

  /** get sub index of parsed Ast */
  private val getSubIdx = cached[Ast, Int] {
    case lex: Lexical => 0
    case Syntactic(name, _, rhsIdx, children) =>
      val rhs = cfg.grammar.nameMap(name).rhsVec(rhsIdx)
      val optionals = (for {
        (opt, child) <- rhs.nts.map(_.optional) zip children if opt
      } yield !child.isEmpty)
      optionals.reverse.zipWithIndex.foldLeft(0) {
        case (acc, (true, idx)) => acc + scala.math.pow(2, idx).toInt
        case (acc, _)           => acc
      }
  }

  // get currnet provenance
  private inline given Option[Provenance] =
    if (keepProvenance) Some(st.provenance) else None

  // create a new context
  private def createContext(
    call: Call,
    func: Func,
    locals: MMap[Local, Value],
    prevCtxt: Context,
  ): Context = createContext(call, func, locals, Some(prevCtxt))
  private def createContext(
    call: Call,
    func: Func,
    locals: MMap[Local, Value],
    prevCtxt: Option[Context] = None,
  ): Context = if (keepProvenance) {
    lazy val prevFeatureStack = prevCtxt.fold(Nil)(_.featureStack)
    lazy val prevCallPath = prevCtxt.fold(CallPath())(_.callPath)
    lazy val prevNearest = prevCtxt.flatMap(_.nearest)
    func.head match
      case Some(head: SyntaxDirectedOperationHead) =>
        val feature = SyntacticFeature(func, head)
        val nearest = for {
          AstValue(ast @ Syntactic(name, _, idx, _)) <- locals.get(Name("this"))
          loc <- ast.loc
          ty = AstSingleTy(name, idx, ast.subIdx)
        } yield Nearest(ty, loc)
        Context(func, locals, feature :: prevFeatureStack, nearest)
      case Some(head: BuiltinHead) =>
        val feature = BuiltinFeature(func, head)
        Context(func, locals, feature :: prevFeatureStack)
      case _ =>
        Context(
          func,
          locals,
          prevFeatureStack,
          prevNearest,
          prevCallPath + call,
        )
  } else Context(func, locals)
}

/** IR interpreter with a CFG */
object Interpreter {
  def apply(
    st: State,
    log: Boolean = false,
    logDir: String = EVAL_LOG_DIR,
    timeLimit: Option[Int] = None,
  ): State = new Interpreter(st, log, logDir, timeLimit).result

  // type update algorithms
  val setTypeMap: Map[String, String] = Map(
    "OrdinaryFunctionCreate" -> "ECMAScriptFunctionObject",
    "ArrayCreate" -> "ArrayExoticObject",
    "OrdinaryObjectCreate" -> "OrdinaryObject",
  )

  /** transition for lexical SDO */
  def eval(lex: Lexical, sdoName: String): PureValue = {
    val Lexical(name, str) = lex
    (name, sdoName) match {
      case (
            "IdentifierName \\ (ReservedWord)" | "IdentifierName",
            "StringValue",
          ) =>
        Str(ESValueParser.parseIdentifier(str))
      case ("PrivateIdentifier", "StringValue") =>
        Str("#" + ESValueParser.parseIdentifier(str.substring(1)))
      // TODO handle numeric seperator in ESValueParser
      case ("NumericLiteral", "MV" | "NumericValue") =>
        ESValueParser.parseNumber(str.replaceAll("_", ""))
      case ("StringLiteral", "SV" | "StringValue") =>
        Str(ESValueParser.parseString(str))
      case ("NoSubstitutionTemplate", "TV") =>
        ESValueParser.parseTVNoSubstitutionTemplate(str)
      case ("TemplateHead", "TV") =>
        ESValueParser.parseTVTemplateHead(str)
      case ("TemplateMiddle", "TV") =>
        ESValueParser.parseTVTemplateMiddle(str)
      case ("TemplateTail", "TV") =>
        ESValueParser.parseTVTemplateTail(str)
      case ("NoSubstitutionTemplate", "TRV") =>
        Str(ESValueParser.parseTRVNoSubstitutionTemplate(str))
      case ("TemplateHead", "TRV") =>
        Str(ESValueParser.parseTRVTemplateHead(str))
      case ("TemplateMiddle", "TRV") =>
        Str(ESValueParser.parseTRVTemplateMiddle(str))
      case ("TemplateTail", "TRV") =>
        Str(ESValueParser.parseTRVTemplateTail(str))
      case (_, "Contains") => Bool(false)
      case ("RegularExpressionLiteral", name) =>
        throw NotSupported(s"RegularExpressionLiteral.$sdoName")
      case _ =>
        throw InvalidAstProp(lex, Str(sdoName))
    }
  }

  /** transition for unary operators */
  def eval(uop: UOp, operand: Value): Value =
    import UOp.*
    (uop, operand) match
      // mathematic values
      case (Abs, Math(n))                    => Math(n.abs)
      case (Floor, Math(n)) if n.isValidLong => Math(n)
      case (Floor, Math(n)) => Math(n - (n % 1) - (if (n < 0) 1 else 0))
      // numeric values
      case (Neg, Number(n)) => Number(-n)
      case (Neg, Math(n))   => Math(-n)
      case (Neg, BigInt(b)) => BigInt(-b)
      // boolean
      case (Not, Bool(b)) => Bool(!b)
      // bitwise
      case (BNot, Math(n))   => Math(~(n.toInt))
      case (BNot, Number(n)) => Number(~(n.toInt))
      case (BNot, BigInt(b)) => BigInt(~b)
      case (_, value)        => throw InvalidUnaryOp(uop, value)

  /** transition for binary operators */
  def eval(bop: BOp, left: Value, right: Value): Value =
    import BOp.*
    (bop, left, right) match {
      // double operations
      case (Add, Number(l), Number(r))  => Number(l + r)
      case (Sub, Number(l), Number(r))  => Number(l - r)
      case (Mul, Number(l), Number(r))  => Number(l * r)
      case (Pow, Number(l), Number(r))  => Number(math.pow(l, r))
      case (Div, Number(l), Number(r))  => Number(l / r)
      case (Mod, Number(l), Number(r))  => Number(l % r)
      case (UMod, Number(l), Number(r)) => Number(l %% r)
      case (Lt, Number(l), Number(r)) if (l equals -0.0) && (r equals 0.0) =>
        Bool(true)
      case (Lt, Number(l), Number(r)) => Bool(l < r)

      // mathematical value operations
      case (Add, Math(l), Math(r)) => Math(l + r)
      case (Sub, Math(l), Math(r)) => Math(l - r)
      case (Mul, Math(l), Math(r)) => Math(l * r)
      case (Div, Math(l), Math(r)) => Math(l / r)
      case (Mod, Math(l), Math(r)) =>
        val m = l % r
        Math(if (m * r) < 0 then r + m else m)
      case (UMod, Math(l), Math(r)) => Math(l %% r)
      case (Pow, Math(l), Math(r)) =>
        Math(math.pow(l.toDouble, r.toDouble))
      // TODO consider 2's complement 32-bit strings
      case (BAnd, Math(l), Math(r)) => Math(l.toLong & r.toLong)
      case (BOr, Math(l), Math(r))  => Math(l.toLong | r.toLong)
      case (BXOr, Math(l), Math(r)) => Math(l.toLong ^ r.toLong)
      case (LShift, Math(l), Math(r)) =>
        Math((l.toInt << r.toInt).toLong)
      case (SRShift, Math(l), Math(r)) =>
        Math((l.toInt >> r.toInt).toLong)
      case (URShift, Math(l), Math(r)) =>
        Math((l.toLong << 32) >>> (32 + (r.toLong % 32)))
      case (Lt, Math(l), Math(r)) => Bool(l < r)

      // extended mathematical value operations
      case (Lt, POS_INF, Math(r)) => Bool(false)
      case (Lt, Math(r), POS_INF) => Bool(true)
      case (Lt, NEG_INF, Math(r)) => Bool(true)
      case (Lt, Math(r), NEG_INF) => Bool(false)

      // logical operations
      case (And, Bool(l), Bool(r)) => Bool(l && r)
      case (Or, Bool(l), Bool(r))  => Bool(l || r)
      case (Xor, Bool(l), Bool(r)) => Bool(l ^ r)

      // equality operations
      case (Eq, Number(l), Number(r))     => Bool(l equals r)
      case (Eq, AstValue(l), AstValue(r)) => Bool(l eq r)
      case (Eq, l, r)                     => Bool(l == r)

      // numeric equality operations
      case (Equal, Math(l), Math(r))     => Bool(l == r)
      case (Equal, Number(l), Number(r)) => Bool(l == r)
      case (Equal, BigInt(l), BigInt(r)) => Bool(l == r)

      // big integers
      case (Add, BigInt(l), BigInt(r))     => BigInt(l + r)
      case (LShift, BigInt(l), BigInt(r))  => BigInt(l << r.toInt)
      case (SRShift, BigInt(l), BigInt(r)) => BigInt(l >> r.toInt)
      case (Sub, BigInt(l), BigInt(r))     => BigInt(l - r)
      case (Mul, BigInt(l), BigInt(r))     => BigInt(l * r)
      case (Div, BigInt(l), BigInt(r))     => BigInt(l / r)
      case (Mod, BigInt(l), BigInt(r))     => BigInt(l % r)
      case (UMod, BigInt(l), BigInt(r))    => BigInt(l %% r)
      case (Lt, BigInt(l), BigInt(r))      => Bool(l < r)
      case (BAnd, BigInt(l), BigInt(r))    => BigInt(l & r)
      case (BOr, BigInt(l), BigInt(r))     => BigInt(l | r)
      case (BXOr, BigInt(l), BigInt(r))    => BigInt(l ^ r)
      case (Pow, BigInt(l), BigInt(r))     => BigInt(l.pow(r.toInt))

      case (_, lval, rval) => throw InvalidBinaryOp(bop, lval, rval)
    }

  /** transition for variadic operators */
  def eval(vop: VOp, vs: List[PureValue]): PureValue =
    import VOp.*
    if (vs.isEmpty) throw InvalidVariadicOp(vop)
    vop match
      case Min =>
        if (vs.contains(NEG_INF)) NEG_INF
        else {
          val filtered = vs.filter(_ != POS_INF)
          if (filtered.isEmpty) POS_INF
          else vopEval(_.asMath, _ min _, Math(_), filtered)
        }
      case Max =>
        if (vs.contains(POS_INF)) POS_INF
        else {
          val filtered = vs.filter(_ != NEG_INF)
          if (filtered.isEmpty) NEG_INF
          else vopEval(_.asMath, _ min _, Math(_), filtered)
        }
        vopEval(_.asMath, _ max _, Math(_), vs)
      case Concat => vopEval(_.asStr, _ + _, Str(_), vs)

  /** transition for mathematical operators */
  def eval(mop: MOp, st: State, vs: List[PureValue]): PureValue =
    import math.*
    (mop, vs) match
      case (MOp.Expm1, List(Math(x))) => Math(expm1(x.toDouble))
      case (MOp.Log10, List(Math(x))) => Math(log10(x.toDouble))
      case (MOp.Log2, List(Math(x)))  => Math(log(x.toDouble) / log(2))
      case (MOp.Cos, List(Math(x)))   => Math(cos(x.toDouble))
      case (MOp.Cbrt, List(Math(x)))  => Math(cbrt(x.toDouble))
      case (MOp.Exp, List(Math(x)))   => Math(exp(x.toDouble))
      case (MOp.Cosh, List(Math(x)))  => Math(cosh(x.toDouble))
      case (MOp.Sinh, List(Math(x)))  => Math(sinh(x.toDouble))
      case (MOp.Tanh, List(Math(x)))  => Math(tanh(x.toDouble))
      case (MOp.Acos, List(Math(x)))  => Math(acos(x.toDouble))
      case (MOp.Acosh, List(Math(x))) => throw NotSupported("acosh")
      case (MOp.Asinh, List(Math(x))) => throw NotSupported("asinh")
      case (MOp.Atanh, List(Math(x))) => throw NotSupported("atanh")
      case (MOp.Asin, List(Math(x)))  => Math(asin(x.toDouble))
      case (MOp.Atan2, List(Math(x), Math(y))) =>
        Math(atan2(x.toDouble, y.toDouble))
      case (MOp.Atan, List(Math(x)))  => Math(atan(x.toDouble))
      case (MOp.Log1p, List(Math(x))) => Math(log1p(x.toDouble))
      case (MOp.Log, List(Math(x)))   => Math(log(x.toDouble))
      case (MOp.Sin, List(Math(x)))   => Math(sin(x.toDouble))
      case (MOp.Sqrt, List(Math(x)))  => Math(sqrt(x.toDouble))
      case (MOp.Tan, List(Math(x)))   => Math(tan(x.toDouble))
      case (MOp.Hypot, List(addr: Addr)) =>
        st(addr) match
          case ListObj(vs) =>
            val ns = vs.map {
              case Math(n) => n.toDouble
              case _       => throw InvalidMathOp(mop, vs.toList)
            }
            Math(sqrt(ns.map(x => x * x).foldLeft(0.0)(_ + _)))
          case _ => throw InvalidMathOp(mop, vs)
      case _ => throw InvalidMathOp(mop, vs)

  // helpers for make transition for variadic operators
  private def vopEval[T](
    f: PureValue => T,
    op: (T, T) => T,
    g: T => PureValue,
    vs: List[PureValue],
  ) = g(vs.map(f).reduce(op))
}
