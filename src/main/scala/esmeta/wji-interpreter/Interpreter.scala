package esmeta.wji.interpreter

import esmeta.wji.ir.*
import esmeta.wji.bridge.host.{HostFunction, WasmError, WasmHost}
import esmeta.wji.state.{ALNum, ALValue, Heap, WjValue}

/** Raised when interpretation cannot proceed (unsupported construct, type
  * mismatch, or a [[WasmError]] propagated from the [[WasmHost]]).
  */
final class InterpreterError(msg: String) extends RuntimeException(msg)

/** Direct (tree-walking) interpreter for the wjmeta IR.
  *
  * Resolves [[ICall]] callees first against the user-defined [[Program]]
  * functions, then against the Wasm embedding operations exposed by `host`.
  * Embedding calls cross the [[WasmHost]] boundary, where [[WjValue.Wasm]] is
  * unwrapped to [[ALValue]] and results are wrapped back.
  */
class Interpreter(program: Program, host: WasmHost):

  private type Locals = Map[String, WjValue]

  /** Control-flow signal threaded out of [[execInst]]. */
  private enum Signal:
    case Next
    case Ret(v: WjValue)

  /** Global (non-local) variables, e.g. `%Store`. */
  private val globals = scala.collection.mutable.Map.empty[String, WjValue]

  /** The live function call stack, outermost first.
    *
    * Only pushed; frames are popped only on normal return. When an
    * [[InterpreterError]] propagates, the frames remain so callers can inspect
    * the chain that led to the failure.
    */
  val callStack: scala.collection.mutable.ArrayBuffer[String] =
    scala.collection.mutable.ArrayBuffer.empty
  
  /** Heap shared by global initializers and all invocations, so heap-backed
    * globals ([[WjValue.Record]]/[[WjValue.List]]) stay valid across calls. */
  private val heap = Heap()

  /** Guards one-time [[Program]] global initialization. */
  private var globalsReady = false

  /** Evaluate each [[GlobalDecl]] once, in declaration order, binding its
    * returned value into [[globals]]. Done lazily on the first [[invoke]] so the
    * [[WasmHost]] connection is live before any embedding call in an
    * initializer (e.g. `store_init`) runs. */
  private def ensureGlobals(): Unit =
    if !globalsReady then
      globalsReady = true
      for GlobalDecl(name, init) <- program.globals do
        execInst(init, Map.empty, heap)._1 match
          case Signal.Ret(v) => globals(name) = v
          case Signal.Next =>
            throw InterpreterError(s"global $name: initializer must return a value")

  /** Run `fname` with `args` on the shared [[heap]], after initializing globals. */
  def invoke(fname: String, args: List[WjValue]): WjValue =
    ensureGlobals()
    callStack.clear()
    callFunc(fname, args, heap)

  /** Run `fname` with `args` in an existing `heap`.
    *
    * Use this when the arguments are [[WjValue.Record]] / [[WjValue.List]]
    * handles that were pre-allocated in `heap` before the call.
    */
  def invoke(fname: String, args: List[WjValue], heap: Heap): WjValue =
    callStack.clear()
    callFunc(fname, args, heap)

  // -- Function dispatch ------------------------------------------------------

  /** Resolve and run an IR-defined WJI function from the [[Program]].
    *
    * Pushes `fname` onto [[callStack]] before executing and pops it only on
    * a normal return. On [[InterpreterError]] the frame is left in place so
    * the caller can inspect the full call chain.
    */
  private def callFunc(fname: String, args: List[WjValue], heap: Heap): WjValue =
    callStack += fname
    val result = program.funcMap.get(fname) match
      case Some(func) =>
        if func.params.length != args.length then
          // allow trailing optional params to be omitted
          val required = func.params.count(!_.optional)
          if args.length < required || args.length > func.params.length then
            throw InterpreterError(
              s"$fname: expected ${func.params.length} args, got ${args.length}",
            )
        val locals = func.params.zip(args).map((p, a) => p.name.name -> a).toMap
        execInst(func.body, locals, heap)._1 match
          case Signal.Ret(v) => v
          case Signal.Next   => WjValue.Undef
      case None => throw InterpreterError(s"unknown IR function: $fname")
    callStack.remove(callStack.length - 1)
    result

  // -- Instructions -----------------------------------------------------------

  private def execInst(inst: Inst, locals: Locals, heap: Heap): (Signal, Locals) =
    inst match
      case IExpr(expr) =>
        evalExpr(expr, locals, heap)
        (Signal.Next, locals)

      case ILet(lhs, expr) =>
        (Signal.Next, locals + (lhs.name -> evalExpr(expr, locals, heap)))

      case IAssign(ref, expr) =>
        (Signal.Next, writeRef(ref, evalExpr(expr, locals, heap), locals, heap))

      case IDelete(ref) =>
        ref match
          case Name(n)   => (Signal.Next, locals - n)
          case Temp(i)   => (Signal.Next, locals - tempKey(i))
          case Global(n) => globals -= n; (Signal.Next, locals)
          case Field(base, keyExpr) =>
            readRef(base, locals, heap) match
              case WjValue.Record(id) =>
                heap.deleteField(id, asStr(evalExpr(keyExpr, locals, heap)))
                (Signal.Next, locals)
              case other =>
                throw InterpreterError(s"delete: cannot delete field of $other")

      case IPush(elemE, listE, front) =>
        val elem = evalExpr(elemE, locals, heap)
        heap.listPush(asListId(evalExpr(listE, locals, heap)), elem, front)
        (Signal.Next, locals)

      case IPop(lhs, listE, front) =>
        val v = heap.listPop(asListId(evalExpr(listE, locals, heap)), front)
        (Signal.Next, locals + (lhs.name -> v))

      case IReturn(expr) =>
        (Signal.Ret(evalExpr(expr, locals, heap)), locals)

      case IAssert(expr) =>
        if !asBool(evalExpr(expr, locals, heap)) then
          throw InterpreterError(s"assertion failed: $expr")
        (Signal.Next, locals)

      case INop() =>
        (Signal.Next, locals)

      case IPrint(expr) =>
        println(showValue(evalExpr(expr, locals, heap), heap))
        (Signal.Next, locals)

      case ISeq(insts) =>
        var cur = locals
        val it = insts.iterator
        while it.hasNext do
          execInst(it.next(), cur, heap) match
            case (Signal.Ret(v), _) => return (Signal.Ret(v), cur)
            case (Signal.Next, ls)  => cur = ls
        (Signal.Next, cur)

      case IIf(cond, thenI, elseI) =>
        if asBool(evalExpr(cond, locals, heap)) then execInst(thenI, locals, heap)
        else execInst(elseI, locals, heap)

      case IWhile(cond, body) =>
        var cur = locals
        while asBool(evalExpr(cond, cur, heap)) do
          execInst(body, cur, heap) match
            case (Signal.Ret(v), _) => return (Signal.Ret(v), cur)
            case (Signal.Next, ls)  => cur = ls
        (Signal.Next, cur)

      case ICall(lhs, fexpr, argEs) =>
        val fname = fexpr match
          case EClo(name)       => name
          case ERef(ref)        => asClo(readRef(ref, locals, heap))
          case other            => asClo(evalExpr(other, locals, heap))
        val args = argEs.map(evalExpr(_, locals, heap))
        (Signal.Next, locals + (lhs.name -> callFunc(fname, args, heap)))

      case ICallEmbed(lhs, fname, argEs) =>
        val args = argEs.map(evalExpr(_, locals, heap))
        (Signal.Next, locals + (lhs.name -> callEmbedding(fname, args, heap)))

  // -- Expressions ------------------------------------------------------------

  private def evalExpr(expr: Expr, locals: Locals, heap: Heap): WjValue =
    expr match
      case EMath(n)    => WjValue.Math(n)
      case ENum(n)     => WjValue.Num(n)
      case EStr(s)     => WjValue.Str(s)
      case EBool(b)    => WjValue.Bool(b)
      case EUndef      => WjValue.Undef
      case ENull       => WjValue.Null
      case EEnum(name) => WjValue.Enum(name)

      case ERef(ref) => readRef(ref, locals, heap)

      case EUnary(uop, e)       => evalUnary(uop, evalExpr(e, locals, heap))
      case EBinary(bop, l, r)   => evalBinary(bop, evalExpr(l, locals, heap), evalExpr(r, locals, heap))

      case EProj(e, idx) =>
        evalExpr(e, locals, heap) match
          case WjValue.Wasm(ALValue.TupV(vs)) =>
            if idx < 0 || idx >= vs.length then
              throw InterpreterError(s"proj: index $idx out of bounds for tuple of size ${vs.length}")
            WjValue.Wasm(vs(idx))
          case other =>
            throw InterpreterError(s"proj: expected a Wasm tuple, got $other")

      case EExists(ref) => WjValue.Bool(existsRef(ref, locals, heap))

      case ETypeOf(e)         => WjValue.Str(typeName(evalExpr(e, locals, heap)))
      case ETypeCheck(e, ty)  => WjValue.Bool(typeName(evalExpr(e, locals, heap)) == ty)

      case ERecord(_, fields) =>
        val fs = fields.map((k, e) => k -> evalExpr(e, locals, heap)).toMap
        WjValue.Record(heap.allocRecord(fs))

      case EList(elems) =>
        WjValue.List(heap.allocList(elems.map(evalExpr(_, locals, heap)).toVector))

      case ELen(expr) =>
        evalExpr(expr, locals, heap) match
          case WjValue.List(id) => WjValue.Math(heap.listSize(id))
          case WjValue.Str(s)   => WjValue.Math(s.length)
          case other            => throw InterpreterError(s"len: not a list or string: $other")

      case EClo(fname) => WjValue.Clo(fname)

      case EUnknown(raw) => throw InterpreterError(s"unknown spec construct: $raw")
      case EYet(msg)     => throw InterpreterError(s"not yet implemented: $msg")

  private def evalUnary(uop: UOp, v: WjValue): WjValue =
    uop match
      case UOp.Neg =>
        v match
          case WjValue.Math(n) => WjValue.Math(-n)
          case WjValue.Num(n)  => WjValue.Num(-n)
          case other           => throw InterpreterError(s"neg: not a number: $other")
      case UOp.Not  => WjValue.Bool(!asBool(v))
      case UOp.BNot =>
        v match
          case WjValue.Math(n) => WjValue.Math(BigDecimal(~n.toBigInt))
          case other           => throw InterpreterError(s"bnot: not an integer: $other")

  private def evalBinary(bop: BOp, l: WjValue, r: WjValue): WjValue =
    import BOp.*
    bop match
      case Add => arith(l, r)(_ + _)(_ + _)
      case Sub => arith(l, r)(_ - _)(_ - _)
      case Mul => arith(l, r)(_ * _)(_ * _)
      case Div => arith(l, r)(_ / _)(_ / _)
      case Mod => arith(l, r)(_ % _)(_ % _)
      case Pow => WjValue.Num(math.pow(asNum(l), asNum(r)))

      case Eq  => WjValue.Bool(l == r)
      case NEq => WjValue.Bool(l != r)
      case Lt  => WjValue.Bool(asNum(l) < asNum(r))
      case Le  => WjValue.Bool(asNum(l) <= asNum(r))
      case Gt  => WjValue.Bool(asNum(l) > asNum(r))
      case Ge  => WjValue.Bool(asNum(l) >= asNum(r))

      case And => WjValue.Bool(asBool(l) && asBool(r))
      case Or  => WjValue.Bool(asBool(l) || asBool(r))

      case BAnd => WjValue.Math(BigDecimal(asInt(l) & asInt(r)))
      case BOr  => WjValue.Math(BigDecimal(asInt(l) | asInt(r)))
      case BXOr => WjValue.Math(BigDecimal(asInt(l) ^ asInt(r)))

  /** Arithmetic that stays in [[WjValue.Math]] when both operands are Math,
    * otherwise falls back to [[WjValue.Num]] (double) arithmetic.
    */
  private def arith(l: WjValue, r: WjValue)(
    m: (BigDecimal, BigDecimal) => BigDecimal,
  )(d: (Double, Double) => Double): WjValue =
    (l, r) match
      case (WjValue.Math(a), WjValue.Math(b)) => WjValue.Math(m(a, b))
      case _                                  => WjValue.Num(d(asNum(l), asNum(r)))

  // -- References -------------------------------------------------------------

  private def readRef(ref: Ref, locals: Locals, heap: Heap): WjValue =
    ref match
      case Name(n)   => locals.getOrElse(n, throw InterpreterError(s"unbound variable: $n"))
      case Temp(i)   => locals.getOrElse(tempKey(i), throw InterpreterError(s"unbound temp: $i"))
      case Global(n) => globals.getOrElse(n, throw InterpreterError(s"unbound global: $n"))
      case Field(base, keyExpr) =>
        val key = evalExpr(keyExpr, locals, heap)
        readRef(base, locals, heap) match
          case WjValue.Record(id) =>
            heap.getField(id, asStr(key)).getOrElse(
              throw InterpreterError(s"no field ${asStr(key)} on record #$id"),
            )
          case WjValue.List(id) => heap.listGet(id, asInt(key))
          case other            => throw InterpreterError(s"cannot index into $other")

  private def writeRef(ref: Ref, v: WjValue, locals: Locals, heap: Heap): Locals =
    ref match
      case Name(n)   => locals + (n -> v)
      case Temp(i)   => locals + (tempKey(i) -> v)
      case Global(n) => globals(n) = v; locals
      case Field(base, keyExpr) =>
        val key = evalExpr(keyExpr, locals, heap)
        readRef(base, locals, heap) match
          case WjValue.Record(id) => heap.setField(id, asStr(key), v); locals
          case WjValue.List(id)   => heap.listSet(id, asInt(key), v); locals
          case other              => throw InterpreterError(s"cannot assign into $other")

  private def existsRef(ref: Ref, locals: Locals, heap: Heap): Boolean =
    ref match
      case Name(n)   => locals.contains(n)
      case Temp(i)   => locals.contains(tempKey(i))
      case Global(n) => globals.contains(n)
      case Field(base, keyExpr) =>
        readRef(base, locals, heap) match
          case WjValue.Record(id) => heap.getField(id, asStr(evalExpr(keyExpr, locals, heap))).isDefined
          case WjValue.List(id)   => asInt(evalExpr(keyExpr, locals, heap)) < heap.listSize(id)
          case _                  => false

  private def tempKey(i: Int): String = s"%temp$i"

  // -- Built-in (Wasm embedding) dispatch -------------------------------------

  /** Maps embedding-function names to [[WasmHost]] methods, converting
    * [[WjValue]] ↔ [[ALValue]] at the boundary. A [[WasmError]] becomes an
    * [[InterpreterError]].
    */
  private def callEmbedding(fname: String, args: List[WjValue], heap: Heap): WjValue =
    def al(i: Int): ALValue = toAL(args(i))
    def alList(i: Int): List[ALValue] = toALList(args(i), heap)

    fname match
      // -- Store ------------------------------------------------------------
      case "store_init" => one(host.storeInit())

      // -- Modules ----------------------------------------------------------
      case "module_decode"   => one(host.moduleDecode(al(0)))
      case "module_validate" => one(host.moduleValidate(al(0)))
      case "module_instantiate" =>
        one(host.moduleInstantiate(al(0), al(1), alList(2)))
      case "module_imports" => one(host.moduleImports(al(0)))
      case "module_exports" => one(host.moduleExports(al(0)))

      // -- Module instances -------------------------------------------------
      case "instance_export" => one(host.instanceExport(al(0), al(1)))

      // -- Functions --------------------------------------------------------
      case "func_alloc" =>
        one(host.funcAlloc(al(0), al(1), toHostFunc(args(2), heap)))
      case "func_type"   => one(host.funcType(al(0), al(1)))
      case "func_invoke" => one(host.funcInvoke(al(0), al(1), alList(2)))

      // -- Tables -----------------------------------------------------------
      case "table_alloc" => one(host.tableAlloc(al(0), al(1), al(2)))
      case "table_type"  => one(host.tableType(al(0), al(1)))
      case "table_read"  => one(host.tableRead(al(0), al(1), al(2)))
      case "table_write" => one(host.tableWrite(al(0), al(1), al(2), al(3)))
      case "table_size"  => one(host.tableSize(al(0), al(1)))
      case "table_grow"  => one(host.tableGrow(al(0), al(1), al(2), al(3)))

      // -- Memories ---------------------------------------------------------
      case "mem_alloc" => one(host.memAlloc(al(0), al(1)))
      case "mem_type"  => one(host.memType(al(0), al(1)))
      case "mem_size"  => one(host.memSize(al(0), al(1)))
      case "mem_grow"  => one(host.memGrow(al(0), al(1), al(2)))

      // -- Tags -------------------------------------------------------------
      case "tag_alloc" => one(host.tagAlloc(al(0), al(1)))
      case "tag_type"  => one(host.tagType(al(0), al(1)))

      // -- Exceptions -------------------------------------------------------
      case "exn_alloc" => one(host.exnAlloc(al(0), al(1), alList(2)))
      case "exn_tag"   => one(host.exnTag(al(0), al(1)))
      case "exn_read"  => one(host.exnRead(al(0), al(1)))

      // -- Globals ----------------------------------------------------------
      case "global_alloc" => one(host.globalAlloc(al(0), al(1), al(2)))
      case "global_type"  => one(host.globalType(al(0), al(1)))
      case "global_read"  => one(host.globalRead(al(0), al(1)))
      case "global_write" => one(host.globalWrite(al(0), al(1), al(2)))

      // -- Values -----------------------------------------------------------
      case "ref_type"    => one(host.refType(al(0), al(1)))
      case "val_default" => one(host.valDefault(al(0)))

      // -- Matching ---------------------------------------------------------
      case "match_valtype"    => one(host.matchValType(al(0), al(1)))
      case "match_externtype" => one(host.matchExternType(al(0), al(1)))

      case other => throw InterpreterError(s"unknown embedding function: $other")

  // -- WasmError result wrappers ----------------------------------------------

  private def either[A](e: Either[WasmError, A])(wrap: A => WjValue): WjValue =
    e match
      case Right(a)  => wrap(a)
      case Left(err) => throw InterpreterError(s"WasmHost error: $err")

  private def one(e: Either[WasmError, ALValue]): WjValue =
    either(e)(WjValue.Wasm.apply)

  // -- Value <-> ALValue boundary conversions ---------------------------------

  /** Build a [[HostFunction]] from a [[WjValue.Clo]] so SpecTec can call back
    * into the interpreter during Wasm execution.
    */
  private def toHostFunc(v: WjValue, heap: Heap): HostFunction =
    val fname = asClo(v)
    (vals: List[ALValue]) =>
      callFunc(fname, vals.map(WjValue.Wasm.apply), heap) match
        case WjValue.Wasm(ALValue.ListV(rs)) => Right(rs)
        case WjValue.Wasm(av)                => Right(List(av))
        // TODO: generalize; the host function's `EList` return is a heap list.
        case WjValue.List(id)                => Right(heap.listAll(id).map(toAL).toList)
        case other => Left(WasmError.ProtocolError(s"host function returned non-Wasm value: $other"))

  private def toAL(v: WjValue): ALValue =
    v match
      case WjValue.Wasm(av) => av
      case WjValue.Str(s)   => ALValue.TextV(s)
      case WjValue.Bool(b)  => ALValue.BoolV(b)
      case WjValue.Num(n)   => ALValue.NumV(ALNum.Real(n))
      case WjValue.Math(n)  => ALValue.NumV(ALNum.Int(n.toBigInt))
      case other            => throw InterpreterError(s"cannot pass $other across the WasmHost boundary")

  private def toALList(v: WjValue, heap: Heap): List[ALValue] =
    v match
      case WjValue.Wasm(ALValue.ListV(vs)) => vs
      case WjValue.List(id)                => heap.listAll(id).map(toAL).toList
      case other                           => throw InterpreterError(s"expected a list, got $other")

  // -- Coercions --------------------------------------------------------------

  private def asBool(v: WjValue): Boolean = v match
    case WjValue.Bool(b) => b
    case other           => throw InterpreterError(s"expected a boolean, got $other")

  private def asStr(v: WjValue): String = v match
    case WjValue.Str(s) => s
    case other          => throw InterpreterError(s"expected a string, got $other")

  private def asNum(v: WjValue): Double = v match
    case WjValue.Num(n)  => n
    case WjValue.Math(n) => n.toDouble
    case other           => throw InterpreterError(s"expected a number, got $other")

  private def asInt(v: WjValue): Int = v match
    case WjValue.Math(n) => n.toInt
    case WjValue.Num(n)  => n.toInt
    case other           => throw InterpreterError(s"expected an integer, got $other")

  private def asClo(v: WjValue): String = v match
    case WjValue.Clo(fname) => fname
    case other              => throw InterpreterError(s"expected a closure, got $other")

  private def asListId(v: WjValue): Int = v match
    case WjValue.List(id) => id
    case other            => throw InterpreterError(s"expected a list handle, got $other")

  // -- Misc -------------------------------------------------------------------
  //
  // TODO: more intuitive showValue & showAL

  /** Human-readable rendering of a [[WjValue]] for [[IPrint]], chosen so the
    * runtime type stays visible: the two numeric kinds are tagged
    * ([[WjValue.Num]] is an IEEE double, [[WjValue.Math]] an exact rational),
    * strings are quoted, enums keep their `~…~` syntax, and heap-allocated
    * records/lists are shown as `<record/list #id …>`. [[WjValue.Wasm]] is
    * rendered by structurally showing its underlying [[ALValue]].
    */
  private def showValue(v: WjValue, heap: Heap): String = v match
    case WjValue.Wasm(av)   => showAL(av)
    case WjValue.Undef      => "undefined"
    case WjValue.Null       => "null"
    case WjValue.Bool(b)    => b.toString
    case WjValue.Num(n)     => s"num:$n"
    case WjValue.Math(n)    => s"math:$n"
    case WjValue.Str(s)     => s""""$s""""
    case WjValue.Enum(n)    => s"~$n~"
    case WjValue.Record(id) => s"<record #$id>"
    case WjValue.List(id)   => s"<list #$id ${heap.listAll(id).map(showValue(_, heap)).mkString("[", ", ", "]")}>"
    case WjValue.Clo(fname) => s"<closure $fname>"

  /** Structural rendering of an [[ALValue]] (a SpecTec-owned value), preserving
    * its type: the numeric kind is tagged (`nat:`/`int:`/`rat:`/`real:`), text
    * is quoted, and each compound shape uses distinct delimiters — `[…]` list,
    * `(…)` tuple, `{…}` record, `id(…)` case, `@id` function name, `?…`/`ε`
    * option.
    */
  private def showAL(av: ALValue): String = av match
    case ALValue.NumV(n)         => showALNum(n)
    case ALValue.BoolV(b)        => b.toString
    case ALValue.TextV(s)        => s""""$s""""
    case ALValue.ListV(vs)       => vs.map(showAL).mkString("[", " ", "]")
    case ALValue.StrV(fs)        => fs.map((k, v) => s"$k=${showAL(v)}").mkString("{", ", ", "}")
    case ALValue.CaseV(id, Nil)  => id
    case ALValue.CaseV(id, args) => s"$id(${args.map(showAL).mkString(" ")})"
    case ALValue.OptV(None)      => "ε"
    case ALValue.OptV(Some(v))   => s"?${showAL(v)}"
    case ALValue.TupV(vs)        => vs.map(showAL).mkString("(", ", ", ")")
    case ALValue.FnameV(id)      => s"@$id"

  private def showALNum(n: ALNum): String = n match
    case ALNum.Nat(v)    => s"nat:$v"
    case ALNum.Int(v)    => s"int:$v"
    case ALNum.Rat(p, q) => s"rat:$p/$q"
    case ALNum.Real(v)   => s"real:$v"

  /** A coarse runtime type tag used by [[ETypeOf]] / [[ETypeCheck]]. */
  private def typeName(v: WjValue): String = v match
    case WjValue.Wasm(_)   => "wasm"
    case WjValue.Undef     => "undefined"
    case WjValue.Null      => "null"
    case WjValue.Bool(_)   => "boolean"
    case WjValue.Num(_)    => "number"
    case WjValue.Math(_)   => "math"
    case WjValue.Str(_)    => "string"
    case WjValue.Enum(n)   => s"~$n~"
    case WjValue.Record(_) => "record"
    case WjValue.List(_)   => "list"
    case WjValue.Clo(_)    => "closure"
