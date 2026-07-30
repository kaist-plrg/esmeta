package esmeta.state

import esmeta.cfg.{CFG, Func}
import esmeta.error.*
import esmeta.es.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.state.util.*
import esmeta.ty.*
import esmeta.util.DoubleEquals
import java.math.MathContext.UNLIMITED
import scala.collection.mutable.{Map => MMap}

/** IR values */
sealed trait Value extends StateElem {

  /** check if the value is an expected type */
  def asStr: String = this match
    case Str(s) => s
    case _      => throw NoString(this)
  def asBool: Boolean = this match
    case Bool(b) => b
    case _       => throw NoBoolean(this)
  def asInt: Int = this match
    case Math(n) if n.isValidInt => n.toInt
    case _                       => throw NoInteger(this)
  def asAst: Ast = this match
    case AstValue(ast) => ast
    case v             => throw NoAst(this)
  def asMath: BigDecimal = this match
    case Math(n) => n
    case v       => throw NoMath(this)
  def asCallable: Callable = this match
    case func: Callable => func
    case v              => throw NoCallable(v)
  def asGrammarSymbol: GrammarSymbol = this match
    case g: GrammarSymbol => g
    case v                => throw NoGrammarSymbol(v)
  def asAddr: Addr = this match
    case addr: Addr => addr
    case v          => throw NoAddr(this)
  def asList(st: State): ListObj = this match
    case addr: Addr =>
      st(addr) match
        case l: ListObj => l
        case obj        => throw NoList(obj)
    case _ => throw NoAddr(this)
  def asRecord(st: State): RecordObj = this match
    case addr: Addr =>
      st(addr) match
        case r: RecordObj => r
        case obj          => throw NoRecord(obj)
    case _ => throw NoAddr(this)
}

/** addresses */
sealed trait Addr extends Value
case class NamedAddr(name: String) extends Addr
case class DynamicAddr(long: Long) extends Addr

/** ordering of addresses */
given Ordering[Addr] = Ordering.by(_ match
  case NamedAddr(name)   => (-1L, name)
  case DynamicAddr(long) => (long, ""),
)

/** function values */
sealed trait Callable extends Value {
  def func: Func
  def captured: Map[Name, Value]
}

/** closures */
case class Clo(func: Func, captured: Map[Name, Value]) extends Callable

/** continuations */
case class Cont(
  func: Func,
  captured: Map[Name, Value],
  callStack: List[CallContext],
) extends Callable

/** abstract syntax tree (AST) values */
case class AstValue(ast: Ast) extends Value

/** grammar symbols */
case class GrammarSymbol(name: String, params: List[Boolean]) extends Value

/** mathematical values */
case class Math(decimal: BigDecimal) extends Value
object Math {
  val zero: Math = Math(0)
  val one: Math = Math(1)
  inline def apply(n: Int): Math = Math(BigDecimal(n, UNLIMITED))
  inline def apply(n: Long): Math = Math(BigDecimal(n, UNLIMITED))
  inline def apply(n: Double): Math = Math(BigDecimal(n, UNLIMITED))
  inline def apply(n: scala.math.BigInt): Math = Math(BigDecimal(n, UNLIMITED))
  inline def apply(s: String): Math = Math(BigDecimal(s, UNLIMITED))
  inline def from(s: String, b: Int): Math = apply(scala.math.BigInt(s, b))
  inline def fromBinary(s: String): Math = from(s, 2)
  inline def fromOctal(s: String): Math = from(s, 8)
  inline def fromHex(s: String): Math = from(s, 16)

  extension (m: Math) {
    def +(n: Math): Math = Math(m.decimal + n.decimal)
    def -(n: Math): Math = Math(m.decimal - n.decimal)
    def *(n: Math): Math = Math(m.decimal * n.decimal)
    def /(n: Math): Math = Math(m.decimal / n.decimal)
    def <(n: Math): Boolean = m.decimal < n.decimal
    def >(n: Math): Boolean = m.decimal > n.decimal
    def pow(n: Math): Math = Math(m.decimal.pow(n.toInt))
    def unary_- : Math = Math(-m.decimal)
    def toInt: Int = m.decimal.toInt
    def toLong: Long = m.decimal.toLong
    def toDouble: Double = m.decimal.toDouble
    def toBigInt: BigInt = BigInt(m.decimal.toBigInt)
    def toBigDecimal: BigDecimal = m.decimal
  }

  given Ordering[Math] = Ordering.by(_.decimal)
}

/** infinity values */
case class Infinity(pos: Boolean) extends Value

/** enums */
case class Enum(name: String) extends Value

/** code units */
case class CodeUnit(c: Char) extends Value

/** Numeric AL values, mirroring `xl/num.ml`'s `num` variant
  * (`spectec/spectec/src/xl/num.ml`). See `state/util/ALValueJson.scala` for
  * the JSON wire encoding.
  */
enum ALNum:
  case Nat(value: scala.math.BigInt)
  case Int(value: scala.math.BigInt)
  case Rat(num: scala.math.BigInt, den: scala.math.BigInt)
  case Real(value: Double)

/** AL (Algorithmic Language) runtime values, mirroring the `value` type of
  * SpecTec's AL AST (`spectec/spectec/src/al/ast.ml`):
  *
  * {{{
  * and value =
  *   | NumV of Num.num                    (* number *)
  *   | BoolV of bool                      (* boolean *)
  *   | TextV of string                    (* string *)
  *   | ListV of value growable_array      (* list of values *)
  *   | StrV of (id, value) record         (* key-value mapping *)
  *   | CaseV of id * value list           (* constructor *)
  *   | OptV of value option               (* optional value *)
  *   | TupV of value list                 (* tuple of values *)
  *   | FnameV of id                       (* name of the first order function *)
  * }}}
  *
  * This is the common value representation used in the params/results of the
  * Wasm Embedding interface (`esmeta.wji.bridge.host.WasmHost`): stores,
  * modules, module instances, function/extern addresses, `val*`, types, etc.
  * are all encoded as [[ALValue]]. See `state/util/ALValueJson.scala` for the
  * JSON wire encoding.
  */
enum ALValue:
  case NumV(num: ALNum)
  case BoolV(value: Boolean)
  case TextV(value: String)
  case ListV(values: List[ALValue])
  case StrV(fields: List[(String, ALValue)])
  case CaseV(id: String, args: List[ALValue])
  case OptV(value: Option[ALValue])
  case TupV(values: List[ALValue])
  case FnameV(id: String)

/** a value owned by an external Wasm embedding (e.g. SpecTec), passed opaquely
  * across the `esmeta.wji.bridge.host.WasmHost` boundary without being
  * interpreted on the ES side.
  *
  * Two exceptions, both unwrapped by the smart constructor below into their
  * direct ECMAScript-value equivalent rather than staying `Wasm`-wrapped:
  *
  *   - [[ALValue.TextV]]: a Wasm `name` (Wasm Core Spec 5.2.4, a UTF-8 byte
  *     sequence) is always just an ECMAScript string once it crosses back out
  *     of a Wasm-side structure (e.g. `module_imports`'s `(name, name,
  *     externtype)` triples end up as property keys in `[$Get$] (importObject,
  *     moduleName)`) — never passed back across the WasmHost boundary as a bare
  *     value itself.
  *   - [[ALValue.BoolV]]: compiled metalang conditions compare an embedding
  *     call's boolean result directly against an ECMAScript `Bool` literal
  *     (e.g. `match_valtype(...)` compiles to `(= _callN true)`) — left
  *     `Wasm`-wrapped, that comparison silently always fails, since
  *     `esmeta.interpreter.Interpreter.eval(bop, l, r)`'s generic `Eq` case is
  *     plain Scala `==`, and `Wasm(ALValue.BoolV(b))` is never `==` to
  *     `Bool(b)` even when `b` matches (different case classes). Unlike
  *     [[ALValue.NumV]] (which must stay opaque — ES has more than one numeric
  *     representation, so unwrapping could silently pick the wrong one) a Wasm
  *     boolean has exactly one unambiguous ECMAScript equivalent, same as a
  *     Wasm string.
  *
  * Both live here, in the one place every `Wasm(...)` construction goes through
  * (positional indexing into a `Wasm(ALValue.TupV(...))`/
  * `CaseV(...)`/`ListV(...)` via `State.apply`, a raw embedding-call result,
  * ...), rather than at each of those call sites individually, so a
  * `Wasm(ALValue.TextV(_))`/`Wasm(ALValue.BoolV(_))` can't accidentally exist
  * anywhere.
  */
case class Wasm private (v: ALValue) extends Value
object Wasm:
  def apply(v: ALValue): Value = v match
    case ALValue.TextV(s) => Str(s)
    case ALValue.BoolV(b) => Bool(b)
    case other            => new Wasm(other)

/** simple values
  *
  * Simple values are ECMAScript values except objects and symbols. ECMAScript
  * objects and symbols need to be stored in a heap.
  */
sealed trait SimpleValue extends Value

/** numeric values */
sealed trait Numeric extends SimpleValue:
  def toMath: Math = this match
    case Number(double) => Math(double)
    case BigInt(bigInt) => Math(bigInt)
case class Number(double: Double) extends Numeric with DoubleEquals {
  def isNaN: Boolean = double.isNaN
}
case class BigInt(bigInt: scala.math.BigInt) extends Numeric

/** non-numeric simple values */
case class Str(str: String) extends SimpleValue
case class Bool(bool: Boolean) extends SimpleValue
case object Undef extends SimpleValue
case object Null extends SimpleValue
