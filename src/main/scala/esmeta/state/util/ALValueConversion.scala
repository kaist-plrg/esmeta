package esmeta.state.util

import esmeta.error.{NoList, NoWasmValue, WasmHostFailure}
import esmeta.state.*

/** Converts a [[Value]] crossing the WasmHost boundary (an `ICallEmbed`
  * argument or a [[esmeta.wji.bridge.host.HostFunction]] result) to an
  * [[ALValue]]. A heap-allocated ES list (e.g. from an Infra-spec list literal
  * like `« |payload| »`, compiled to `EList`/`Addr`) is recursively converted
  * into an [[ALValue.ListV]] — this is the one case that isn't already a
  * [[Wasm]] value, needed by embedding functions whose `val*`/`externval*`
  * argument wasn't itself built by a prior embedding call.
  */
def toAL(st: State, v: Value): ALValue = v match
  case Wasm(av)                   => av
  case Str(s)                     => ALValue.TextV(s)
  case Bool(b)                    => ALValue.BoolV(b)
  case Number(n)                  => ALValue.NumV(ALNum.Real(n))
  case Math(n) if n.toBigInt >= 0 => ALValue.NumV(ALNum.Nat(n.toBigInt))
  case Math(n)                    => ALValue.NumV(ALNum.Int(n.toBigInt))
  case addr: Addr =>
    st(addr) match
      case ListObj(vs) => ALValue.ListV(vs.map(toAL(st, _)).toList)
      case other       => throw NoList(other)
  case other => throw NoWasmValue(other)

/** [[toAL]]'s inverse for a single `` `Nat ``-/`` `Int ``-tagged AL number, the
  * shape a single wasm byte always arrives as crossing the WasmHost boundary.
  */
def fromALNum(av: ALValue): Value = av match
  case ALValue.NumV(ALNum.Nat(n)) => Math(n)
  case ALValue.NumV(ALNum.Int(n)) => Math(n)
  case other =>
    throw WasmHostFailure(s"fromALNum: expected a byte, got $other")

/** *(hardcoding)* `f32.const`/`f64.const`'s own value, decomposed into the AL
  * `floatN` shape (`spectec/spectec/src/xl/num.ml`-adjacent, the sign/
  * exponent/mantissa `CaseV` tree `Backend_interpreter.Construct.al_to_floatN`
  * expects on the wire) from a plain Scala `Double` — see
  * `esmeta.interpreter.Interpreter`'s `ECase` case, the one place a `Wasm(
  * ALValue.CaseV("CONST", ...))` gets built, for why this lives at that single
  * boundary rather than spread across every algorithm that constructs an f32/
  * f64 value: on the ESMeta side, a wasm f32/f64 value is just an ordinary
  * `Number`(`Double`) the whole time (arithmetic, comparisons, `[math]`, ...
  * all work on it the same as any other Number) — only once it's about to cross
  * to SpecTec as a `CONST` payload does it need this shape at all, so doing the
  * conversion right there (rather than eagerly, at each `Let X be ... rounded
  * ...`/`Return [=f32.const=] X.` site) keeps every other WJI pass untouched.
  * `docs/hardcodes.md` #19.
  *
  * `layout` mirrors OCaml `construct.ml`'s `layout32`/`layout64` record
  * (width/exponent/mantissa bit counts); `toBits` narrows `d` to the target
  * precision and reinterprets its IEEE-754 bit pattern as an unsigned integer,
  * exactly like `F32.to_bits`/`F64.to_bits` on the OCaml side — `.toFloat`'s
  * own narrowing conversion (JLS 5.1.3) already performs "rounded to the
  * nearest representable value using IEEE 754-2019 round to nearest, ties to
  * even mode" for the `f32` case, which is why `ToWebAssemblyValue`'s own "Let
  * |f32| be |number| rounded ..." step (`esmeta.wji.lang.parser.ExprParser`'s
  * `RoundedToNearestRepresentable`) doesn't need a rounding step of its own —
  * this function does the one real rounding that matters, at the one point a
  * rounded value is actually observed (its wire encoding).
  */
private case class FloatLayout(width: Int, exponent: Int, mantissa: Int)
private val floatLayout32 = FloatLayout(32, 8, 23)
private val floatLayout64 = FloatLayout(64, 11, 52)

def wasmF32Const(d: Double): ALValue =
  wasmFloatConst(floatLayout32, java.lang.Float.floatToRawIntBits(d.toFloat))
def wasmF64Const(d: Double): ALValue =
  wasmFloatConst(floatLayout64, java.lang.Double.doubleToRawLongBits(d))

private def wasmFloatConst(layout: FloatLayout, bits: Long): ALValue =
  import scala.math.BigInt
  val unsigned = BigInt(bits) & ((BigInt(1) << layout.width) - 1)
  val maskSign = BigInt(1) << (layout.width - 1)
  val maskMant = (BigInt(1) << layout.mantissa) - 1
  val maskExp = (maskSign - 1) - maskMant
  val bias = (BigInt(1) << (layout.exponent - 1)) - 1
  val isNeg = (unsigned & maskSign) != 0
  val n = unsigned & maskExp
  val m = unsigned & maskMant
  val mag =
    if n == 0 then ALValue.CaseV("SUBNORM", List(ALValue.NumV(ALNum.Nat(m))))
    else if n != maskExp then
      val exp = (n >> layout.mantissa) - bias
      ALValue.CaseV(
        "NORM",
        List(ALValue.NumV(ALNum.Nat(m)), ALValue.NumV(ALNum.Int(exp))),
      )
    else if m == 0 then ALValue.CaseV("INF", Nil)
    else ALValue.CaseV("NAN", List(ALValue.NumV(ALNum.Nat(m))))
  ALValue.CaseV(if isNeg then "NEG" else "POS", List(mag))
