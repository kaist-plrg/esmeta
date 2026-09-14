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

/** [[wasmF32Const]]/[[wasmF64Const]]'s inverse — recovers the mathematical
  * value a `[=f32.const=]`/`[=f64.const=] |f32|`/`|f64|` form match's own
  * payload denotes, for `ToJSValue`'s `[=𝔽=](|f32|/|f64| interpreted as a
  * [=mathematical value=])` step (`esmeta.wji.compiler.Compiler`'s
  * `AsMath(WasmFloatPayload(width, e))` case, `docs/hardcodes.md` #19's
  * counterpart entry covers the wasm-to-JS direction).
  *
  * Reconstructs the raw sign/exponent/mantissa bit pattern first (mirroring
  * OCaml `construct.ml`'s own `al_to_floatN`/`al_to_fmagN` — the reverse of
  * `wasmFloatConst` above, field for field) and reinterprets *that* as a native
  * `Float`/`Double` via `intBitsToFloat`/`longBitsToDouble`, rather than
  * computing `(1 + m·2⁻ᴹ)·2^exp`/`m·2⁻ᴹ·2^exp_min` by hand — letting the JDK's
  * own IEEE-754 bit layout do the reassembly avoids re-deriving (and risking a
  * mismatched) rounding/precision behavior for the mathematical formula
  * version.
  *
  * Per `ToJSValue`'s own step order (index.bs:1386-1393), `+∞`/`-∞`/`NaN` are
  * each handled by a dedicated condition *before* this conversion ever runs —
  * so a finite `NORM`/`SUBNORM` payload is the only shape genuinely expected
  * here. `INF`/`NAN` are still reconstructed correctly (both `al_to_fmagN`
  * cases carried through), but `Math` (backed by `BigDecimal`) has no
  * representation for a non-finite value at all — surfaced as a clear
  * [[WasmHostFailure]] rather than an opaque `BigDecimal` construction crash.
  */
def wasmF32ToMath(v: ALValue): Math =
  toFiniteMath(
    v,
    java.lang.Float
      .intBitsToFloat(wasmFloatBits(floatLayout32, v).toInt)
      .toDouble,
  )
def wasmF64ToMath(v: ALValue): Math =
  toFiniteMath(
    v,
    java.lang.Double.longBitsToDouble(wasmFloatBits(floatLayout64, v).toLong),
  )

private def toFiniteMath(v: ALValue, d: Double): Math =
  if d.isNaN || d.isInfinite then
    throw WasmHostFailure(
      s"wasmFloatToMath: $v is not finite (got $d) -- ToJSValue's own " +
      "+∞/-∞/NaN guards should have caught this before reaching " +
      "'interpreted as a mathematical value' at all",
    )
  Math(d)

private def wasmFloatBits(layout: FloatLayout, v: ALValue): scala.math.BigInt =
  import scala.math.BigInt
  def asNat(av: ALValue): BigInt = av match
    case ALValue.NumV(ALNum.Nat(n)) => n
    case other =>
      throw WasmHostFailure(s"wasmFloatBits: expected a Nat, got $other")
  def asInt(av: ALValue): BigInt = av match
    case ALValue.NumV(ALNum.Nat(n)) => n
    case ALValue.NumV(ALNum.Int(n)) => n
    case other =>
      throw WasmHostFailure(s"wasmFloatBits: expected an Int, got $other")
  val maskSign = BigInt(1) << (layout.width - 1)
  val maskMant = (BigInt(1) << layout.mantissa) - 1
  val maskExp = (maskSign - 1) - maskMant
  val bias = (BigInt(1) << (layout.exponent - 1)) - 1
  def magBits(mag: ALValue): BigInt = mag match
    case ALValue.CaseV("NORM", List(m, n)) =>
      ((asInt(n) + bias) << layout.mantissa) + asNat(m)
    case ALValue.CaseV("SUBNORM", List(m)) => asNat(m)
    case ALValue.CaseV("INF", Nil)         => maskExp
    case ALValue.CaseV("NAN", List(m))     => maskExp + asNat(m)
    case other =>
      throw WasmHostFailure(s"wasmFloatBits: invalid floatN magnitude $other")
  v match
    case ALValue.CaseV("POS", List(mag)) => magBits(mag)
    case ALValue.CaseV("NEG", List(mag)) => maskSign + magBits(mag)
    case other =>
      throw WasmHostFailure(s"wasmFloatBits: invalid floatN value $other")
