package esmeta.ir

import esmeta.ir.util.Parser

// IR operators
sealed trait Op extends IRElem

// unary operators
enum UOp extends Op:
  // mathematic values
  case Abs, Floor
  // numeric values
  case Neg
  // boolean
  case Not
  // bitwise
  case BNot
object UOp extends Parser.From(Parser.uop)

// binary operators
enum BOp extends Op:
  // equality (e.g. is, are)
  case Eq
  // numeric values
  case Add, Sub, Mul, Pow, Div, Mod, Lt, Equal
  // bitwise
  case BAnd, BOr, BXOr
  // shift
  case LShift, RShift
  // boolean
  case And, Or, Xor
object BOp extends Parser.From(Parser.bop)

// variadic operators
enum VOp extends Op:
  // mathematic values
  case Min, Max
  // string
  case Concat
object VOp extends Parser.From(Parser.vop)

// mathematical operators
enum MOp extends Op:
  case Expm1, Log10, Log2, Cos, Cbrt, Exp, Cosh, Sinh, Tanh, Acos, Acosh
  case Asinh, Atanh, Asin, Atan2, Atan, Log1p, Log, Sin, Sqrt, Tan
object MOp extends Parser.From(Parser.mop)

// conversion operators
enum COp extends Op:
  case ToApproxNumber, ToNumber, ToBigInt, ToMath, ToCodeUnit, ToCodePoint
  case ToStr(radix: Option[Expr], upper: Boolean = false)
  // reinterprets a wasm f32/f64 `CaseV("POS"/"NEG", [...])` payload
  // (opaque everywhere else in ESMeta) as a mathematical value -- the width
  // (32 vs 64) has to be baked into the operator itself, since by the time
  // this conversion runs there's no longer any sibling tag to read it from
  // (see esmeta.wji.lang.Expr.WasmFloatPayload's own doc for where the width
  // still is, and esmeta.state.util.wasmF32Const/wasmF64Const for the
  // opposite-direction construction this undoes). WJI-only; not produced by
  // mainline ECMA-262 compilation.
  case ToMathF32, ToMathF64
object COp extends Parser.From(Parser.cop)
