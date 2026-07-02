package esmeta.wji.lang

sealed trait Expr
object Expr:
  case class Var(name: String) extends Expr
  case object This extends Expr
  // value is a decimal or hex (0x...) string literal
  case class Num(value: String) extends Expr
  case class Bool(value: Boolean) extends Expr
  case class Str(value: String) extends Expr
  /** A reference to an ECMA-262/Wasm-specific term (a `[=...=]`/`**...**`/
    * `<emu-const>` token whose meaning is fixed by the spec's own glossary
    * rather than by an extracted `<div algorithm>`), e.g. `null`, `undefined`,
    * `current Realm`.
    */
  case class SpecTerm(name: String) extends Expr
  case class Slot(base: Expr, slot: String) extends Expr
  case class Index(base: Expr, key: Expr) extends Expr
  case class AlgoCall(link: String, args: List[Expr]) extends Expr
  case class JSCall(name: String, args: List[Expr]) extends Expr
  case class Abrupt(check: String, expr: Expr) extends Expr
  case class New(iface: String) extends Expr
  case class UnknownNew(raw: String) extends Expr

  /** `« E1, E2, ... »` — Infra-spec List literal. */
  case class List_(elements: List[Expr]) extends Expr

  /** `«[ K1 → V1, K2 → V2, ... ]»` — Infra-spec ordered map literal. */
  case class Map_(entries: List[(Expr, Expr)]) extends Expr

  /** The length/size of a string or list. */
  case class Length(expr: Expr) extends Expr

  /** `lhs op rhs` — arithmetic binary operation (+, -, *, modulo, &div;,
    * &minus;).
    */
  case class BinOp(lhs: Expr, op: String, rhs: Expr) extends Expr

  /** `base<sup>exp</sup>` — exponentiation. */
  case class Pow(base: Expr, exp: Expr) extends Expr

  /** Unary negation `-expr`. */
  case class Neg(expr: Expr) extends Expr

  /** `|x| interpreted as a [=mathematical value=]` — cast to math value. */
  case class AsMath(expr: Expr) extends Expr

  /** `(E1, E2, ...)` — parenthesised tuple, used in destructuring Let LHS. */
  case class Tuple(elems: List[Expr]) extends Expr

  /** Spec prose that didn't match any recognised expression pattern. */
  case class Unknown(raw: String) extends Expr
