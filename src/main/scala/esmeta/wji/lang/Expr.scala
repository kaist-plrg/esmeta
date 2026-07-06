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

  /** A field read on a record/structure: base's field named `name`. Covers both
    * ECMA-262's `.[[name]]` internal-slot syntax and a WebAssembly-spec field
    * written as `base.[=name=]`, where the `[=...=]` is a plain documentation
    * link rather than a call.
    */
  case class Field(base: Expr, name: String) extends Expr
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

  /** "a [=link=] which <desc>" — a value described by a relative clause rather
    * than constructed directly (e.g. "a [=Data Block=] which is [=identified
    * with=] the underlying memory of |memaddr|"). Not yet evaluable; kept as a
    * distinct node (rather than falling into [[Unknown]]) so it's never
    * mistaken for an [[AlgoCall]].
    */
  case class Described(link: String, desc: String) extends Expr

  /** "(a|an|the) <desc> such that <cond>" — a definite/indefinite/superlative
    * description satisfying a predicate: "there is a value described by desc,
    * satisfying cond" (e.g. "a [=host address=] |hostaddr| exists such that
    * ...", "the unsigned integer such that |i64| is [=signed_64=](|u64|)", "the
    * smallest address such that ..."). `desc` is kept as raw text since it may
    * or may not contain a `[=link=]`/`|var|` — the phrasing varies across the
    * spec. Not yet evaluable; kept as a distinct node for the same reason as
    * [[Described]].
    */
  case class SuchThat(desc: String, cond: String) extends Expr

  /** Spec prose that didn't match any recognised expression pattern. */
  case class Unknown(raw: String) extends Expr
