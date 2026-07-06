package esmeta.wji.lang

sealed trait Cond
object Cond:
  /** EXPR is EXPR / EXPR is not EXPR, and EXPR equals EXPR */
  case class Eq(lhs: Expr, rhs: Expr, negated: Boolean = false) extends Cond

  /** EXPR < EXPR etc. — op is "<", ">", "<=", ">=" */
  case class Compare(lhs: Expr, op: String, rhs: Expr) extends Cond

  /** EXPR [=map/exists=] / EXPR [=map/doesn't exist=] */
  case class MapExists(expr: Expr, negated: Boolean = false) extends Cond

  /** EXPR [=implements=] {{Iface}} / EXPR does not [=implement=] {{Iface}} */
  case class Implements(expr: Expr, iface: String, negated: Boolean = false)
    extends Cond

  /** EXPR is [not] of the form FORM [where COND] */
  case class IsOfForm(
    expr: Expr,
    form: Expr,
    cond: Option[Cond] = None,
    negated: Boolean = false,
  ) extends Cond

  /** EXPR [=matches/TYPE=] EXPR / EXPR does not [=matches/TYPE=] EXPR */
  case class Matches(
    lhs: Expr,
    matchType: String,
    rhs: Expr,
    negated: Boolean = false,
  ) extends Cond

  /** EXPR is [not] missing */
  case class IsMissing(expr: Expr, negated: Boolean = false) extends Cond

  /** EXPR [=is [not] a[n] TYPE=] — spec type-check predicate */
  case class IsType(expr: Expr, typeName: String, negated: Boolean = false)
    extends Cond

  case class And(left: Cond, right: Cond) extends Cond
  case class Or(left: Cond, right: Cond) extends Cond

  /** An expression that stands in for a full condition by abbreviating the
    * operator/LHS from context, e.g. the `[=−∞=]` in `|f32| is [=+∞=] or
    * [=−∞=]`.
    */
  case class Abbreviated(expr: Expr) extends Cond

  /** `This step is not reached.` */
  case object Unreachable extends Cond

  /** `If this [operation] throws an exception, ...` / `If this [operation]
    * throws a {{TypeError}}, ...` — checks whether the immediately preceding
    * step completed abruptly via throw, optionally narrowed to a specific
    * exception type (`kind`, e.g. `"TypeError"`; `None` for the untyped "an
    * exception" phrasing). The thrown value is referenced in the body as
    * `|exception|` (see `ExprParser`'s `"the exception"` alias); the
    * accompanying `catch it` phrase carries no separate meaning of its own.
    */
  case class Throws(kind: Option[String] = None) extends Cond

  /** Whether `base` has a field named `field`. A desugar-only utility (not
    * produced by [[CondParser]] from spec prose) used by `ExpandThrowsPass` to
    * tell a completion record (which has `.Type`/ `.Value`) apart from a bare
    * value it might otherwise be confused with; distinct from [[MapExists]],
    * which is tied to the `[=map/exists=]` spec vocabulary specifically.
    */
  case class HasField(base: Expr, field: String) extends Cond

  case class Unknown(text: String) extends Cond
