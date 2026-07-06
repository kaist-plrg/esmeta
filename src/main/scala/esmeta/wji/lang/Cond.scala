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

  /** EXPR has a [[SLOT]] internal slot / EXPR does not have a [[SLOT]]
    * internal slot.
    */
  case class HasSlot(expr: Expr, slot: String, negated: Boolean = false)
    extends Cond

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

  case class Unknown(text: String) extends Cond
