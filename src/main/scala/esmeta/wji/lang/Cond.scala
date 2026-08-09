package esmeta.wji.lang

sealed trait Cond
object Cond:
  /** EXPR is EXPR / EXPR is not EXPR, and EXPR equals EXPR */
  case class Eq(lhs: Expr, rhs: Expr, negated: Boolean = false) extends Cond

  /** Comparison operator for [[Compare]]. */
  enum CompareOp:
    case Lt, Le, Gt, Ge

  /** EXPR < EXPR etc. */
  case class Compare(lhs: Expr, op: CompareOp, rhs: Expr) extends Cond

  /** Whether the reference `expr` exists — `EXPR [=map/exists=]` / `EXPR
    * [=map/doesn't exist=]` (spec prose always applies this to an index
    * expression, e.g. `|map|[|key|]`, so `expr` is normally an [[Expr.Index]]),
    * but also reused for a plain record field check (e.g. `|x|.[[Type]]`, an
    * [[Expr.Field]]) by desugar passes like `ExpandThrowsPass` that need to
    * tell a completion record apart from a bare value before reading `.Type`.
    */
  case class HasField(expr: Expr, negated: Boolean = false) extends Cond

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

  /** EXPR has a [[SLOT]] internal slot / EXPR does not have a [[SLOT]] internal
    * slot.
    */
  case class HasSlot(expr: Expr, slot: String, negated: Boolean = false)
    extends Cond

  /** EXPR contains any duplicates / EXPR contains no duplicates (also "does not
    * contain any duplicates") — checks a list for duplicate elements.
    */
  case class HasDuplicates(list: Expr, negated: Boolean = false) extends Cond

  /** ELEM is [not] contained in LIST — list-membership check, e.g. "|funcaddr|
    * is contained in |moduleinst|.funcaddrs".
    */
  case class Contains(elem: Expr, list: Expr, negated: Boolean = false)
    extends Cond

  /** EXPR [=is [not] a[n] TYPE=] — spec type-check predicate */
  case class IsType(expr: Expr, typeName: String, negated: Boolean = false)
    extends Cond

  /** SUBJECT is [not] [=exposed=] in REALM — WebIDL's `[Exposed]`
    * extended-attribute check (`webidl/index.bs`, `<dfn id="dfn-exposed">`):
    * whether `subject` (an interface/callback interface/namespace/member) is
    * exposed in `realm`. `esmeta.wji.compiler.lowering.ExpandExposedPass`
    * eliminates every occurrence before `Compiler` ever sees one — see its own
    * doc for why a genuine call, not an inline check, is needed here.
    */
  case class Exposed(subject: Expr, realm: Expr, negated: Boolean = false)
    extends Cond

  case class And(left: Cond, right: Cond) extends Cond
  case class Or(left: Cond, right: Cond) extends Cond

  /** An expression that stands in for a full condition by abbreviating the
    * operator/LHS from context, e.g. the `[=−∞=]` in `|f32| is [=+∞=] or
    * [=−∞=]`.
    */
  case class Abbreviated(expr: Expr) extends Cond

  /** `any |binder| in COLLECTION[, or COLLECTION, ...] BODY` — "any type in
    * |parameters| or |results| [=matches/valtype=] [=v128=] or [=exnref=]"
    * ("does some element, across all of `collections`, satisfy `body`" — `body`
    * refers to the bound element via `Expr.Var(binder)`). Every `collections`
    * entry is searched, in order — mirrors mainline ESMeta's
    * `ContainsCondition`/`SuchThat` ("list contains an element such that ..."),
    * generalized to more than one list so the parser doesn't need to fold "in A
    * or B" into this node's `Or`-of-two-`Any` shape itself.
    */
  case class Any(binder: String, collections: List[Expr], body: Cond)
    extends Cond

  /** `a/an NOUN |binder| exists such that BODY` — "a [=host address=]
    * |hostaddr| exists such that |map|[|hostaddr|] is the same as |v|"
    * (index.bs:1469) — genuine existential quantification over some implicit,
    * unstated-in-the-prose domain (contrast [[Any]], whose `collections` are
    * spelled out directly). Which domain `binder` actually ranges over has to
    * be inferred from `body`'s own structure (e.g. a `map[binder]` reference
    * implies searching that map's key domain) — a lowering pass's job
    * (`ExpandExistentialsPass`), not this node's; `body` refers to the bound
    * value via `Expr.Var(binder)`, same as [[Any]].
    */
  case class Exists(binder: String, body: Cond) extends Cond

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

  case class Unknown(text: String) extends Cond
