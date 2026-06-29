package esmeta.wji.ir

sealed trait Expr

// -- Literals -----------------------------------------------------------------

/** Mathematical integer or rational, e.g. `42` or `1/2` */
case class EMath(n: BigDecimal) extends Expr

/** IEEE-754 double, e.g. `3.14` */
case class ENum(n: Double) extends Expr

case class EStr(s: String) extends Expr
case class EBool(b: Boolean) extends Expr
case object EUndef extends Expr
case object ENull extends Expr

/** Enumeration value, e.g. `~normal~`, `~throw~`, `~return~` */
case class EEnum(name: String) extends Expr

// -- References ---------------------------------------------------------------

/** Read the value at a reference */
case class ERef(ref: Ref) extends Expr

// -- Operations ---------------------------------------------------------------

case class EUnary(uop: UOp, expr: Expr) extends Expr
case class EBinary(bop: BOp, left: Expr, right: Expr) extends Expr

/** Project the `idx`-th component out of a tuple value. Used to destructure the
  * `(store, funcaddr)` / `(store, val*)` pairs returned by embedding operations
  * such as `func_alloc` / `func_invoke`, which arrive as a
  * [[esmeta.wji.state.ALValue.TupV]] wrapped in [[esmeta.wji.state.WjValue.Wasm]].
  */
case class EProj(expr: Expr, idx: Int) extends Expr

// -- Type / existence checks --------------------------------------------------

/** True if the field/slot `ref` is present */
case class EExists(ref: Ref) extends Expr

/** Runtime type tag of an expression */
case class ETypeOf(expr: Expr) extends Expr

/** True if `expr` is an instance of the named type */
case class ETypeCheck(expr: Expr, ty: String) extends Expr

// -- Allocation ---------------------------------------------------------------

/** Allocate a new record, e.g. `new Record { [[Field]]: expr }` */
case class ERecord(tname: String, fields: List[(String, Expr)]) extends Expr

/** Allocate a new list */
case class EList(elems: List[Expr]) extends Expr

/** Length of a list or string */
case class ELen(expr: Expr) extends Expr

// -- Closures -----------------------------------------------------------------

/** Reference to a named function / abstract operation */
case class EClo(fname: String) extends Expr

// -- Fallback -----------------------------------------------------------------

/** Spec prose that the metalang parser could not recognise (Expr.Unknown). */
case class EUnknown(raw: String) extends Expr

/** Compiler placeholder for constructs not yet compiled. */
case class EYet(msg: String) extends Expr

// -- Operators ----------------------------------------------------------------

enum UOp:
  case Neg  // arithmetic negation
  case Not  // logical NOT
  case BNot // bitwise NOT

enum BOp:
  // arithmetic
  case Add, Sub, Mul, Div, Mod, Pow
  // comparison
  case Eq, NEq, Lt, Le, Gt, Ge
  // logical
  case And, Or
  // bitwise
  case BAnd, BOr, BXOr
