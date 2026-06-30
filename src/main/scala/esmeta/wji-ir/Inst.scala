package esmeta.wji.ir

sealed trait Inst

/** Evaluate `expr` and discard the result (for side effects) */
case class IExpr(expr: Expr) extends Inst

/** `let lhs = expr` — introduce a new local binding */
case class ILet(lhs: Name, expr: Expr) extends Inst

/** `ref = expr` — mutate an existing reference */
case class IAssign(ref: Ref, expr: Expr) extends Inst

/** Delete a field or local binding */
case class IDelete(ref: Ref) extends Inst

/** Push `elem` onto `list` at the front or back */
case class IPush(elem: Expr, list: Expr, front: Boolean) extends Inst

/** Pop an element from `list` at the front or back, bind it to `lhs` */
case class IPop(lhs: Name, list: Expr, front: Boolean) extends Inst

/** Return from the current function */
case class IReturn(expr: Expr) extends Inst

/** Runtime assertion — throws if `expr` is false */
case class IAssert(expr: Expr) extends Inst

/** No-op */
case class INop() extends Inst

/** Evaluate `expr` and print a human-readable rendering to stdout (a general
  * debugging aid for observing values while running any IR function).
  */
case class IPrint(expr: Expr) extends Inst

/** Execute a sequence of instructions in order */
case class ISeq(insts: List[Inst]) extends Inst

/** `if cond then thenI else elseI` */
case class IIf(cond: Expr, thenI: Inst, elseI: Inst) extends Inst

/** `while cond do body` */
case class IWhile(cond: Expr, body: Inst) extends Inst

/** `lhs = fexpr(args)` — call an IR-defined WJI function and bind the result.
  *
  * `fexpr` is typically `EClo(name)` (static call) or `ERef(Name(f))` (dynamic
  * call through a closure value). The callee is resolved against the functions
  * defined in the [[Program]]. To call a Wasm embedding operation, use
  * [[ICallEmbed]] instead.
  */
case class ICall(lhs: Name, fexpr: Expr, args: List[Expr]) extends Inst

/** `lhs = <embed fname>(args)` — call a Wasm embedding function (e.g.
  * `module_decode`, `func_invoke`) across the
  * [[esmeta.wji.bridge.host.WasmHost]] boundary, and bind the result.
  *
  * `fname` names an embedding operation directly rather than through a closure
  * value, because the embedding interface is a fixed catalog defined by the
  * Wasm standard (see `esmeta.wji.bridge.host.WasmHost`).
  */
case class ICallEmbed(lhs: Name, fname: String, args: List[Expr]) extends Inst
