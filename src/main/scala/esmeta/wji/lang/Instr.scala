package esmeta.wji.lang

/** A structured view of a single algorithm step.
  *
  * Distinguishes the *shape* of a step (`Let`, `If`, `Return`, ...).
  * Value-position sub-expressions are typed as [[Expr]]; conditions,
  * references, and collection expressions remain as raw spec prose strings.
  *
  * `If`/`ElseIf`/`Else` are kept as separate sibling [[Instr]]s, mirroring how
  * they appear as separate items in the spec's numbered lists.
  */
sealed trait Instr:
  /** nested instructions that run as part of this one, e.g. the branch of an
    * `If`, the loop body of a `ForEach`/`While`, or the steps performed before
    * a `Return`/`Throw`/...
    */
  def body: List[Instr]

object Instr:
  /** What happens with the result of a [[Perform]] call. */
  enum PerformOutcome:
    case Discard
    case ReturnResult
    case BindResult(variable: String)

  /** `Let LHS be EXPR.` */
  case class Let(lhs: Expr, expr: Expr, body: List[Instr] = Nil) extends Instr

  /** `Set LHS to EXPR.` */
  case class Set(lhs: Expr, expr: Expr, body: List[Instr] = Nil) extends Instr

  /** `If COND, ...` */
  case class If(cond: Cond, body: List[Instr]) extends Instr

  /** `Else if COND, ...` / `Otherwise, if COND, ...` */
  case class ElseIf(cond: Cond, body: List[Instr]) extends Instr

  /** `Else, ...` / `Otherwise, ...` */
  case class Else(body: List[Instr]) extends Instr

  /** `Return EXPR.` / `Return.` */
  case class Return(expr: Option[Expr], body: List[Instr] = Nil) extends Instr

  /** `Assert: COND.` */
  case class Assert(cond: Cond, body: List[Instr] = Nil) extends Instr

  /** `Throw TARGET.` / `[=Throw=] TARGET.` */
  case class Throw(target: String, body: List[Instr] = Nil) extends Instr

  /** `For each ELEM of/in COLLECTION, ...` */
  case class ForEach(elem: Expr, collection: Expr, body: List[Instr])
    extends Instr

  /** `While COND: ...` */
  case class While(cond: Cond, body: List[Instr]) extends Instr

  /** `Run the following steps [=in parallel=]:` */
  case class RunInParallel(body: List[Instr]) extends Instr

  /** `[=list/Append=] ITEM to COLLECTION.` */
  case class Append(item: Expr, collection: Expr, body: List[Instr] = Nil)
    extends Instr

  /** `[=iteration/continue=].` */
  case class Continue(body: List[Instr] = Nil) extends Instr

  /** a named-operation call used as a statement; `outcome` describes what
    * happens with the result (discard, return, or bind to a variable)
    */
  case class Perform(
    func: String,
    args: List[Expr],
    outcome: PerformOutcome = PerformOutcome.Discard,
    body: List[Instr] = Nil,
  ) extends Instr

  /** `Note: TEXT` */
  case class Note(text: String, body: List[Instr] = Nil) extends Instr

  /** anything that didn't match a recognized shape, kept verbatim for later
    * refinement
    */
  case class Unknown(text: String, body: List[Instr] = Nil) extends Instr

  /** Produced by [[esmeta.wji.compiler.desugar.GroupIfChainPass]]: a fully
    * grouped if/else-if/else tree. Replaces flat `If`/`ElseIf`/`Else` siblings.
    * `branches` is non-empty; `fallback` is the else-body (may be empty if
    * there is no else clause).
    */
  case class IfChain(
    branches: List[(Cond, List[Instr])],
    fallback: List[Instr],
  ) extends Instr:
    def body: List[Instr] = Nil

  /** Apply `f` to the direct `body` of this instruction, returning a copy with
    * the transformed body. Does not recurse — callers are responsible for
    * traversal.
    */
  extension (instr: Instr)
    def mapBody(f: List[Instr] => List[Instr]): Instr = instr match
      case i: Let           => i.copy(body = f(i.body))
      case i: Set           => i.copy(body = f(i.body))
      case i: If            => i.copy(body = f(i.body))
      case i: ElseIf        => i.copy(body = f(i.body))
      case i: Else          => i.copy(body = f(i.body))
      case i: Return        => i.copy(body = f(i.body))
      case i: Assert        => i.copy(body = f(i.body))
      case i: Throw         => i.copy(body = f(i.body))
      case i: ForEach       => i.copy(body = f(i.body))
      case i: While         => i.copy(body = f(i.body))
      case i: RunInParallel => i.copy(body = f(i.body))
      case i: Append        => i.copy(body = f(i.body))
      case i: Continue      => i.copy(body = f(i.body))
      case i: Perform       => i.copy(body = f(i.body))
      case i: Note          => i.copy(body = f(i.body))
      case i: Unknown       => i.copy(body = f(i.body))
      case i: IfChain =>
        i.copy(
          branches = i.branches.map((c, b) => (c, f(b))),
          fallback = f(i.fallback),
        )
