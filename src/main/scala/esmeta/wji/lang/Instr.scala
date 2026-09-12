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

  /** `Throw TARGET.` / `[=Throw=] TARGET.` — `target` is either an
    * `Expr.New(iface)` ("a {{TypeError}} exception"/"a [=/new=] {{X}} object",
    * constructing a fresh error) or any other already-computed value expression
    * (e.g. `[=ToJSValue=](|payload|[0])`, `|exception|`) to throw directly —
    * see [[esmeta.wji.compiler.lowering.CompletionWrapping]] for how the two
    * are told apart and compiled differently.
    */
  case class Throw(target: Expr, body: List[Instr] = Nil) extends Instr

  /** `For each ELEM of/in COLLECTION, ...` */
  case class ForEach(elem: Expr, collection: Expr, body: List[Instr])
    extends Instr

  /** `For each ELEM1 and ELEM2 of/in COLLECTION1 and COLLECTION2, paired
    * linearly, ...` -- WHATWG Infra's "paired linearly" idiom for iterating two
    * lists in lockstep (zip), e.g. js-api's `Exception` constructor ("For each
    * |value| and |resultType| of |payload| and |types|, paired linearly, ...").
    * Kept as its own case rather than generalizing [[ForEach]] to N-way: the
    * single-collection form is overwhelmingly the common one, and this idiom is
    * rare (2 occurrences in the whole js-api spec) -- see
    * `esmeta.wji.compiler.lowering.ExpandForEachPass`, the only place with any
    * real interest in it (fully eliminated during lowering, so nothing past
    * that point needs to know this case exists).
    */
  case class ForEachPaired(
    elem1: Expr,
    elem2: Expr,
    collection1: Expr,
    collection2: Expr,
    body: List[Instr],
  ) extends Instr

  /** `For ELEM in COLLECTION, ...` — distinct from [[ForEach]] since the spec
    * phrases it without "each" (e.g. "For |i| in [=the range=] |offset| to
    * |offset| + |length| &minus; 1, inclusive, ..."). `collection` is commonly
    * an [[Expr.Range]], but this instruction itself isn't specific to ranges.
    */
  case class For(elem: Expr, collection: Expr, body: List[Instr]) extends Instr

  /** `While COND: ...` */
  case class While(cond: Cond, body: List[Instr]) extends Instr

  /** `Run the following steps [=in parallel=]:` */
  case class RunInParallel(body: List[Instr]) extends Instr

  /** `[=list/Append=] ITEM to COLLECTION.` */
  case class Append(item: Expr, collection: Expr, body: List[Instr] = Nil)
    extends Instr

  /** `[=list/Remove=] from LIST all the ELEMKIND that are PROPERTY.` — removes
    * every element of `list` for which `property` holds. Both `elemKind` (e.g.
    * `"operations"`) and `property` (e.g. `"unforgeable"`) are kept as raw spec
    * text, not resolved at parse time: `elemKind` becomes an assertion about
    * each element's kind and `property` becomes the actual filter condition,
    * once `esmeta.wji.compiler.lowering.ExpandRemovePass` expands this into an
    * explicit loop — including synthesizing a fresh per-element binder, since
    * the spec prose itself never names one.
    */
  case class Remove(
    list: Expr,
    elemKind: String,
    property: String,
    body: List[Instr] = Nil,
  ) extends Instr

  /** Removes and returns the last element of `list` in place, binding it to
    * `lhs`. Compiles to a real `esmeta.ir.IPop`.
    */
  case class Pop(lhs: Expr, list: Expr, body: List[Instr] = Nil) extends Instr

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

  /** a closure-*value* call used as a statement (contrast with [[Perform]],
    * whose `func` is a static `[=link=]` name); produced by
    * `esmeta.wji.compiler.lowering.ExpandClosureCallPass` from an
    * [[Expr.ClosureCall]] found in `Let`/`Return` RHS position. `outcome`
    * mirrors [[Perform]]'s.
    */
  case class PerformClosure(
    closure: Expr,
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

  /** `Try running the following steps: ...` — `call` is the try-body, parsed
    * the same way every other "verb + the following steps:" idiom is: as an
    * immediately-invoked closure call over a `FollowingSteps` placeholder
    * (`Expr.ClosureCall(Expr.FollowingSteps(Nil, false), Nil)`), with `body`
    * holding the step's own nested list — the raw try-body instructions,
    * exactly as [[esmeta.wji.extractor.AlgorithmExtractor]] naturally attaches
    * any step's nested list. `esmeta.wji.compiler.lowering.
    * ExpandFollowingStepsPass` hoists the `FollowingSteps` (and, with it,
    * `body`) into a fresh top-level `Algorithm`, the same way it already does
    * for [[Let]], turning `call` into `Expr.ClosureCall(Expr. Closure(name,
    * captured), Nil)` and `body` into `Nil`. Must be immediately followed by a
    * sibling [[Catch]] to mean anything — see
    * `esmeta.wji.compiler.lowering.ExpandThrowsPass`, which expands the pair
    * into a real completion-record check.
    */
  case class Try(call: Expr, body: List[Instr] = Nil) extends Instr

  /** `And then, if <a lt="an exception was thrown">an exception |E| was
    * thrown</a>: ...` — WebIDL's catch clause; only meaningful as the sibling
    * immediately following a [[Try]]. `exceptionVar` is raw, with pipes (e.g.
    * `"|E|"`).
    */
  case class Catch(exceptionVar: String, body: List[Instr]) extends Instr

  /** Produced by [[esmeta.wji.compiler.lowering.GroupIfChainPass]]: a fully
    * grouped if/else-if/else tree. Replaces flat `If`/`ElseIf`/`Else` siblings.
    * `branches` is non-empty; `fallback` is the else-body (may be empty if
    * there is no else clause).
    */
  case class IfChain(
    branches: List[(Cond, List[Instr])],
    fallback: List[Instr],
  ) extends Instr:
    def body: List[Instr] = Nil

  /** Whether every path through `body` is guaranteed to reach a `Return` or
    * `Throw` — i.e. execution can never fall off the end. Also recognizes a
    * `Perform`/`PerformClosure` with a `ReturnResult` outcome — the "call X and
    * return the result" shape — as an exit: `Perform`'s own `ReturnResult` is
    * ordinarily eliminated by
    * `esmeta.wji.compiler.lowering.ExpandPerformReturnResultPass` well before
    * any `alwaysExits` call site runs, but recognizing it here too avoids
    * silently depending on that pass having already run first;
    * `PerformClosure`'s `ReturnResult` is never eliminated at all — it's the
    * normal output of `esmeta.wji.compiler.lowering.ExpandClosureCallPass`,
    * which `Compiler` compiles directly into a real `IReturn` (see its own
    * doc). Recognizes an `IfChain` as exhaustive only when it has a non-empty
    * `fallback` (a real `else`) and every branch, including the fallback,
    * itself always exits. Does not look inside loop bodies
    * (`ForEach`/`ForEachPaired`/`For`/`While`) or `RunInParallel` — none of
    * those guarantee they run at all, or that the current algorithm exits when
    * they do.
    */
  def alwaysExits(body: List[Instr]): Boolean =
    body.exists {
      case _: Return                                            => true
      case _: Throw                                             => true
      case Perform(_, _, PerformOutcome.ReturnResult, _)        => true
      case PerformClosure(_, _, PerformOutcome.ReturnResult, _) => true
      case IfChain(branches, fallback) =>
        fallback.nonEmpty &&
        branches.forall((_, b) => alwaysExits(b)) &&
        alwaysExits(fallback)
      case _ => false
    }

  /** Apply `f` to the direct `body` of this instruction, returning a copy with
    * the transformed body. Does not recurse — callers are responsible for
    * traversal.
    */
  extension (instr: Instr)
    def mapBody(f: List[Instr] => List[Instr]): Instr = instr match
      case i: Let            => i.copy(body = f(i.body))
      case i: Set            => i.copy(body = f(i.body))
      case i: If             => i.copy(body = f(i.body))
      case i: ElseIf         => i.copy(body = f(i.body))
      case i: Else           => i.copy(body = f(i.body))
      case i: Return         => i.copy(body = f(i.body))
      case i: Assert         => i.copy(body = f(i.body))
      case i: Throw          => i.copy(body = f(i.body))
      case i: ForEach        => i.copy(body = f(i.body))
      case i: ForEachPaired  => i.copy(body = f(i.body))
      case i: For            => i.copy(body = f(i.body))
      case i: While          => i.copy(body = f(i.body))
      case i: RunInParallel  => i.copy(body = f(i.body))
      case i: Append         => i.copy(body = f(i.body))
      case i: Remove         => i.copy(body = f(i.body))
      case i: Pop            => i.copy(body = f(i.body))
      case i: Continue       => i.copy(body = f(i.body))
      case i: Perform        => i.copy(body = f(i.body))
      case i: PerformClosure => i.copy(body = f(i.body))
      case i: Note           => i.copy(body = f(i.body))
      case i: Unknown        => i.copy(body = f(i.body))
      case i: Try            => i.copy(body = f(i.body))
      case i: Catch          => i.copy(body = f(i.body))
      case i: IfChain =>
        i.copy(
          branches = i.branches.map((c, b) => (c, f(b))),
          fallback = f(i.fallback),
        )
