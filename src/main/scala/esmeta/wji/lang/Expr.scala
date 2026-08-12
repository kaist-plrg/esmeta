package esmeta.wji.lang

sealed trait Expr
object Expr:
  case class Var(name: String) extends Expr
  case class Num(value: String) extends Expr // decimal or hex string literal
  case class Bool(value: Boolean) extends Expr
  case class Str(value: String) extends Expr

  case object This extends Expr // only used in interface member
  case object GivenValue extends Expr // only used in setter

  /** A single byte value (0-255), e.g. the zero-fill element produced when
    * expanding [[NewByteSequence]]. Kept distinct from [[Num]] (a
    * mathematical-value literal) since it compiles to a `Number` value rather
    * than a `Math` value.
    */
  case class Byte(value: Int) extends Expr

  /** A field read on a record/structure: base's field named `name`. Covers both
    * ECMA-262's `.[[name]]` internal-slot syntax and a WebAssembly-spec field
    * written as `base.[=name=]`.
    */
  case class Field(base: Expr, name: String) extends Expr
  case class Index(base: Expr, key: Expr) extends Expr

  /** A raw `[=...=]` Bikeshed autolink. `ResolveLinksPass` resolves every
    * `Link` into an [[AlgoCall]], [[Case]], or [[SpecTerm]].
    */
  case class Link(link: String, args: List[Expr]) extends Expr
  case class AlgoCall(link: String, args: List[Expr]) extends Expr
  case class JSCall(name: String, args: List[Expr]) extends Expr
  case class Case(tag: String, args: List[Expr]) extends Expr
  case class SpecTerm(name: String) extends Expr

  case class Abrupt(check: String, expr: Expr) extends Expr
  case class New(iface: String) extends Expr
  case class UnknownNew(raw: String) extends Expr

  /** "a new {{ArrayBuffer}} with the internal slots [[X]], [[Y]], ..." —
    * js-api's own bespoke ArrayBuffer construction (`create a fixed length
    * memory buffer`/`create a resizable memory buffer`), kept distinct from
    * [[New]] (WebIDL's "a [=/new=] {{X}}" platform-object idiom) since
    * `ArrayBuffer` isn't a js-api-defined interface at all — it's an ECMA-262
    * primitive js-api merely reuses, so it needs a different construction path
    * entirely (see docs/underspecified-behaviors.md: no algorithm is actually
    * named for what this phrasing should call). Expanded by
    * `esmeta.wji.compiler.lowering.ExpandNewArrayBufferPass` into a `JSCall` on
    * ECMA-262's own `AllocateArrayBuffer`.
    */
  case object NewArrayBuffer extends Expr

  /** `« E1, E2, ... »` — Infra-spec List literal. */
  case class List_(elements: List[Expr]) extends Expr

  /** `«[ K1 → V1, K2 → V2, ... ]»` — Infra-spec ordered map literal. */
  case class Map_(entries: List[(Expr, Expr)]) extends Expr

  /** The length/size of a string, list, or map (entry count). */
  case class Length(expr: Expr) extends Expr

  enum BOp:
    case Add, Sub, Mul, Div, Mod

  /** `lhs op rhs` — arithmetic binary operation. */
  case class BinOp(lhs: Expr, op: BOp, rhs: Expr) extends Expr

  /** `base<sup>exp</sup>` — exponentiation. */
  case class Pow(base: Expr, exp: Expr) extends Expr

  /** Unary negation `-expr`. */
  case class Neg(expr: Expr) extends Expr

  /** `|x| interpreted as a [=mathematical value=]` — cast to math value. */
  case class AsMath(expr: Expr) extends Expr

  /** `X as a WebAssembly [=u64=]` — annotates `expr` as being in Wasm's value
    * domain rather than JS's. Compiles to `expr` unchanged (see `Compiler`'s
    * own case): whatever consumes it is either an embedding call (`ICallEmbed`,
    * which converts every argument via `toAL` at the call boundary regardless
    * of this annotation) or plain arithmetic on the math value `expr` already
    * denotes, so the annotation carries no information this pipeline needs to
    * act on.
    */
  case class AsWasm(expr: Expr, ty: String) extends Expr

  /** `[=𝔽=](...)` call syntax — ECMA-262's "the Number value for" notation */
  case class AsNumber(expr: Expr) extends Expr

  /** `[=ℤ=](...)` — ECMA-262's "the BigInt value for" notation */
  case class AsBigInt(expr: Expr) extends Expr

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
    * spec. `cond` is a real [[Cond]] (parsed via `CondParser` at the same time
    * `desc`/`cond` are split out, `ExprParser`'s only use of `CondParser`) —
    * unlike `desc`, it's an actual predicate every other part of this AST
    * already knows how to walk/resolve/hoist (`ResolveLinksPass`, evaluation-
    * order normalization, ...), so keeping it as opaque prose would hide it
    * from all of that for no reason. Not yet evaluable itself; kept as a
    * distinct node for the same reason as [[Described]].
    */
  case class SuchThat(desc: String, cond: Cond) extends Expr

  /** `EXPR if COND[,] (and|or) EXPR otherwise` — WebIDL's conditional
    * expression idiom (webidl_yet_categorized.md category I-G), e.g. "Let
    * |modifiable| be <emu-val>false</emu-val> if |op| is [=unforgeable=] and
    * <emu-val>true</emu-val> otherwise." `thenExpr` is the `EXPR` before "if";
    * `elseExpr` is the `EXPR` after the "and"/"or" connector and before
    * "otherwise" — the spec prose itself never says "then"/"else", only
    * "if"/"otherwise", but the two field names still read clearly for what each
    * position means. Every real occurrence seen so far is a `Let` RHS, but
    * `ExprParser.parse` recognizes this as an ordinary expression shape
    * wherever it appears, the same as `SuchThat` above. Not compiled via
    * `compileExpr` — there's no IR-level conditional *expression*, only a
    * conditional *instruction* — so `esmeta.wji.compiler.lowering.
    * ExpandConditionalPass` desugars a `Let` bound to this shape into a real
    * `Instr.IfChain` (binding the `Let`'s LHS in both branches) before
    * `Compiler` ever sees it.
    */
  case class Conditional(cond: Cond, thenExpr: Expr, elseExpr: Expr)
    extends Expr

  /** "a new [=byte sequence=] of [=byte sequence/length=] equal to LENGTH" — a
    * freshly allocated byte sequence of the given length (each byte 0, per the
    * Infra Standard's "byte sequence" definition). Not yet evaluable; kept as a
    * distinct node (rather than falling into [[UnknownNew]]) since `length` is
    * itself a meaningful sub-expression worth preserving.
    */
  case class NewByteSequence(length: Expr) extends Expr

  /** "a [=Data Block=] which is [=identified with=] the underlying memory of
    * |memaddr|" — the live-aliasing construct `create a fixed length memory
    * buffer`/`create a resizable memory buffer`/`refresh the Memory buffer` all
    * use for `WebAssembly.Memory.prototype.buffer`. Not directly evaluable —
    * WJI can't truly alias JVM/OCaml memory across the process boundary —
    * expanded by `esmeta.wji.compiler.lowering.ExpandDataBlockOfPass` into an
    * explicit `mem_read_bytes` bridge call plus a fill loop. Parsed directly to
    * this dedicated node (rather than falling into the generic [[Described]],
    * which also covers unrelated "which ..." phrasings like "a [=host
    * function=] which executes |steps| when called") since this is the one
    * "which ..." shape that actually needs its own lowering.
    */
  case class DataBlockOf(memaddr: Expr) extends Expr

  /** "[=the range=] LOW to HIGH, inclusive" — an integer range used as the
    * collection of a `For` loop. Both bounds are assumed inclusive for now,
    * since every occurrence seen in the spec so far is written "...,
    * inclusive"; revisit if an "exclusive" phrasing ever shows up.
    */
  case class Range(low: Expr, high: Expr) extends Expr

  /** "the index of LIST where ELEM is found" — the position of `elem` in `list`
    * (index.bs:1255, `name of the WebAssembly function`). Not directly
    * evaluable; expanded into a real search loop by `ExpandIndexOfPass`, which
    * only handles it in direct `Let` RHS position (the only shape seen so far)
    * — see that pass's own doc.
    */
  case class IndexOf(list: Expr, elem: Expr) extends Expr

  /** An unnamed, not-yet-hoisted closure literal — "the following steps ...:"
    * itself, wherever it appears as an argument/value, taking `params` as
    * formal parameters (no `|` delimiters). If vardadicLast is true, the last
    * parameter of params consumes all of remaining args and stores as the list.
    */
  case class FollowingSteps(params: List[String], variadicLast: Boolean = false)
    extends Expr

  /** "performing CLOSURE given ARG[, ARG...][ and ARG]" — invoking a closure
    * *value* (as opposed to [[Instr.Perform]], which invokes a *named*
    * `[=link=]` as a statement). `closure` is typically a bare [[Var]] holding
    * a closure passed in as a parameter (e.g. `|onFullfilledStepsArg|` in the
    * (patched) PromiseReactionJob text — see `SpecPatch`), but is kept as a
    * general `Expr` rather than a bare name since nothing about the "performing
    * ... given ..." phrasing restricts it to a variable. Mirrors [[AlgoCall]]/
    * [[Case]]'s `args: List[Expr]` shape rather than [[FollowingSteps]]'s
    * bare-name `params: List[String]`, since a call's arguments are arbitrary
    * expressions, not formal parameter names.
    */
  case class ClosureCall(closure: Expr, args: List[Expr]) extends Expr

  /** Spec prose that didn't match any recognised expression pattern. */
  case class Unknown(raw: String) extends Expr

  // ---- Below: no spec surface syntax of their own. `ExprParser` never
  // produces any of these — only later lowering/compilation passes do, once
  // something needs an `Expr` shape spec prose doesn't literally write. ----

  /** A reference to a closure value over the synthetic algorithm `name`,
    * capturing the current values of `captured` variable names from the
    * enclosing scope. Always fully-formed — the sole producer is
    * `esmeta.wji.compiler.lowering.ExpandFollowingStepsPass`, which hoists a
    * [[FollowingSteps]]'s substeps into a fresh top-level [[Algorithm]] named
    * `name` and replaces the `FollowingSteps` with a reference to it. No
    * `params` field: a closure *value* only needs identity + captures, not its
    * callee's formal parameter list — that's a property of the hoisted
    * [[Algorithm]] itself, not of a reference to it (mirrors `ir.EClo(fname,
    * captured)`, which has no params either). `captured` is computed
    * automatically (free-variable analysis over the substeps, excluding the
    * callee's params) rather than parsed from spec prose, since wji's "the
    * following steps ...:" phrasings never spell out an explicit capture list
    * the way ECMA-262's `AbstractClosureExpression` does. Compiles directly to
    * `ir.EClo(name, captured)` — see `esmeta.wji.compiler.Compiler`.
    */
  case class Closure(name: String, captured: List[String]) extends Expr

  /** Projects component `idx` out of `base`, a Wasm-embedding call's `(store,
    * X)`-shaped result (e.g. `module_instantiate`, `func_invoke`, `func_alloc`,
    * `global_alloc`, ... — every embedding function that returns a tuple pairs
    * the updated store with its own result), or the `idx`-th positional
    * constructor argument of a `Wasm(CaseV(...))`. Produced by
    * `esmeta.wji.compiler.lowering.ExpandDestructuringLetPass`/
    * `ExpandIsOfFormPass`. Compiles to the same `ir.Field` ref [[Index]] does
    * (see `esmeta.wji.compiler.Compiler`) — `State.apply` dispatches on the
    * base value's runtime shape, so the identical `Field` read works whether it
    * turns out to be a heap object or an opaque `Wasm` value; kept as its own
    * metalang node rather than reusing [[Index]] outright since the index here
    * is always a literal `Int`, not an arbitrary key `Expr`.
    */
  case class TupleProj(base: Expr, idx: Int) extends Expr

  /** Reads `base`'s SpecTec `CaseV` constructor tag as a string (mirrors
    * [[TupleProj]]'s role for the positional payload — see its doc). Produced
    * by `esmeta.wji.compiler.lowering.ExpandIsOfFormPass` as the LHS of a
    * `Cond.Eq` comparison against the expected tag, in place of a dedicated "is
    * of this form" condition node — a form check is just a string equality once
    * the tag has been read out, so it reuses [[Cond.Eq]] rather than
    * duplicating it. Compiles directly to `ir.ECaseTag`. See
    * `esmeta.wji.compiler.Compiler`.
    */
  case class CaseTag(base: Expr) extends Expr

  /** `Some`/`None` wrapping around a `Case` constructor argument — e.g. Wasm's
    * `null? heaptype` reftype shape, which SpecTec represents as a genuine
    * `OptV` positional argument inside `CaseV("REF", [OptV(...), ...])`.
    * Produced by `esmeta.wji.compiler.lowering.NormalizeSpecTecCaseShapePass`
    * when reshaping a `SpecTerm` shorthand (e.g. `funcref`) into its real
    * nested `Case` form. Compiles directly to `ir.EOpt`. See
    * `esmeta.wji.compiler.Compiler`.
    */
  case class Opt(expr: Option[Expr]) extends Expr

  case class Enum(s: String) extends Expr

  case class GetMember(definition: Expr, member: MemberKind) extends Expr

  extension (expr: Expr)
    /** Every `Expr` directly nested one level inside this one — used for
      * generic "is a residual X still anywhere in here" invariant checks (e.g.
      * a lowering pass asserting a construct it only partially expands is
      * really gone), rather than hand-writing that traversal at each call site.
      * Mirrors `Instr.mapBody`'s role for instruction-level nesting.
      */
    def children: List[Expr] = expr match
      case Field(base, _)             => List(base)
      case Index(base, key)           => List(base, key)
      case Link(_, args)              => args
      case AlgoCall(_, args)          => args
      case Case(tag, args)            => args
      case JSCall(_, args)            => args
      case Abrupt(_, e)               => List(e)
      case List_(elems)               => elems
      case Map_(entries)              => entries.flatMap((k, v) => List(k, v))
      case Length(e)                  => List(e)
      case BinOp(l, _, r)             => List(l, r)
      case Pow(base, exp)             => List(base, exp)
      case Neg(e)                     => List(e)
      case AsMath(e)                  => List(e)
      case AsWasm(e, _)               => List(e)
      case AsNumber(e)                => List(e)
      case AsBigInt(e)                => List(e)
      case Tuple(elems)               => elems
      case NewByteSequence(length)    => List(length)
      case DataBlockOf(memaddr)       => List(memaddr)
      case Range(low, high)           => List(low, high)
      case IndexOf(list, elem)        => List(list, elem)
      case ClosureCall(closure, args) => closure :: args
      case TupleProj(base, _)         => List(base)
      case CaseTag(base)              => List(base)
      case Opt(inner)                 => inner.toList
      case Conditional(_, t, e)       => List(t, e)
      case _                          => Nil

    /** Whether `pred` holds for this `Expr` or any `Expr` nested inside it, at
      * any depth.
      */
    def containsWhere(pred: Expr => Boolean): Boolean =
      pred(expr) || expr.children.exists(_.containsWhere(pred))
