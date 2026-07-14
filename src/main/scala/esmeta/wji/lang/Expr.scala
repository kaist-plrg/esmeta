package esmeta.wji.lang

sealed trait Expr
object Expr:
  case class Var(name: String) extends Expr
  case object This extends Expr
  // value is a decimal or hex (0x...) string literal
  case class Num(value: String) extends Expr
  case class Bool(value: Boolean) extends Expr
  case class Str(value: String) extends Expr

  /** A single byte value (0-255), e.g. the zero-fill element produced when
    * expanding [[NewByteSequence]]. Kept distinct from [[Num]] (a
    * mathematical-value literal) since it compiles to a `Number` value rather
    * than a `Math` value.
    */
  case class Byte(value: Int) extends Expr

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

  /** A raw `[=...=]` Bikeshed autolink, parsed before it's known whether it
    * names a callable algorithm — Bikeshed's `[=...=]` is plain autolink syntax
    * and can just as well point to a term/value definition, so parsing it
    * straight into [[AlgoCall]] would presuppose something the parser can't
    * actually verify (that requires the full set of extracted algorithm names,
    * which only exists once every algorithm has been parsed). This ambiguity
    * only exists for the bare-link and prose-args forms, though — see
    * [[AlgoCall]]. `ResolveLinksPass` resolves every remaining `Link` into
    * either an [[AlgoCall]] (name matches a known algorithm, or it's invoked
    * with args) or a [[SpecTerm]] (a bare reference to something else).
    */
  case class Link(link: String, args: List[Expr]) extends Expr

  /** A confirmed call to an extracted WJI algorithm (or, for an args-carrying
    * link that names something else — a Wasm embedding function, an
    * ECMA-262/WebIDL AO — still a call, just resolved elsewhere at compile
    * time; see `esmeta.wji.compiler.Compiler`). Usually produced by
    * `ResolveLinksPass` from a [[Link]], but [[ExprParser]] constructs this
    * directly for the explicit-parens `[=link=](args)` form — that syntax is
    * unambiguous call notation on its own (mirrors `JSCall`), so there's
    * nothing for a later pass to decide.
    */
  case class AlgoCall(link: String, args: List[Expr]) extends Expr

  /** A SpecTec Wasm Core Spec constructor/variant application (mirrors AL's own
    * `CaseE of mixop * expr list`), e.g. `[=i32.const=] |u32|` or the form that
    * follows `is of the form` in a [[Cond.IsOfForm]] (e.g.
    * `[=external-type/func=] |functype|`). Produced by `ResolveLinksPass` from
    * a [[Link]] whose `link` isn't a known algorithm name but is used with args
    * — syntactically identical to [[AlgoCall]]'s "unknown name with args" case,
    * but semantically a constructor/pattern rather than a call, so it's kept
    * distinct rather than resolved elsewhere at compile time the way an
    * unrecognized [[AlgoCall]] is.
    */
  case class Case(tag: String, args: List[Expr]) extends Expr
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

  /** Arithmetic binary operator; `Sub` covers both "-" and "&minus;" as seen in
    * spec prose.
    */
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

  /** "a new [=byte sequence=] of [=byte sequence/length=] equal to LENGTH" — a
    * freshly allocated byte sequence of the given length (each byte 0, per the
    * Infra Standard's "byte sequence" definition). Not yet evaluable; kept as a
    * distinct node (rather than falling into [[UnknownNew]]) since `length` is
    * itself a meaningful sub-expression worth preserving.
    */
  case class NewByteSequence(length: Expr) extends Expr

  /** "[=the range=] LOW to HIGH, inclusive" — an integer range used as the
    * collection of a `For` loop. Both bounds are assumed inclusive for now,
    * since every occurrence seen in the spec so far is written "...,
    * inclusive"; revisit if an "exclusive" phrasing ever shows up.
    */
  case class Range(low: Expr, high: Expr) extends Expr

  /** Spec prose that didn't match any recognised expression pattern. */
  case class Unknown(raw: String) extends Expr

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

  /** An unnamed, not-yet-hoisted closure literal — "the following steps ...:"
    * itself, wherever it appears as an argument/value, taking `params` as
    * formal parameters (no `|` delimiters). Two spec phrasings currently
    * produce this (see [[ExprParser]]):
    *
    *   - `"Let |x| be the following steps given argument |V|: substeps"` —
    *     parsed directly as `Instr.Let`'s RHS, with `params` from the `given
    *     argument(s)` list.
    *   - `"[=Queue a task=] ... to perform the following steps: substeps"` —
    *     parsed as one of `Instr.Perform`'s `args` (`params` always `Nil`,
    *     mirroring ECMA-262's "a new Job Abstract Closure ... that captures
    *     ...").
    *
    * No `body` field: [[ExprParser]] only ever sees the one prose string
    * introducing the phrase, not the nested list items it introduces — those
    * always sit in the *owning instruction's* own `body` (exactly like any
    * other nested block; every earlier lowering pass already recurses into it
    * via `Instr.mapBody`), so a `body` field here would only ever hold `Nil`.
    * `esmeta.wji.compiler.lowering.ExpandFollowingStepsPass` recognizes an
    * unlowered `FollowingSteps` by the owning instruction's `body` still being
    * non-empty, hoists that `body` into a fresh top-level [[Algorithm]], and
    * replaces the `FollowingSteps` with a [[Closure]] referencing it.
    */
  case class FollowingSteps(params: List[String]) extends Expr

  /** Projects component `idx` out of `base`, a Wasm-embedding call's `(store,
    * X)`-shaped result (e.g. `module_instantiate`, `func_invoke`, `func_alloc`,
    * `global_alloc`, ... — every embedding function that returns a tuple pairs
    * the updated store with its own result). Produced by
    * `esmeta.wji.compiler.lowering.ExpandDestructuringLetPass` in place of a
    * plain [[Index]], since the runtime representation is a Wasm-side `TupV`,
    * not a heap list/record — [[Index]] compiles to a `Field` (heap-addressed)
    * read, whereas this compiles directly to `ir.EProj`, the IR node dedicated
    * to unpacking a `Wasm(TupV(...))`. See `esmeta.wji.compiler.Compiler`.
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
