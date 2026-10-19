package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Cond, Expr, Instr}
import esmeta.wji.lang.Instr.PerformOutcome

/** Shared logic for wrapping every `Return`/`Throw` exit path of an algorithm's
  * body in an ECMA-262 Completion Record: converts a `Instr.Throw` into a real
  * `ThrowCompletion` return, and wraps every other `Instr.Return` in
  * `NormalCompletion` unless the value being returned is already itself a
  * completion (checked at runtime via `HasField`, mirroring mainline
  * `esmeta.compiler.Compiler`'s own `isCompletion(x)` guard on `ReturnStep`) —
  * so a caller can always assume the result has a `.Type` field. That runtime
  * check is skipped — emitting an unconditional wrap, or (see
  * [[provenCompletion]]) no wrap at all — whenever it would otherwise compile
  * to permanently dead code: either `expr`'s own shape already fixes its
  * compiled type to something other than a Completion Record
  * ([[neverCompletion]]), or an enclosing branch's own condition already proved
  * `expr` to be one (most commonly [[ExpandAbruptPass]]'s own `?`-propagation
  * branch, whose condition and body — `Cond.IsType(inner, "AbruptCompletion")`
  * guarding exactly `Return(inner)` — otherwise has this pass immediately
  * re-ask the very question its enclosing `if` was just built to answer). Both
  * were found via `esmeta.wji.WjiCoverage`'s per-branch hit counts against the
  * official corpus always landing 0 on the redundant check's dead side — see
  * `personal/coverage-gaps.md`.
  *
  * {{{
  *   If(cond, [Throw(New("TypeError"))])
  *   Return(Some(expr))
  * }}}
  * becomes:
  * {{{
  *   If(cond, [
  *     Perform("__NEW_ERROR_OBJ__", [Str("%TypeError.prototype%")], BindResult(_err1)),
  *     Perform("ThrowCompletion", [Var(_err1)], BindResult(_ret1)),
  *     Return(Some(Var(_ret1))),
  *   ])
  *   Let(_v1, expr)
  *   If(_v1 has field "Type", [Return(Some(_v1))])
  *   Else [
  *     Perform("NormalCompletion", [_v1], BindResult(_ret2)),
  *     Return(Some(Var(_ret2))),
  *   ]
  * }}}
  *
  * `Instr.Throw`'s `target` is already a real `Expr` by the time this runs
  * (parsed at `InstrParser`, not here — see `Instr.Throw`'s own doc): an
  * `Expr.New(iface)` ("a {{Iface}} exception"/"a [=/new=] {{Iface}} object")
  * constructs a fresh error via `__NEW_ERROR_OBJ__` as before; anything else is
  * already a computed value (e.g. `ToJSValue(...)`, a bound variable) and gets
  * wrapped in `ThrowCompletion` directly, no construction step. Always emits
  * already-compiler-ready `Instr.Perform(..., BindResult(_))` + `Instr.Return`
  * — never an inline `Expr.AlgoCall` — so a caller never needs a later pass
  * (`ExpandInlineAlgoCallPass`/`ExpandPerformReturnResultPass`) to finish the
  * job for it.
  *
  * Kept as a plain object rather than folded into [[WrapCompletionReturnsPass]]
  * itself — mirrors [[CompletionAlgorithms]]'s own split from
  * [[MarkCompletionAlgorithmsPass]]: this object is the rewrite logic itself,
  * [[WrapCompletionReturnsPass]] is just the thin driver deciding *which*
  * algorithms (every one with `returnsCompletion = true`, the whole-program
  * fixed point over "can this algorithm itself abruptly complete" — see
  * [[CompletionAlgorithms]]) to run it on, same division of labor as that other
  * pair. [[AddBuiltinBehaviourPass]] does *not* call this directly, despite
  * seeding `returnsCompletion = true` unconditionally for every
  * `isBuiltinBehaviour` closure (mainline's own `esmeta.compiler.Compiler.
  * compile`'s `Builtin => needRetComp = true` — a calling-convention
  * requirement, not a "can it throw" one): it runs well before
  * `MarkCompletionAlgorithmsPass` even stamps that flag (see
  * `AddBuiltinBehaviourPass`'s own doc), so a closure it creates just reaches
  * `WrapCompletionReturnsPass` later looking like any other algorithm with
  * `returnsCompletion = true` — `WrapCompletionReturnsPass` really is this
  * object's one and only caller today.
  */
object CompletionWrapping:

  /** Generates this object's `_err`/`_v`/`_ret` names for a single algorithm.
    * Scoped as a value local to each [[expandAlgorithm]] call rather than a
    * mutable field on this `object` — the latter is JVM-wide singleton state,
    * so concurrent callers (e.g. multiple ScalaTest suites compiling algorithms
    * in parallel, which sbt's default `Test / parallelExecution` allows) would
    * race on incrementing/resetting a shared counter, producing
    * nondeterministic naming depending on thread interleaving.
    */
  private class Counter:
    private var n = 0
    def freshErr(): String = { n += 1; s"_err$n" }
    def freshVal(): String = { n += 1; s"_v$n" }
    def freshRet(): String = { n += 1; s"_ret$n" }

  /** Entry point for a caller's per-`Algorithm` pass over `expand`: creates a
    * fresh `Counter` so fresh names start over at each algorithm instead of
    * accumulating across the whole program (`expand` itself also recurses into
    * nested bodies, e.g. `Return(None, body)`'s `body` — those inner calls
    * reuse the same `Counter` passed down from here, rather than each getting
    * their own).
    *
    * `pureCallees`/`alwaysCompletionCallees` (both normalized, lowercased
    * names) let this object's one caller, [[WrapCompletionReturnsPass]] — which
    * runs well after [[MarkCompletionAlgorithmsPass]]'s fixed point, see this
    * class's own doc — tell this pass about *other* WJI algorithms whose own
    * completion-ness is already settled one way or the other: `pureCallees`
    * names every algorithm proven to never itself return a Completion Record
    * (`returnsCompletion = false`), `alwaysCompletionCallees` every algorithm
    * proven to always return one (`returnsCompletion = true`, the exact
    * complement — see [[CompletionAlgorithms]]'s own doc for what that fixed
    * point actually establishes either way). Both default to empty for any
    * future caller that can't supply them.
    */
  def expandAlgorithm(
    instrs: List[Instr],
    pureCallees: Set[String] = Set.empty,
    alwaysCompletionCallees: Set[String] = Set.empty,
  ): List[Instr] =
    expand(
      instrs,
      Counter(),
      pureCallees = pureCallees,
      alwaysCompletionCallees = alwaysCompletionCallees,
    )

  /** Hardcoded mainline ECMA-262 operations confirmed, directly against their
    * own spec text/implementation rather than anything `pureCallees` can derive
    * (they aren't WJI `Algorithm`s at all -- no `returnsCompletion` fixed point
    * covers them, same as `docs/hardcodes.md`'s own category of knowledge this
    * pipeline can't derive from the spec on its own), to never produce a
    * Completion Record:
    *
    *   - `converted_to_a_javascript_value` (see `WebIdlConversion.scala`'s own
    *     doc: "can't throw either ... and was never affected by the
    *     completion-wrapping question"). Its sibling
    *     `converted_to_an_idl_value` is deliberately *not* here: that one
    *     genuinely can be abrupt (a required dictionary member found absent, an
    *     invalid enum value, a getter that itself throws), unlike this one.
    *   - `CreateArrayFromList` (`js-api`'s own `[$CreateArrayFromList$]
    *     (|values|)` call, e.g. `call_an_exported_function`'s final `Return`)
    * -- `ecma262/spec.html#sec-createarrayfromlist`'s own return-type
    * annotation is plain `an Array`, never "a normal completion containing ...
    * or a throw completion" the way an abrupt operation's signature always is,
    * and its three-step body (`! ArrayCreate(0)` then a loop of `!
    * CreateDataPropertyOrThrow`) backs that up -- every step inside is itself
    * `!`-asserted non-abrupt, so there is no path to an abrupt completion to
    * assert away in the first place.
    */
  private val neverCompletionNatives: Set[String] =
    Set("converted_to_a_javascript_value", "createarrayfromlist")

  /** Whether a `Perform`/`PerformClosure` call target `func` (still in its
    * original, possibly-bracketed link-text form -- see
    * [[CompletionAlgorithms.normalize]]'s own doc) is known, one way or the
    * other, to never itself produce a Completion Record.
    */
  private def isProvablyPure(func: String, pureCallees: Set[String]): Boolean =
    val name = CompletionAlgorithms.normalize(func)
    pureCallees.contains(name) || neverCompletionNatives.contains(name)

  /** The mirror image of [[isProvablyPure]]: whether `func` is a WJI algorithm
    * already proven to *always* return a Completion Record. No native-routine
    * allowlist here the way [[neverCompletionNatives]] backs the pure side --
    * `converted_to_an_idl_value` (the one native conversion that can genuinely
    * be abrupt) returns a *plain* value on its ordinary success path, only a
    * real `ThrowCompletion` on failure (see `WebIdlConversion.scala`'s own
    * doc), so it's dual-shaped rather than always-wrapped and doesn't qualify
    * either way -- "always a Completion Record no matter what" only actually
    * holds for a WJI `Algorithm` [[WrapCompletionReturnsPass]] itself wrapped.
    */
  private def isProvablyCompletion(
    func: String,
    alwaysCompletionCallees: Set[String],
  ): Boolean =
    alwaysCompletionCallees.contains(CompletionAlgorithms.normalize(func))

  /** `expr`'s own constructor already fixes its compiled runtime type to
    * something other than a Completion Record, regardless of what any nested
    * sub-expression evaluates to -- so a `Cond.IsType(_, "Completion")` check
    * on it can never be true. Deliberately narrow (a fixed list of
    * constructors, not a general "does this ever produce a completion"
    * analysis) and conservative: every one of these compiles straight to a
    * definite ECMAScript/Math/List/record value (see `Compiler`'s own cases for
    * `Bool`/`Num`/`Str`/`Byte`/`AsMath`/`List_`/`RecordLit`, and
    * `SpecTerm("true"|"false"|"null"|"undefined")`'s direct
    * `EBool`/`ENull`/`EUndef` compilation).
    *
    * `RecordLit(tname, _)`/`New(iface)` are safe for the same reason a
    * `pure`-tracked `Var` bound to one survives arbitrary intervening calls
    * (see [[expand]]): `esmeta.ty.CompT` (the type `Cond.IsType(_,
    * "Completion")` tests against) is `RecordTy("CompletionRecord")`, keyed on
    * the record's own *type tag* set once at construction, never on its field
    * contents -- no amount of `obj.Field = value` mutation after the fact can
    * turn a `RecordLit("Global" | "Memory" | "Table" | "Tag" | "Exception" |
    * "Module" | ..., _)` into one tagged `"CompletionRecord"`. Confirmed no
    * occurrence of `RecordLit` in this corpus ever uses a completion-shaped
    * tname -- real completions are always produced via
    * `NormalCompletion`/`ThrowCompletion` calls, never written as a record
    * literal. `Expr.New(iface)` ("a [=/new=] {{Iface}} object"/"a {{Iface}}
    * exception") compiles exactly the same way (`Compiler`'s own case:
    * `ERecord(iface, ordinaryObjectFields(iface))`) -- `iface` ranges over real
    * interfaces (`Module`/`Instance`/...) *and* WebIDL `<dfn exception>`s
    * (`CompileError`/...), but either way it's the bare error/interface object
    * itself, never yet wrapped in any completion (that happens separately, see
    * `Instr.Throw`'s own two cases above).
    *
    * `Expr.Case`/`Expr.Tuple` compile to `ECase`/`ETup` (`Compiler`'s own
    * cases) -- a `Wasm(CaseV(...))`/`Wasm(TupV(...))` value, a structurally
    * distinct `esmeta.state.Value` variant from any `RecordObj`, completion or
    * not -- so these are safe regardless of their own `args`/`elems`' purity,
    * same as `List_`. `Expr.AsWasm(inner, _)` ("X as a WebAssembly u64")
    * compiles to `inner` completely unchanged (`Compiler`'s own doc: "the
    * annotation carries no information this pipeline needs to act on"), so it
    * defers to `inner`'s own `neverCompletion` verdict rather than adding a new
    * one -- this is what actually lets `addressvaluetou64`'s `Return(Some
    * (AsWasm(AsMath(Var(n)), "u64")))` match here, not a special case of its
    * own.
    *
    * `Expr.Field(_, "Value")` is safe regardless of the base expression or how
    * it was computed -- not because of anything about the base, but because
    * Completion Records never nest in this entire system: a `NormalCompletion`/
    * `ThrowCompletion`'s own `[[Value]]` is, by construction, always the real
    * payload itself, never *another* Completion Record one level further in.
    * Checked against every actual `Expr.Field(_, "Value")` site this pipeline
    * ever compiles a WJI algorithm body to contain -- `ExpandAbruptPass`'s own
    * `?`/`!` unwrap, `ExpandThrowsPass`'s catch-idiom unwrap,
    * `AddInterfaceMemberBuiltinBehaviourPass`'s `converted_to_an_idl_value`
    * abrupt-value extraction -- all three pull a Completion Record's payload,
    * never anything else; `ExprParser`'s "the max value in X" idiom compiles
    * the *same* field name onto a `limits` record's `maximum` `Opt` wrapper
    * instead (a different, unrelated use of the name "Value"), but that payload
    * is just as plainly never completion-shaped (a `u64`). No occurrence of
    * this field name anywhere in the corpus holds a completion-shaped value.
    */
  private def neverCompletion(expr: Expr): Boolean = expr match
    case _: Expr.Bool | _: Expr.Num | _: Expr.Str | _: Expr.Byte => true
    case Expr.SpecTerm("true" | "false" | "null" | "undefined")  => true
    case _: Expr.AsMath                                          => true
    case _: Expr.List_                                           => true
    case _: Expr.RecordLit                                       => true
    case _: Expr.New                                             => true
    case _: Expr.Case                                            => true
    case _: Expr.Tuple                                           => true
    case Expr.Field(_, "Value")                                  => true
    case Expr.AsWasm(inner, _) => neverCompletion(inner)
    case _                     => false

  /** `cond`, as an `IfChain` branch's own condition, already proves `e`
    * [[IsType is a Completion/AbruptCompletion]] for every instruction inside
    * that branch's body -- most notably [[ExpandAbruptPass]]'s own
    * `?`-expansion, whose branch is *exactly* `(Cond.IsType(inner,
    * "AbruptCompletion"), [Return(inner)])`. Recognized here (rather than only
    * at the `Return` site) so [[expand]] can thread the fact down to that
    * `Return` as it recurses into the branch.
    */
  private def provenCompletion(cond: Cond): Set[Expr] = cond match
    case Cond.IsType(e, "AbruptCompletion" | "Completion", false) => Set(e)
    case _                                                        => Set.empty

  private def expand(
    instrs: List[Instr],
    counter: Counter,
    known: Set[Expr] = Set.empty,
    pure: Set[Expr] = Set.empty,
    pureCallees: Set[String] = Set.empty,
    alwaysCompletionCallees: Set[String] = Set.empty,
  ): List[Instr] =
    // a left-to-right fold, not a plain `flatMap` with a single shared
    // `known`/`pure` -- a `Let`/`Set` reassigning one of their vars partway
    // through `instrs` must stop that var counting as proven-completion/
    // proven-pure for every instruction *after* it (even though no actual
    // pass currently reassigns a var `provenCompletion` tracked --
    // ExpandAbruptPass's own branch body is always the single instruction
    // `[Return(inner)]` -- this keeps that invariant enforced structurally
    // rather than merely assumed). `pure` *gains* a var the moment a `Let`
    // *or* `Set` binds it to a `neverCompletion` expr (e.g. `Let(global,
    // RecordLit("Global", _))`, or `Set(This, New(iface))` -- the latter is
    // exactly how `AddInterfaceMemberBuiltinBehaviourPass.createThisBinding`
    // seeds every WebIDL constructor's `**this**`, a reassignment of an
    // existing param rather than a fresh `Let`, but `neverCompletion(rhs)`
    // already proves the same thing about the *value* regardless of which
    // instruction wrote it) or a `Perform` binds it to the result of a
    // provably-pure callee (see [[isProvablyPure]]); `known` *also* gains a
    // var now, the mirror case -- a `Perform` binding the result of a callee
    // already proven to *always* return a Completion Record (see
    // [[isProvablyCompletion]]) -- on top of its original source (an
    // enclosing `IfChain`'s own condition, see [[provenCompletion]]).
    // `pureCallees`/`alwaysCompletionCallees` themselves never change across
    // the fold -- just threaded through unchanged, same as into every
    // recursive `expand` call in `expandInstr` below.
    instrs
      .foldLeft((List.empty[Instr], known, pure)) {
        case ((acc, k, p), instr) =>
          val clearedP = invalidate(p, instr)
          val nextP = instr match
            case Instr.Let(lhs, rhs, _) if neverCompletion(rhs) =>
              clearedP + lhs
            case Instr.Set(lhs, rhs, _) if neverCompletion(rhs) =>
              clearedP + lhs
            case Instr.Perform(func, _, PerformOutcome.BindResult(v), _, _)
                if isProvablyPure(func, pureCallees) =>
              clearedP + Expr.Var(v)
            case _ => clearedP
          val clearedK = invalidate(k, instr)
          val nextK = instr match
            case Instr.Perform(func, _, PerformOutcome.BindResult(v), _, _)
                if isProvablyCompletion(func, alwaysCompletionCallees) =>
              clearedK + Expr.Var(v)
            case _ => clearedK
          (
            acc ++ expandInstr(
              instr,
              counter,
              k,
              p,
              pureCallees,
              alwaysCompletionCallees,
            ),
            nextK,
            nextP,
          )
      }
      ._1

  /** Every name `instr` itself rebinds -- `Let`/`Set`'s own `lhs`,
    * `Perform`/`PerformClosure`'s `PerformOutcome.BindResult(name)` (e.g. a
    * `call-convert`'s `name = <converted_to_a_javascript_value>(name)`, which
    * reassigns `name` in place), and `Pop`'s `lhs`. Recurses into `instr`'s own
    * nested body -- needed since `expand`'s fold only ever sees sibling
    * instructions at one level: a `While`/`ForEach`/... loop body that
    * reassigns a var partway through (e.g. `js-string-fromcharcodearray`'s `let
    * result = ""` before the loop, then `let result = (concat result
    * charCodeString)` *inside* it) must still invalidate that var for whatever
    * comes *after* the whole loop, even though the loop itself is a single
    * opaque sibling as far as the outer fold is concerned. `IfChain` needs its
    * own case since its `body` is always `Nil` (its real nested instructions
    * live in `branches`/`fallback` instead, see its own doc). Over-collecting
    * here (treating a name as "reassigned" when it's actually a fresh,
    * branch-local `Let` that only shadows outer scope) is harmless -- it just
    * forgoes an optimization, never produces a wrong one -- so this never needs
    * to distinguish the two.
    */
  private def reassignedVars(instr: Instr): Set[Expr] =
    val direct: Set[Expr] = instr match
      case Instr.Let(lhs, _, _) => Set(lhs)
      case Instr.Set(lhs, _, _) => Set(lhs)
      case Instr.Perform(_, _, PerformOutcome.BindResult(v), _, _) =>
        Set(Expr.Var(v))
      case Instr.PerformClosure(_, _, PerformOutcome.BindResult(v), _) =>
        Set(Expr.Var(v))
      case Instr.Pop(lhs, _, _) => Set(lhs)
      case _                    => Set.empty
    val nested = instr match
      case i: Instr.IfChain =>
        i.branches.flatMap((_, b) => b.flatMap(reassignedVars)).toSet
        ++ i.fallback.flatMap(reassignedVars)
      case _ => instr.body.flatMap(reassignedVars).toSet
    direct ++ nested

  private def invalidate(known: Set[Expr], instr: Instr): Set[Expr] =
    known -- reassignedVars(instr)

  private def expandInstr(
    instr: Instr,
    counter: Counter,
    known: Set[Expr],
    pure: Set[Expr],
    pureCallees: Set[String],
    alwaysCompletionCallees: Set[String],
  ): List[Instr] =
    instr match
      // a "the following steps..." closure's substeps are a separate
      // algorithm-to-be (see CompletionAlgorithms) — its own Returns are none
      // of this algorithm's business to wrap. (A no-op by the time
      // AddBuiltinBehaviourPass calls this: no FollowingSteps remain anywhere
      // in the program past ExpandFollowingStepsPass.)
      case i @ Instr.Let(_, Expr.FollowingSteps(_, _), _) => List(i)
      case Instr.Throw(Expr.New(iface), _) =>
        val err = counter.freshErr()
        val ret = counter.freshRet()
        List(
          Instr.Perform(
            "__NEW_ERROR_OBJ__",
            List(Expr.Str(s"%$iface.prototype%")),
            PerformOutcome.BindResult(err),
          ),
          Instr.Perform(
            "ThrowCompletion",
            List(Expr.Var(err)),
            PerformOutcome.BindResult(ret),
          ),
          Instr.Return(Some(Expr.Var(ret))),
        )
      // target is already a computed value (e.g. a ToJSValue(...) call, or a
      // bound variable holding an already-constructed exception object) --
      // unlike the Expr.New case above, there's nothing to construct, so wrap
      // it in ThrowCompletion directly.
      case Instr.Throw(target, _) =>
        val ret = counter.freshRet()
        List(
          Instr.Perform(
            "ThrowCompletion",
            List(target),
            PerformOutcome.BindResult(ret),
          ),
          Instr.Return(Some(Expr.Var(ret))),
        )
      // `expr` is already proven to be a Completion by an enclosing branch's
      // own condition (see `provenCompletion`) -- returning it needs no
      // further check or wrap, it already *is* the exact Completion Record to
      // propagate.
      case Instr.Return(Some(expr), _) if known.contains(expr) =>
        List(Instr.Return(Some(expr)))
      // expr's own shape already guarantees it can never be a Completion
      // Record -- e.g. a literal `false`/`undefined`, or `|x| interpreted as a
      // mathematical value` always yields a Math value regardless of `x` --
      // so the usual "is this already a completion" runtime check would be
      // permanently dead code (confirmed by esmeta.wji.WjiCoverage against the
      // official corpus: every occurrence of this shape had its `then` branch
      // at 0 hits, e.g. the nine `[[...]] internal method of an exported GC
      // object` algorithms, personal/coverage-gaps.md). Skipping the check
      // entirely avoids generating it in the first place, rather than
      // emitting then eliminating it with a separate pass. `pure.contains
      // (expr)` is the same proof, just reached indirectly: `expr` is a `Var`
      // most recently bound (in this same flat instruction list) to a
      // `neverCompletion` expr, with no reassignment since -- e.g. `Let(global,
      // RecordLit("Global", _)); Perform("initialize_a_global_object", [global,
      // ...], BindResult(_)); Return(global)` (the intervening call only
      // mutates `global`'s *fields*, never its type tag, so the proof survives
      // it -- see `neverCompletion`'s own doc on `RecordLit`).
      case Instr.Return(Some(expr), _)
          if neverCompletion(expr) || pure.contains(expr) =>
        val ret = counter.freshRet()
        List(
          Instr.Perform(
            "NormalCompletion",
            List(expr),
            PerformOutcome.BindResult(ret),
          ),
          Instr.Return(Some(Expr.Var(ret))),
        )
      case Instr.Return(Some(expr), _) =>
        // a bare Var (the common case now that ExpandInlineAlgoCallPass/
        // ExpandPerformReturnResultPass already ran) needs no re-binding —
        // testing/returning it directly, rather than aliasing it under a fresh
        // name first, avoids a pointless extra Let.
        val (bindings, vVar) = expr match
          case v: Expr.Var => (Nil, v)
          case _ =>
            val v = counter.freshVal()
            val vVar = Expr.Var(v)
            (List(Instr.Let(vVar, expr)), vVar)
        val ret = counter.freshRet()
        bindings ++ List(
          Instr.IfChain(
            branches = List(
              (
                Cond.IsType(vVar, "Completion"),
                List(Instr.Return(Some(vVar))),
              ),
            ),
            fallback = List(
              Instr.Perform(
                "NormalCompletion",
                List(vVar),
                PerformOutcome.BindResult(ret),
              ),
              Instr.Return(Some(Expr.Var(ret))),
            ),
          ),
        )
      case Instr.Return(None, body) =>
        val ret = counter.freshRet()
        List(
          Instr.Perform(
            "NormalCompletion",
            List(Expr.SpecTerm("unused")),
            PerformOutcome.BindResult(ret),
          ),
          Instr.Return(
            Some(Expr.Var(ret)),
            expand(
              body,
              counter,
              known,
              pure,
              pureCallees,
              alwaysCompletionCallees,
            ),
          ),
        )
      // Not handled via the generic `mapBody` catch-all below (unlike every
      // other `Instr`) -- `mapBody` applies one function uniformly to every
      // branch, with no way to give each branch its own, condition-derived
      // `known` set.
      case i: Instr.IfChain =>
        List(
          i.copy(
            branches = i.branches.map((c, b) =>
              (
                c,
                expand(
                  b,
                  counter,
                  known ++ provenCompletion(c),
                  pure,
                  pureCallees,
                  alwaysCompletionCallees,
                ),
              ),
            ),
            fallback = expand(
              i.fallback,
              counter,
              known,
              pure,
              pureCallees,
              alwaysCompletionCallees,
            ),
          ),
        )
      case other =>
        List(
          other.mapBody(
            expand(
              _,
              counter,
              known,
              pure,
              pureCallees,
              alwaysCompletionCallees,
            ),
          ),
        )
