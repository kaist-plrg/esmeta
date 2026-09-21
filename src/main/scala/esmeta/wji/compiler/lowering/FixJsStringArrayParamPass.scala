package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}

/** Fixes up everything `fromCharCodeArray`/`intoCharCodeArray` need to actually
  * work with their `|array|` parameter's raw Wasm GC array reference
  * (`Case("REF.ARRAY_ADDR", [arrayaddr])`, per
  * `AddBuiltinFunctionHostfuncPass`'s own argument-conversion loop, which
  * deliberately leaves this one parameter unconverted — see that pass's own
  * doc) — both directions, in one pass:
  *
  *   - '''rebinding''' `|array|`, right after each algorithm's existing "if
  * |array| is null" check, from that raw reference into the actual list of
  * element values it addresses — `|store|.ARRAYS[|arrayaddr|].FIELDS`, exactly
  * the field the real Wasm Core Spec's own `ARRAY.LEN`/`ARRAY.GET`/`ARRAY.SET`
  * reduction rules read/write (`4.3-execution.instructions.spectec`,
  * `$arrayinst(z)[a].FIELDS`). Built directly as `Expr.Field`/`Expr.Index`
  * nodes rather than a `SpecPatch` (unlike `AddBuiltinFunctionHostfuncPass`'s
  * own `[=ToJSValue=]`/plain-English argument loop): the Wasm Core Spec's own
  * embedding appendix (`core/appendix/embedding.rst`) defines prose-level
  * relations for every other store-addressed collection this corpus reads this
  * way (`table_read`, `global_read`, `mem_read`) but none for GC arrays at all,
  * since those postdate the embedding appendix's own last update
  * (`docs/spec_errors.md` #36) — no interpreter change needed for *this*
  * direction though: `store` is already a plain, fully-marshalled
  * `ALValue.StrV` on the WJI side by the time a hostfunc call runs (confirmed
  * by `esmeta.interpreter.WasmMemoryBridge`'s own direct `ARRAYS`/`MEMS`-field
  * navigation of exactly this same `store` value, for the same kind of
  * cross-boundary sync), so the same *generic* mechanism
  * `esmeta.state.State.apply` already uses for every other Wasm struct/list
  * field/index read (`Wasm(ALValue.StrV(fields)) => applyFields(fields,
  * field)`, `Wasm(ALValue.ListV(vs)) => apply(vs, field)`) already covers it.
  *
  * `intoCharCodeArray` only ever reads `|array|` as a whole (`the number of
  * elements in |array|`, for its own bounds check) — a plain rebind is enough.
  * `fromCharCodeArray` additionally reads *individual elements* (`the value of
  * the element stored at index |i| in |array|`) and hands each straight to
  * `[$FromCharCode$]`, which — like every other js-string abstract op after
  * `docs/spec_errors.md` #32's fix — expects an already-JS-domain value. But an
  * *element* of this `(mut i16)` array's `FIELDS` isn't a plain wasm number the
  * way a `CONST`-wrapped hostfunc argument is — the Wasm Core Spec packs a
  * narrow (`i8`/`i16`) storage type's elements (`construct.ml`'s
  * `al_to_packtype`/ `Aggr.PackField`), so each one is `Case("PACK",
  * [Case("I16", []), payload])`, confirmed directly via a step-log dump
  * (`wasm<CaseV(PACK,List(CaseV(I16,List()), NumV(Nat(0))))>`). So
  * `fromCharCodeArray` gets a *converted* copy instead of a plain rebind —
  * unwrapping each element's `PACK` payload and running it through the same
  * `[=𝔽=](... interpreted as a [=mathematical value=])` bridge
  * `docs/spec_errors.md` #33/#35's other fixes already use to get from a raw
  * wasm number to a genuine JS Number — so the rest of the algorithm (a plain
  * `array[i]` read) transparently sees already-converted values, no further
  * per-call-site fix needed.
  *
  *   - '''rewriting the write''': `intoCharCodeArray`'s "Set the element at
  *     index X in |array| to Y" step (by the time this pass runs, already
  *     hoisted to a plain `Instr.Set(Expr.Index(Var("array"), X), Y)` — see
  *     this pass's own pipeline position below) into a call to the new
  *     `array_write` embedding function
  *     (`spectec/spectec/src/backend-interpreter/embedding.ml`) instead of a
  *     literal `Set`. Unlike the read direction above, `esmeta.state.
  *     Value.asAddr`/`asList` require a genuine heap `Addr` to write through —
  *     the rebound `|array|` is a plain, immutable `Wasm(ALValue.ListV(...))`,
  *     not one. Nor can WJI build a *replacement* `store`/`arrayinst` value of
  *     its own to reassign instead: both are named-field Wasm structs
  *     (`ALValue.StrV`), and the only Wasm-value constructor WJI's compiler has
  *     is `Expr.Case`/`ECase` — positional (`ALValue.CaseV`), not named-field.
  *     `array_write` closes this the same way
  *     `table_write`/`global_write`/`mem_write` already do for every *other*
  *     store-addressed collection (`docs/spec_errors.md` #36) — it mutates the
  *     real store on the OCaml side and hands back the (same, now-updated)
  *     store value, which this pass immediately reassigns to `[=surrounding
  *     agent=]'s [=associated store=]` so `create_a_builtin_function_hostfunc`
  *     (which re-reads that same field right before returning) picks it up.
  *     Takes an already-packed `fieldval` (`Case("PACK", [Case("I16", []),
  *     payload])`, matching this array's own `(mut i16)` element type — the
  *     only element type any current js-string builtin uses) — mirrors
  *     `table_write`'s own `ref` argument, which likewise crosses the boundary
  *     already in its final wasm-value form, no conversion inside `array_write`
  *     itself. `Y` here is always `[=ToWebAssemblyValue=](|charCode|,
  *     [=i32=])`'s own already-hoisted result, `Case("CONST", [Case("I32", []),
  *     payload])` — its `payload` (index 1) is what needs repacking; the
  *     `I32`/`CONST` wrapper itself is discarded. Only the write direction
  *     needs `array_write` — reads (`sizeof`, indexed reads) already work
  *     generically off the plain `Wasm(ListV(...))` this same pass rebinds
  *     `|array|` to, no RPC round trip needed there.
  *
  * A genuinely-null `|array|` never reaches any of this at all (both algorithms
  * `return`/throw on their own "if |array| is null" check first) — and even if
  * it did, `AddBuiltinFunctionHostfuncPass` converts a `ref.null` argument via
  * `ToJSValue` like any other externref (only `REF.ARRAY_ADDR` itself is
  * exempted), so that null check already sees a genuine JS `null`, not this
  * pass's concern.
  *
  * Runs dead last in [[Lowering.pipeline]] (after `NormalizeAlgoNamePass`) —
  * not because the rebind half needs it (it would work at any position, a pure
  * prepend of fresh `Let`s that nothing upstream reads), but because the
  * write-rewrite half does: it needs the target `Instr.Set` already in its
  * final, hoisted shape (a bare `Expr.Index(Var("array"), _)` lhs with a
  * plain-`Var` rhs) — the shape `ExpandAbruptPass`/`WrapCompletionReturnsPass`
  * (mid-pipeline) produce, not the raw, still-abrupt-marked call `ExprParser`
  * parses `[=ToWebAssemblyValue=](...)` into. Kept as one pass rather than two
  * (an earlier version split the rebind out to run alongside
  * `AddJsStringBuiltinsPass`) since there was no actual reason for the split —
  * both fixes fit the same "patch these two algorithms' bodies" shape, and the
  * write-rewrite's late placement works just as well for the rebind.
  *
  * Category: Structural desugaring — Injection.
  */
object FixJsStringArrayParamPass extends LoweringPass:

  private val FromCharCodeArray = "js-string-fromCharCodeArray"
  private val IntoCharCodeArray = "js-string-intoCharCodeArray"

  private val store = Expr.Var("store")
  private val arrayAddr = Expr.Var("arrayaddr")
  private val rawFields = Expr.Var("_rawFields")
  private val loopIdx = Expr.Var("_j")
  private val agentStore =
    Expr.Field(Expr.SpecTerm("surrounding agent"), "associated store")

  private def bindStoreAndAddr: List[Instr] =
    List(
      Instr.Let(store, agentStore),
      Instr.Let(arrayAddr, Expr.TupleProj(Expr.Var("array"), 0)),
    )

  private def arrayFields: Expr =
    Expr.Field(Expr.Index(Expr.Field(store, "ARRAYS"), arrayAddr), "FIELDS")

  /** `intoCharCodeArray`: a plain rebind, no per-element conversion needed. */
  private def rebindArrayPlain: List[Instr] =
    bindStoreAndAddr :+ Instr.Let(Expr.Var("array"), arrayFields)

  /** `fromCharCodeArray`: rebind to a *converted* copy, unpacking each
    * `Case("PACK", [numtype, payload])` element into a genuine JS Number.
    */
  private def rebindArrayConverted: List[Instr] =
    bindStoreAndAddr ++ List(
      Instr.Let(rawFields, arrayFields),
      Instr.Let(Expr.Var("array"), Expr.List_(Nil)),
      Instr.Let(loopIdx, Expr.Num("0")),
      Instr.While(
        Cond.Compare(loopIdx, Cond.CompareOp.Lt, Expr.Length(rawFields)),
        List(
          Instr.Append(
            Expr.AsNumber(
              Expr.AsMath(
                Expr.TupleProj(Expr.Index(rawFields, loopIdx), 1),
              ),
            ),
            Expr.Var("array"),
          ),
          Instr.Set(
            loopIdx,
            Expr.BinOp(loopIdx, Expr.BOp.Add, Expr.Num("1")),
          ),
        ),
      ),
    )

  /** Rewrites `intoCharCodeArray`'s (by now fully hoisted) "Set the element at
    * index X in |array| to Y" into an `array_write` call — see this object's
    * own doc.
    */
  private def rewriteArrayWrite(instrs: List[Instr]): List[Instr] =
    instrs.flatMap {
      case Instr.Set(Expr.Index(Expr.Var("array"), idx), rhs, body) =>
        val packed =
          Expr.Case("PACK", List(Expr.Case("I16", Nil), Expr.TupleProj(rhs, 1)))
        List(
          Instr.Perform(
            "array_write",
            List(store, arrayAddr, idx, packed),
            Instr.PerformOutcome.BindResult("store"),
          ),
          Instr.Set(agentStore, store),
        ) ++ rewriteArrayWrite(body)
      case i => List(i.mapBody(rewriteArrayWrite))
    }

  private def prependRebind(
    body: List[Instr],
    rebind: List[Instr],
  ): List[Instr] =
    body match
      case head :: tail => head :: rebind ++ tail
      case Nil          => Nil

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map { a =>
      if a.id.contains(FromCharCodeArray) then
        a.copy(body = prependRebind(a.body, rebindArrayConverted))
      else if a.id.contains(IntoCharCodeArray) then
        a.copy(body =
          rewriteArrayWrite(prependRebind(a.body, rebindArrayPlain)),
        )
      else a
    }
