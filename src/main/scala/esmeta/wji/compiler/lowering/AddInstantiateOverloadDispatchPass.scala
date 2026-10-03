package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, AlgorithmKind, Cond, Expr, Instr, WjiParam}

/** `WebAssembly.instantiate` is WebIDL-overloaded (a buffer-source-first and a
  * module-first variant, `spectec/document/js-api/index.bs:653`/`660`) — a
  * shape `AlgorithmExtractor`/`Compiler` have no general support for (two
  * `Algorithm`s with the same `name`/`kind` collide on the same compiled
  * function name, `esmeta.cfg.CFG.fnameMap`'s `.toMap` silently keeping only
  * the later one; `personal/TODO.md` #58). `SpecPatch` #3 already renames both
  * dfns (`instantiate_bytes`/`instantiate_object`) purely to stop them from
  * colliding with each other, freeing the literal `instantiate` name entirely.
  *
  * This pass reclaims that name for one hand-built, `WebAssembly.instantiate`
  * -specific dispatcher `Algorithm` — deliberately not a general WebIDL
  * overload-resolution mechanism (`docs/hardcodes.md` #23): it just checks
  * whether `ArgumentsList[0]` [[Cond.Implements]] `Module` and forwards
  * (unconditionally, no re-validation of its own) to whichever real overload
  * matches — the same distinguishing check real WebIDL overload resolution
  * would make for this `(Module or BufferSource)`-shaped ambiguity, since
  * `Module` is a real interface type and every non-`Module` argument is
  * `instantiate_bytes`'s problem to reject correctly (already handled by its
  * own `AllowSharedBufferSource` conversion, `TODO.md` #57's reject-not-throw
  * fix included).
  *
  * Runs right after [[AddInterfaceMemberBuiltinBehaviourPass]], once both real
  * overloads already have the final `(this, ArgumentsList)`
  * `<BUILTIN>:` calling convention and their final, non-colliding
  * `INTRINSICS.WebAssembly.instantiate_bytes`/`instantiate_object` names — this
  * pass's own synthetic `Algorithm` is hand-built directly in that same
  * already-final shape (mirrors [[AddJsStringBuiltinsPass]]'s own "append a
  * synthesized `Algorithm`" pattern), so it needs no reshaping of its own and
  * is never itself seen by `AddInterfaceMemberBuiltinBehaviourPass`.
  * `Instr.Perform`'s `func` field isn't restricted to `[=link=]`/`[$jscall$]`
  * syntax — a bare string that already equals a real registered `Func` name (as
  * both of these now are) resolves directly (`Compiler.nameFromLink` is a no-op
  * on an already-bare string; `Interpreter.EClo` looks it up in `cfg.fnameMap`
  * by that exact string) — so forwarding is a plain cross-function call, no
  * closure/capture machinery needed.
  *
  * `0 < |ArgumentsList|` is checked before ever reading `ArgumentsList[0]` —
  * unlike [[Cond.Implements]] itself (safely `false` for any non-`Addr` value),
  * out-of-range indexing on `ArgumentsList` throws `InvalidObjField` rather
  * than gracefully reading as `undefined` (mirrors every other
  * `ArgumentsList[i]` read in [[AddInterfaceMemberBuiltinBehaviourPass]], e.g.
  * its own `givenValueBinding`). A zero-argument call falls to
  * `instantiate_bytes` — an arbitrary choice, harmless since both overloads'
  * first parameter is required, so either one correctly rejects with a
  * `TypeError` promise either way (`TODO.md` #57).
  *
  * `manuals/intrinsics`'s `instantiate: [TTT] #INTRINSICS.WebAssembly.
  * instantiate;` descriptor and its `length: 1` override are both keyed by this
  * final intrinsic name, not by which `Algorithm` produced it, so this pass's
  * synthetic dispatcher inherits both automatically — nothing to change there.
  *
  * Requires:
  *   - [[AddInterfaceMemberBuiltinBehaviourPass]]: needs both real overloads
  *     already reshaped into the `(this, ArgumentsList)` calling
  *     convention, under their final, non-colliding `INTRINSICS.WebAssembly.*`
  *     names, before this pass's forwarding calls can reference them.
  *
  * Category: Structural desugaring — Injection.
  */
object AddInstantiateOverloadDispatchPass extends LoweringPass:

  override def requires: Set[LoweringPass] =
    Set(AddInterfaceMemberBuiltinBehaviourPass)

  private val BuiltinParams =
    List(
      WjiParam("|this|"),
      WjiParam("|ArgumentsList|"),
    )

  private def forward(fname: String): List[Instr] =
    List(
      Instr.Perform(
        fname,
        List(Expr.This, Expr.Var("ArgumentsList")),
        Instr.PerformOutcome.ReturnResult,
      ),
    )

  private val dispatcher: Algorithm =
    Algorithm(
      id = None,
      name = Some("instantiate"),
      params = BuiltinParams,
      head =
        "<synthesized by AddInstantiateOverloadDispatchPass — TODO.md #58>",
      body = List(
        Instr.IfChain(
          List(
            Cond.And(
              Cond.Compare(
                Expr.Num("0"),
                Cond.CompareOp.Lt,
                Expr.Length(Expr.Var("ArgumentsList")),
              ),
              Cond.Implements(
                Expr.Index(Expr.Var("ArgumentsList"), Expr.Num("0")),
                Expr.SpecTerm("Module"),
              ),
            ) -> forward("INTRINSICS.WebAssembly.instantiate_object"),
          ),
          forward("INTRINSICS.WebAssembly.instantiate_bytes"),
        ),
      ),
      kind = AlgorithmKind.NamespaceMethod("WebAssembly"),
    )

  def run(algos: List[Algorithm]): List[Algorithm] = algos :+ dispatcher
