package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, AlgorithmKind, Cond, Expr, Instr, WjiParam}

/** Reshapes every Getter/Setter/Constructor/Method-kind [[Algorithm]] — all 4
  * kinds WebIDL calls an interface "member", per `webidl/index.bs`'s own
  * "Members" section ("The constructor steps, getter steps, setter steps, and
  * method steps ... have access to a this value") — into the `<BUILTIN>:`
  * calling convention mainline's own `Call`/`BuiltinCallOrConstruct` machinery
  * expects, the same two fix-ups [[AddBuiltinBehaviourPass]] applies to a
  * hoisted `CreateBuiltinFunction` closure, for the same reason (a
  * calling-convention requirement, not conditional on whether the algorithm
  * itself can abruptly complete):
  *
  * `Method` here is only ever a *real interface* member (`Table.get`,
  * `Global.valueOf`, ...) — `esmeta.wji.extractor.Extractor` already downgrades
  * any `Method(for)` whose `for` isn't in the extracted interfaces list to
  * `Plain` before lowering ever runs, so a *namespace* method
  * (`WebAssembly.instantiate`/`compile`/`validate`) never reaches this pass.
  * This isn't just a naming-collision workaround: `webidl/index.bs` itself
  * treats a namespace's own operations and an interface's members as products
  * of two genuinely different algorithms. "[=create a namespace object=]"
  * builds the namespace object directly (`OrdinaryObjectCreate` off
  * `%Object.prototype%`) and installs its operations straight onto *that*
  * object; "[=create an interface object=]"/"create an interface prototype
  * object" instead builds a *separate* interface prototype object, and it's
  * only that second object getter/setter/constructor/method properties ever
  * attach to — which is exactly the shape `unpackArgumentsList`/[[Compiler]]'s
  * `INTRINSICS.WebAssembly.<iface>.prototype.<name>` naming below assumes (a
  * namespace operation has no `.prototype` segment at all: `WebAssembly.
  * instantiate`, never `WebAssembly.prototype.instantiate`). WJI's own
  * interface-object mechanization (`ExpandNewInterfaceObjectPass`, `Compiler`'s
  * `Expr.New`/`namesWithPrototypeIntrinsic`) only ever implements "create an
  * interface object" — there's no "create a namespace object" mechanization at
  * all yet, so treating `WebAssembly`'s own operations the same way as an
  * interface's members would have been structurally wrong even before any bug
  * showed up. One did show up when this was tried anyway, confirming it
  * concretely rather than just in theory: `webidl/index.bs`'s exported term "a
  * new promise" (`Return [=?=] [$NewPromiseCapability$] (...)`) returns the raw
  * `PromiseCapabilityRecord`, not its `.[[Promise]]` — every namespace-level
  * algorithm following `Let |promise| be [=a new promise=]. ... Return
  * |promise|.` ends up returning the capability record instead of an actual
  * `Promise` unless something explicitly unwraps `.Promise` first, which the
  * hand-written `manuals/funcs/INTRINSICS. WebAssembly.instantiate.ir` glue
  * does and nothing generic does yet. See `docs/hardcodes.md` #7 and
  * `personal/TODO.md`.
  *
  *   - '''parameter unpacking''': `BuiltinCallOrConstruct` always invokes a
  *     builtin as `func.__CODE__(this, argumentsList, newTarget)` — a fixed
  *     3-argument shape — regardless of what parameters the algorithm itself
  *     declares (e.g. `Table.get(|index|)`, `Instance(|moduleObject|,
  * |importObject|)`). Every originally-declared `|param|` is unpacked from
  * `ArgumentsList` positionally, mirroring
  * [[AddBuiltinBehaviourPass.unpackArgumentsList]] (same shape, just reading
  * the capitalized `ArgumentsList`/`NewTarget`/`this` names this convention
  * uses — see [[BuiltinParams]] — instead of that pass's lowercase
  * `argumentsList`/`newTarget`/`thisArgument`, a different convention for
  * hoisted closures rather than top-level interface members). `**this**` itself
  * needs no such unpacking: it now compiles directly to the same local the
  * `|this|` parameter declares (see `esmeta.wji.compiler.Compiler`'s
  * `Expr.This` case), so simply declaring `|this|` as a parameter is already
  * enough to bind it — no `Set **this** to |this|.` prefix needed.
  *   - '''WebIDL's implicit setter argument''': a `Setter`-kind algorithm's
  *     `**the given value**` (`Expr.GivenValue`) is WebIDL's other implicit
  *     member-only binding, alongside `**this**` — unpacked from
  *     `ArgumentsList[0]` the same way, since (unlike every declared `|param|`)
  *     it was never a real Bikeshed `|pipe|` variable `extractParams` could
  *     have already found.
  *   - '''Completion-record wrapping''': every exit path must return a real
  *     Completion Record, same as [[AddBuiltinBehaviourPass]]'s own reason
  *     (mainline's Call machinery always expects one back, regardless of
  *     whether the algorithm itself can abruptly complete — see that pass's
  *     doc). Unlike that pass, this one *can* just reuse
  *     [[WrapCompletionReturnsPass]]'s work when it already ran — a member that
  *     transitively calls something abrupt (`Table.length`, `Global.value`) is
  *     already `returnsCompletion = true` and has already been wrapped by it;
  *     only one with no abrupt-completion signal of its own (e.g.
  *     `Instance.exports`, just `return **this**.\[[Exports]]`) still needs
  *     [[CompletionWrapping.expandAlgorithm]] called directly here, or it
  *     wouldn't be wrapped by anything at all.
  *
  * `esmeta.wji.compiler.Compiler.compileAlgo` handles the remaining, genuinely
  * compiler-level half — registering the result under the exact case-preserved
  * name `manuals/intrinsics` references for each kind (e.g.
  * `INTRINSICS.get:WebAssembly.Instance.prototype.exports`,
  * `INTRINSICS.set:WebAssembly.Global.prototype.value`, `INTRINSICS.
  * WebAssembly.Instance`) with `FuncKind.Builtin` — since naming/`FuncKind`
  * aren't things this metalang-level pipeline has any other reason to know
  * about.
  *
  * Category: Structural desugaring — Injection.
  */
object AddInterfaceMemberBuiltinBehaviourPass extends LoweringPass:

  /** Requires:
    *   - [[ExpandFollowingStepsPass]]/[[ExpandInlineAlgoCallPass]]: same reason
    *     [[AddBuiltinBehaviourPass]] needs them before calling the same
    *     [[CompletionWrapping.expandAlgorithm]] utility — its own `Return`
    *     handling assumes a call already sitting in `Instr.Perform` form, not a
    *     raw inline `Expr.AlgoCall`.
    *   - [[WrapCompletionReturnsPass]]: needs its wrapping already applied to a
    *     `returnsCompletion = true` member's body, so this pass can tell that
    *     case apart from one it still has to wrap itself (see class doc).
    */
  override def requires: Set[LoweringPass] = Set(
    ExpandFollowingStepsPass,
    ExpandInlineAlgoCallPass,
    WrapCompletionReturnsPass,
  )

  private val BuiltinParams =
    List(
      WjiParam("|this|"),
      WjiParam("|ArgumentsList|"),
      WjiParam("|NewTarget|"),
    )

  private def stripPipes(s: String): String =
    s.stripPrefix("|").stripSuffix("|")

  /** the `params.zipWithIndex` prefix instructions that unpack `ArgumentsList`
    * positionally into the algorithm's own originally-declared parameter names
    * — mirrors [[AddBuiltinBehaviourPass.unpackArgumentsList]] (see that
    * method's own doc); duplicated rather than shared since the two conventions
    * use differently-cased names for the list itself.
    *
    * Each param whose [[WjiParam.idlType]] is known (see
    * `esmeta.wji.extractor.Extractor.enrichParamTypes`) gets one more step
    * right after its own unpacking (nested inside the "argument actually
    * supplied" branch — see below), running the raw JS argument through
    * `converted_to_an_idl_value` — mirroring how WebIDL's own "overload
    * resolution algorithm" converts every ES argument to its declared IDL type
    * before the operation body ever runs. That function itself is still mostly
    * an identity stub (see `docs/hardcodes.md` #2) — only `"unsigned long"`
    * does a real conversion today — but routing every typed param through it
    * uniformly, rather than special-casing `"unsigned long"` here, means a
    * later type just needs a new case added there, not a change to this pass.
    *
    * When fewer arguments were actually supplied than this param's position,
    * [[WjiParam.optional]] decides what happens: for a genuinely optional
    * WebIDL parameter (`enrichParamTypes` OR's in the WebIDL declaration's own
    * `optional` keyword), the param is simply left unbound — not defaulted to
    * `undefined` — so `Cond.IsMissing`'s `"|X| is missing"` check
    * (`Compiler.compileExpr`'s `EExists`) correctly reports absence; spec text
    * for a real optional parameter always branches on exactly that check before
    * ever reading the value (WebIDL's own authoring convention — there is no
    * sensible unconditional default otherwise), so nothing downstream ever
    * needs to observe an "absent" param as a bound `undefined`. A *required*
    * param reaching this branch instead means WebIDL's own overload resolution
    * (which validates argument count before the operation's steps ever run) let
    * an arity mismatch through unnoticed — a genuine bug somewhere, not a case
    * worth padding over with a silent `undefined`, so it asserts unreachable
    * instead (mirrors a real "Assert: This step is not reached." in spec text,
    * [[Cond.Unreachable]]).
    */
  private def unpackArgumentsList(params: List[WjiParam]): List[Instr] =
    params.zipWithIndex.map {
      case (p, i) =>
        val name = stripPipes(p.name)
        val convert = p.idlType.toList.map { ty =>
          Instr.Perform(
            "converted_to_an_idl_value",
            List(Expr.Var(name), Expr.Str(ty)),
            Instr.PerformOutcome.BindResult(name),
          )
        }
        val supplied =
          Instr.Let(
            Expr.Var(name),
            Expr.Index(Expr.Var("ArgumentsList"), Expr.Num(i.toString)),
          ) :: convert
        val omitted =
          if p.optional then Nil else List(Instr.Assert(Cond.Unreachable))
        Instr.IfChain(
          List(
            Cond.Compare(
              Expr.Num(i.toString),
              Cond.CompareOp.Lt,
              Expr.Length(Expr.Var("ArgumentsList")),
            ) -> supplied,
          ),
          omitted,
        )
    }

  /** `**the given value**`'s binding, for a `Setter` only — WebIDL passes it as
    * the setter's sole argument, so it's `ArgumentsList[0]`, same shape as
    * [[unpackArgumentsList]] but for a name that was never a declared `|param|`
    * in the first place.
    */
  private def givenValueBinding(kind: AlgorithmKind): List[Instr] = kind match
    case AlgorithmKind.Setter(_) =>
      List(
        Instr.Let(
          Expr.Var("givenValue"),
          Expr.Index(Expr.Var("ArgumentsList"), Expr.Num("0")),
        ),
      )
    case _ => Nil

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map { a =>
      a.kind match
        case AlgorithmKind.Getter(_) | AlgorithmKind.Setter(_) |
            AlgorithmKind.Constructor(_) | AlgorithmKind.Method(_) =>
          val wrappedBody =
            if a.returnsCompletion then a.body
            else CompletionWrapping.expandAlgorithm(a.body)
          a.copy(
            params = BuiltinParams,
            body = unpackArgumentsList(a.params) ++
              givenValueBinding(a.kind) ++ wrappedBody,
          )
        case AlgorithmKind.Plain => a
    }
