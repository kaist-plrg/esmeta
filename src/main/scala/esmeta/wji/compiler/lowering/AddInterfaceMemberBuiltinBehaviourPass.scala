package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, AlgorithmKind, Cond, Expr, Instr, WjiParam}
import esmeta.error.UnsupportedSpecShape

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
  * hoisted closures rather than top-level interface members). For a
  * Getter/Setter/Method, `**this**` needs no such unpacking: it's already a
  * real receiver at call time (`[[Call]]` always supplies one), and compiles
  * directly to the same local the `|this|` parameter declares (see
  * `esmeta.wji.compiler.Compiler`'s `Expr.This` case), so simply declaring
  * `|this|` as a parameter is already enough to bind it — no `Set **this** to
  * |this|.` prefix needed. A `Constructor` is different: it's invoked via
  * `[[Construct]]`, which per ECMA-262
  * (`sec-built-in-function-objects-construct-argumentslist-newtarget`) never
  * supplies a `this` at all (`BuiltinCallOrConstruct` gets `~uninitialized~`) —
  * allocating the object and binding it as `this` is WebIDL's own "create an
  * interface object" preamble (`webidl/index.bs`, step "internally create a new
  * object implementing the interface" before "Perform the constructor steps ...
  * with object as this"), a step outside the constructor-steps text itself,
  * which is why no js-api constructor algorithm ever writes it and every one
  * instead ends by mutating `this`'s fields with no explicit `Return`.
  * [[createThisBinding]]/[[returnThisBinding]] mechanize exactly that
  * preamble/epilogue, reusing the same `Expr.New(iface)` → `ERecord(iface,
  * ordinaryObjectFields(iface))` construction
  * `esmeta.wji.compiler.Compiler.compileExpr` already uses for the "Let |x| be
  * a new Y." shape inside algorithm bodies (see `docs/hardcodes.md` #7) — it
  * already has real prototype wiring for every interface a WJI test constructs
  * directly (`Module`, `Instance`, `Memory`, `Table`, `Global`).
  *   - '''WebIDL's implicit setter argument''': a `Setter`-kind algorithm's
  *     `**the given value**` (`Expr.GivenValue`) is WebIDL's other implicit
  *     member-only binding, alongside `**this**` — unpacked from
  *     `ArgumentsList[0]` the same way, since (unlike every declared `|param|`)
  *     it was never a real Bikeshed `|pipe|` variable `extractParams` could
  *     have already found.
  *   - '''Completion-record wrapping''': every exit path must return a real
  *     Completion Record, same as [[AddBuiltinBehaviourPass]]'s own reason
  *     (mainline's Call machinery always expects one back, regardless of
  *     whether the algorithm itself can abruptly complete).
  *     [[CompletionAlgorithms.compute]] seeds `returnsCompletion = true`
  *     unconditionally for every interface member — WebIDL's own overload
  *     resolution can always throw a `TypeError` for a `Constructor`'s arity
  *     mismatch (see [[omittedBranch]]), and every Getter/Setter/Method shares
  *     the same unconditional calling-convention requirement — so
  *     [[InsertFallthroughReturnPass]]/[[WrapCompletionReturnsPass]] (which run
  *     after this pass — see `Lowering.pipeline`) handle every exit path
  *     uniformly, the same as for any ordinary algorithm; this pass itself just
  *     leaves `Return`/`Throw` raw.
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

  private val BuiltinParams =
    List(
      WjiParam("|this|"),
      WjiParam("|ArgumentsList|"),
      WjiParam("|NewTarget|"),
    )

  private def stripPipes(s: String): String =
    s.stripPrefix("|").stripSuffix("|")

  /** the instructions that run in `unpackArgumentsList`'s `IfChain` when fewer
    * arguments were actually supplied than a param's own position — mirrors
    * [[AddBuiltinBehaviourPass.omittedBranch]] (duplicated for the same reason
    * `unpackArgumentsList` itself is).
    *
    *   - A *required* param (`!p.optional`) reaching here is exactly the case
    *     WebIDL's own overload resolution algorithm (`webidl/index.bs`
    *     `argcount`/effective-overload-set steps) is defined to catch before
    *     the operation's steps ever run: with no real overloading in this
    *     corpus (a single entry per identifier, [=list/append|expanded=] only
    *     by trailing optional/defaulted params), too few arguments always means
    *     no effective-overload-set entry matches, so the algorithm throws a
    *     `TypeError` — mirrored here directly as a raw [[Instr.Throw]] (left
    *     for `CompletionAlgorithms`/`WrapCompletionReturnsPass` to wrap later —
    *     see class doc) rather than asserted unreachable, since real user code
    *     does reach this (see `tests/wji/constructors.js`'s `new
    *     WebAssembly.Module()`).
    *   - An optional param with no [[WjiParam.default]] is bound to `undefined`
    *     — not left unbound. Per WebIDL's overload resolution algorithm, an
    *     omitted argument and one explicitly passed as `undefined` both convert
    *     to the same "missing" sentinel before an operation's own steps ever
    *     run, and that sentinel is never itself a real ECMAScript value once
    *     inside those steps — the only value real spec text ever observes for
    *     it is `undefined` (see `docs/spec_inconsistencies.md` #16).
    *     `Cond.IsMissing`'s `"|X| is missing"` check compiles to exactly that
    *     comparison (`Compiler.compileCond`), so real optional-parameter spec
    *     text branching on it (`Table`'s `|value|`, `Global`'s `|v|`) still
    *     works; spec text that skips the check and reads the param directly
    *     (e.g. `Instance`'s `|importObject|`, passed straight into `read the
    *     imports`) now gets a real bound value instead of crashing. "missing"
    *     per WebIDL's own argument-list processing — omitting it is equivalent
    *     to passing the default value literally, and spec text using one (e.g.
    *     `Module`'s constructor reading `|options|["builtins"]`
    *     unconditionally) never checks `IsMissing` for it at all, so leaving it
    *     unbound would crash instead. Only `"{}"` (an empty dictionary — the
    *     only default this corpus's WebIDL actually declares) is handled, via
    *     the same `[$OrdinaryObjectCreate$](null)` idiom spec text itself
    *     already uses for a fresh, no-own-properties object (e.g. `create an
    *     exports object`'s `|exportsObject|`) — plain property reads on it (all
    *     a defaulted-dictionary param's own spec text ever does) behave
    *     identically whether its prototype is `null` or `%Object.prototype%`,
    *     so there's no need to build a "real" `{}` literal's prototype chain
    *     just for this. Any other default text fails loudly via
    *     `UnsupportedSpecShape` instead of being guessed at.
    */
  private def omittedBranch(p: WjiParam, name: String): List[Instr] =
    if !p.optional then List(Instr.Throw(Expr.New("TypeError")))
    else
      p.default match
        case None =>
          List(Instr.Let(Expr.Var(name), Expr.SpecTerm("undefined")))
        case Some("{}") =>
          List(
            Instr.Perform(
              "OrdinaryObjectCreate",
              List(Expr.SpecTerm("null")),
              Instr.PerformOutcome.BindResult(name),
            ),
          )
        case Some(other) =>
          throw UnsupportedSpecShape(
            "AddInterfaceMemberBuiltinBehaviourPass",
            s"parameter ${p.name} has unsupported default value: $other",
          )

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
    * [[WjiParam.optional]]/[[WjiParam.default]] decide what happens — see
    * [[omittedBranch]].
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
        Instr.IfChain(
          List(
            Cond.Compare(
              Expr.Num(i.toString),
              Cond.CompareOp.Lt,
              Expr.Length(Expr.Var("ArgumentsList")),
            ) -> supplied,
          ),
          omittedBranch(p, name),
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

  /** WebIDL's "internally create a new object implementing the interface"
    * preamble — see this pass's own class doc for why a `Constructor` (unlike
    * Getter/Setter/Method) needs this instead of relying on an already-bound
    * `**this**`.
    */
  private def createThisBinding(kind: AlgorithmKind): List[Instr] = kind match
    case AlgorithmKind.Constructor(iface) =>
      List(Instr.Set(Expr.This, Expr.New(iface)))
    case _ => Nil

  /** The matching epilogue: every js-api constructor algorithm ends by mutating
    * `**this**`'s fields with no explicit `Return`, relying on WebIDL's outer
    * wrapper to return the object it created — mechanized here as an explicit,
    * still-raw `Return **this**.` instead. Left unwrapped:
    * `CompletionAlgorithms` seeds every `Constructor` as `returnsCompletion =
    * true` unconditionally (see class doc), so `WrapCompletionReturnsPass`
    * wraps this along with the rest of the body, uniformly.
    */
  private def returnThisBinding(kind: AlgorithmKind): List[Instr] = kind match
    case AlgorithmKind.Constructor(_) => List(Instr.Return(Some(Expr.This)))
    case _                            => Nil

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map { a =>
      a.kind match
        case AlgorithmKind.Getter(_) | AlgorithmKind.Setter(_) |
            AlgorithmKind.Constructor(_) | AlgorithmKind.Method(_) =>
          a.copy(
            params = BuiltinParams,
            body = unpackArgumentsList(a.params) ++
              givenValueBinding(a.kind) ++ createThisBinding(a.kind) ++
              a.body ++ returnThisBinding(a.kind),
          )
        case AlgorithmKind.Plain => a
    }
