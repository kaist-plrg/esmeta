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
  * does and nothing generic does yet. See `docs/hardcodes.md` #7.
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
    *     does reach this (see `tests/wji/manual/constructors.js`'s `new
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
    *     exports object`'s `|exportsObject|`) — then, same as the "argument
    *     actually supplied" branch just above, run through
    *     `converted_to_an_idl_value` if `p.idlType` is known, so a dictionary
    *     member with its own IDL default (e.g. `ExceptionOptions.traceStack =
    *     false`) actually gets filled in instead of just being a plain
    *     no-own-properties object — omitting an optional dictionary argument
    *     and passing `{}` explicitly must produce the same result, and the
    *     "supplied" branch already always converts. Any other default text
    *     fails loudly via `UnsupportedSpecShape` instead of being guessed at.
    */
  /** required (non-defaulted) members of the WebIDL dictionaries
    * `converted_to_an_idl_value` (`esmeta.wji.interpreter.WebIdlConversion`)
    * already knows how to convert — duplicated here rather than shared (same
    * call as [[unpackArgumentsList]]'s own doc makes for its sibling in
    * `AddBuiltinBehaviourPass`) since that function's member/default tables are
    * native-Scala runtime data private to the `wji.interpreter` package, while
    * this needs the *required* subset at compile time, to emit an IR-level
    * check (see [[requiredMemberChecks]]) — `converted_to_an_idl_value` itself
    * has no way to raise a catchable ECMAScript `TypeError` for a missing
    * required member (`docs/hardcodes.md` #2), so the check has to happen here
    * instead, before that call ever runs.
    */
  private val requiredDictionaryMembers: Map[String, List[String]] = Map(
    "MemoryDescriptor" -> List("initial"),
    "TableDescriptor" -> List("element", "initial"),
    "GlobalDescriptor" -> List("value"),
    "TagType" -> List("parameters"),
  )

  /** one `Cond.HasField(..., negated = true) -> Throw(New("TypeError"))` guard
    * per required member of `ty`, run before `converted_to_an_idl_value` so a
    * missing required member throws a real, catchable `TypeError` instead of
    * silently converting to a dictionary that's missing the field, which
    * crashes later the first time something reads it.
    *
    * `name` is still the *raw* ECMAScript argument here — own JS properties
    * live under its `__MAP__` sub-map, each entry itself a
    * `PropertyDescriptor`, so a member check has to route through
    * `Expr.Field(_, "__MAP__")` explicitly: a plain `Expr.Index(Expr.Var(name),
    * Expr.Str(member))` would compile to a bare `Field` ref straight on
    * `name`'s own Record (`esmeta.wji.compiler.Compiler.compileRef`'s `Index`
    * case), which only sees that Record's literal fields
    * (`"Prototype"`/`"__MAP__"`/...) — never the JS-level property `member`
    * actually names, so `exists` there is always `false`. A non-`Object` value
    * (`undefined`/`null`, but also any primitive — a `false`/number/string/
    * `Symbol()` argument is just as valid a WPT "invalid descriptor" case as
    * `undefined`, see `spectec/test/js-api/memory/constructor.any.js`'s
    * "Invalid descriptor argument") gets an explicit `Cond.IsType(_, "Object",
    * negated = true)` check first rather than falling into the same `__MAP__`
    * read, since dereferencing `.__MAP__` straight on one crashes with
    * `InvalidRefBase` instead of ever reaching `exists` — mirrors WebIDL's own
    * dictionary conversion, whose first step is exactly "if Type(V) is not
    * Undefined, Null, or Object, throw a TypeError".
    */
  private def requiredMemberChecks(ty: String, name: String): List[Instr] =
    requiredDictionaryMembers.get(ty) match
      case None => Nil
      case Some(members) =>
        List(
          Instr.IfChain(
            List(
              Cond.IsType(
                Expr.Var(name),
                "Object",
                negated = true,
              ) -> List(Instr.Throw(Expr.New("TypeError"))),
            ),
            members.map { member =>
              Instr.IfChain(
                List(
                  Cond.HasField(
                    Expr.Index(
                      Expr.Field(Expr.Var(name), "__MAP__"),
                      Expr.Str(member),
                    ),
                    negated = true,
                  ) -> List(Instr.Throw(Expr.New("TypeError"))),
                ),
                Nil,
              )
            },
          ),
        )

  private def omittedBranch(p: WjiParam, name: String): List[Instr] =
    if !p.optional then List(Instr.Throw(Expr.New("TypeError")))
    else
      p.default match
        case None =>
          List(Instr.Let(Expr.Var(name), Expr.SpecTerm("undefined")))
        case Some("{}") =>
          Instr.Perform(
            "OrdinaryObjectCreate",
            List(Expr.SpecTerm("null")),
            Instr.PerformOutcome.BindResult(name),
          ) :: p.idlType.toList.map { ty =>
            Instr.Perform(
              "converted_to_an_idl_value",
              List(Expr.Var(name), Expr.Str(ty)),
              Instr.PerformOutcome.BindResult(name),
            )
          }
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
        val checks = p.idlType.toList.flatMap(requiredMemberChecks(_, name))
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
          ) :: (checks ++ convert)
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

  /** `webidl/index.bs`'s "attribute getter"/"attribute setter"/"creating an
    * operation function" each open with "If |jsValue| does not implement
    * |target|, ... throw a TypeError" (`|jsValue|` being **this**, `|target|`
    * this member's own interface) before ever running the attribute's/
    * operation's own steps — a check every js-api Getter/Setter/Method text
    * itself always omits (it's WebIDL's job, not theirs), so nothing here
    * mechanized it until now. `Cond.Implements(Expr.This, iface, negated =
    * true)` — the same primitive `docs/hardcodes.md` #11 already built for
    * "read the imports"/"create a host function" — compiles to `EImplements`, a
    * flat record-tag comparison bypassing `esmeta.ty.TyModel` entirely. A
    * `Cond.IsType`/`RecordT`-based nominal check was tried here first and
    * reverted: `TyModel.diffOf` crashes (`parentOf(l).get` on a type only known
    * via WJI's *dynamic* subtype registry) whenever **this** turns out to be
    * some other, unrelated Object — and even fixing that crash,
    * `RecordTy.contains`'s subtyping is structural, not nominal, so it could
    * still wrongly let one interface's instance brand-check as another (see
    * #11's own write-up of the identical trap for `Tag`/`Exception`).
    */
  private def brandingCheck(kind: AlgorithmKind): List[Instr] =
    val iface = kind match
      case AlgorithmKind.Getter(i) => Some(i)
      case AlgorithmKind.Setter(i) => Some(i)
      case AlgorithmKind.Method(i) => Some(i)
      case _                       => None
    iface.toList.map { i =>
      Instr.IfChain(
        List(
          Cond.Implements(Expr.This, i, negated = true) -> List(
            Instr.Throw(Expr.New("TypeError")),
          ),
        ),
        Nil,
      )
    }

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
            body = brandingCheck(a.kind) ++ unpackArgumentsList(a.params) ++
              givenValueBinding(a.kind) ++ createThisBinding(a.kind) ++
              a.body ++ returnThisBinding(a.kind),
          )
        case AlgorithmKind.Plain => a
    }
