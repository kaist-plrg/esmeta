package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, AlgorithmKind, Cond, Expr, Instr, WjiParam}
import esmeta.wji.compiler.Compiler
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
  * preamble/epilogue. The object itself still reuses the same `Expr.New(iface)`
  * → `ERecord(iface, ordinaryObjectFields(iface))` construction
  * `esmeta.wji.compiler.Compiler.compileExpr` already uses for the "Let |x| be
  * a new Y." shape inside algorithm bodies (see `docs/hardcodes.md` #7), but
  * its `[[Prototype]]` gets overwritten right after — WebIDL's real preamble is
  * `? OrdinaryCreateFromConstructor(NewTarget, "%<iface>.prototype%")`
  * (`webidl/index.bs`), whose whole point is reading `NewTarget`'s own
  * `"prototype"` property first (falling back to the default intrinsic only
  * when that isn't an Object) — exactly what makes `class Sub extends
  * WebAssembly.Module {}; new Sub(...) instanceof Sub` true.
  * `ordinaryObjectFields`'s `Prototype` field is always the fixed default
  * intrinsic (correct for the unrelated re-entrant callers of bare
  * `Expr.New(iface)`, e.g. "create a memory object" from an address — never
  * invoked through `[[Construct]]`, so there's no real `NewTarget` to consult
  * there), so `createThisBinding` doesn't touch that shared helper; it just
  * replaces the field again with the real ECMA-262 AO
  * `GetPrototypeFromConstructor(NewTarget, intrinsicDefaultProto)`
  * (`ecma262/spec.html`'s `sec-getprototypefromconstructor` — the exact
  * sub-step `OrdinaryCreateFromConstructor` itself delegates to) mainline
  * already compiles, called here by its literal AO name the same way
  * hand-written `manuals/funcs` `.ir` glue already reuses mainline AOs (e.g.
  * `ConvertToInt.ir`'s `clo<"ToNumber">`).
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
  /** the WebIDL dictionary types this pass already knew about before
    * required-member validation moved into `converted_to_an_idl_value` itself
    * (`esmeta.wji.interpreter.WebIdlConversion.readDictionary`, which now
    * genuinely throws — see its own doc) — kept here only for the *other* guard
    * dictionary conversion needs first, [[nonObjectCheck]]. `ExceptionOptions`
    * was never in this set either, before or after: it has no required members,
    * and never got the non-`Object` guard (a pre-existing gap,
    * `docs/hardcodes.md` #2, not newly introduced by this pass).
    */
  private val knownDictionaryTypes: Set[String] =
    Set("MemoryDescriptor", "TableDescriptor", "GlobalDescriptor", "TagType")

  /** WebIDL dictionary conversion's own first step: "if Type(V) is not
    * Undefined, Null, or Object, throw a TypeError" — run before
    * `converted_to_an_idl_value` for `ty`'s in [[knownDictionaryTypes]], since
    * that function itself only ever reads `argument` as `Undefined`/`Null`/an
    * `Object` (its `Get` calls would crash on anything else, e.g. a `false`/
    * number/string/`Symbol()` argument — just as valid a WPT "invalid
    * descriptor" case as `undefined`, see
    * `spectec/test/js-api/memory/constructor.any.js`'s "Invalid descriptor
    * argument"). Also covers a bare `sequence<T>` param (so far only
    * `Exception`'s constructor's `payload`) — sequence conversion actually
    * requires an Object even more strictly (no `Undefined`/`Null` collapsing),
    * but the same "throw unless Object" check is a safe (if slightly
    * stricter-than-spec on paper) stand-in: `WebIdlConversion. toSequence`
    * reads `argument`'s own `"__MAP__"` field directly, which crashes on a
    * non-`Addr` value (e.g. `123n`,
    * `spectec/test/js-api/exception/constructor.tentative.any.js`'s "Invalid
    * exception argument") instead of throwing `TypeError`.
    */
  private def nonObjectCheck(ty: String, name: String): List[Instr] =
    if !(knownDictionaryTypes(ty) || ty.startsWith("sequence<")) then Nil
    else
      List(
        Instr.IfChain(
          List(
            Cond.IsType(Expr.Var(name), "Object", negated = true) ->
            List(Instr.Throw(Expr.New("TypeError"))),
          ),
          Nil,
        ),
      )

  /** `Perform converted_to_an_idl_value(name, ty), let name be the result.`
    * followed by an abrupt-completion check: `converted_to_an_idl_value`
    * (`WebIdlConversion.call`) returns the plain converted value on success,
    * same as ever -- but a dictionary member's getter can itself throw, or a
    * required member can turn out absent, or an enum value can turn out
    * invalid, and any of those now come back as a genuine `ThrowCompletion`
    * instead (see its own doc for why the *success* case deliberately isn't
    * also completion-wrapped: `webidl/index.bs`'s real `react` algorithm has
    * its own unmarked call site that never unwraps one). The `AbruptCompletion`
    * check tells the two apart; on abrupt, propagate it directly, exactly the
    * way any other `?`-marked call's `Instr.Return` does once
    * `CompletionWrapping` wraps this algorithm's own exit paths (this pass's
    * `run` always runs it, see class doc) -- otherwise `name` already holds the
    * right value, nothing further to unwrap.
    */
  private def convertedIdlValueBinding(name: String, ty: String): List[Instr] =
    List(
      Instr.Perform(
        "converted_to_an_idl_value",
        List(Expr.Var(name), Expr.Str(ty)),
        Instr.PerformOutcome.BindResult(name),
      ),
      Instr.IfChain(
        List(
          Cond.IsType(Expr.Var(name), "AbruptCompletion") ->
          List(Instr.Return(Some(Expr.Var(name)))),
        ),
        Nil,
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
          ) :: p.idlType.toList.flatMap(convertedIdlValueBinding(name, _))
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
        val checks = p.idlType.toList.flatMap(nonObjectCheck(_, name))
        val convert =
          p.idlType.toList.flatMap(convertedIdlValueBinding(name, _))
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

  /** every ECMAScript class constructor throws a `TypeError` when invoked via
    * plain `[[Call]]` instead of `[[Construct]]` (`sec-ecmascript-function-
    * objects-call-thisargument-argumentslist`'s own "If
    * F.[[IsClassConstructor]] is true, throw a TypeError" — a
    * `Constructor`-kind interface member is exactly this shape, per WebIDL's
    * own "internally create a new object implementing the interface" preamble
    * requiring a real `[[Construct]]`). Mainline's `BuiltinCallOrConstruct`
    * already threads the real `NewTarget` on `[[Construct]]` and `undefined` on
    * a plain `[[Call]]` (ECMA-262's own mechanized behavior, no WJI involvement
    * needed) — `run()` binds `|NewTarget|` for every `Constructor` (and
    * `Getter`) of this pass's algorithms, just unchecked until now.
    * `Cond.IsMissing` compiles to exactly `NewTarget == undefined`
    * (`Compiler.compileCond`), so this is a one-guard check.
    */
  private def newTargetCheck(kind: AlgorithmKind): List[Instr] = kind match
    case AlgorithmKind.Constructor(_) =>
      List(
        Instr.IfChain(
          List(
            Cond.IsMissing(Expr.Var("NewTarget")) ->
            List(Instr.Throw(Expr.New("TypeError"))),
          ),
          Nil,
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
      val default = Instr.Set(Expr.This, Expr.New(iface))
      Compiler.namesWithPrototypeIntrinsic.get(iface) match
        case None => List(default)
        case Some(intrinsicKey) =>
          List(
            default,
            // "Let x be ? GetPrototypeFromConstructor(NewTarget, intrinsicKey)."
            // shape -- left for NormalizeEvaluationOrderPass/ExpandAbruptPass
            // (both run after this pass) to hoist/expand the same way real
            // parsed prose would.
            Instr.Let(
              Expr.Var("_proto"),
              Expr.Abrupt(
                "?",
                Expr.AlgoCall(
                  "GetPrototypeFromConstructor",
                  List(Expr.Var("NewTarget"), Expr.Str(intrinsicKey)),
                ),
              ),
            ),
            Instr.Set(Expr.Field(Expr.This, "Prototype"), Expr.Var("_proto")),
          )
    case _ => Nil

  /** Wraps every `Return`'s value -- recursively, including ones nested inside
    * an `IfChain`/`ForEach`/etc. (`Instr.mapBody` already knows how to
    * structurally recurse into each of those, `IfChain`'s own `branches`/
    * `fallback` included) -- in `converted_to_a_javascript_value`. WebIDL's own
    * calling convention implicitly converts an operation's return value to a
    * real JavaScript value the same way it converts each argument to its
    * declared IDL type (`unpackArgumentsList`'s own `converted_to_an_idl_value`
    * injection) -- spec prose never spells this out either (just "Return
    * |exports|."), so nothing mechanized it before: `Module.exports`'s
    * `sequence<ModuleExportDescriptor>` return value was a raw internal
    * `ListObj` of raw internal `MapObj`s, never actually turned into a real
    * `Array` of real objects (`WebIdlConversion.toJsValue` didn't know how to
    * convert a `ListObj` at all until now either -- see its own doc).
    *
    * Only for `Getter`/`Method` -- WebIDL declares a real return *type* for
    * both, unlike `Setter` (no return value at all) or `Constructor` (whose own
    * implicit `Return **this**`, see [[returnThisBinding]], is already a real
    * object, never worth this). Safe to apply unconditionally to every one of
    * them regardless of what they actually return: `toJsValue` is already
    * identity passthrough for anything that isn't a `MapObj`/ `ListObj`, so
    * wrapping a Return that never needed it is a no-op.
    */
  private def wrapReturnValues(
    kind: AlgorithmKind,
    body: List[Instr],
  ): List[Instr] =
    val needsWrap = kind match
      case AlgorithmKind.Getter(_)    => true
      case AlgorithmKind.Method(_, _) => true
      case _                          => false
    if !needsWrap then body
    else
      var freshCounter = 0
      def freshName(): String =
        freshCounter += 1
        s"_returnValue$freshCounter"
      def transform(instrs: List[Instr]): List[Instr] = instrs.flatMap {
        case Instr.Return(Some(expr), nested) =>
          val (bindings, name) = expr match
            case Expr.Var(v) => (Nil, v)
            case _ =>
              val v = freshName()
              (List(Instr.Let(Expr.Var(v), expr)), v)
          bindings ++ List(
            Instr.Perform(
              "converted_to_a_javascript_value",
              List(Expr.Var(name)),
              Instr.PerformOutcome.BindResult(name),
            ),
            Instr.Return(Some(Expr.Var(name)), transform(nested)),
          )
        case other => List(other.mapBody(transform))
      }
      transform(body)

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
            AlgorithmKind.Constructor(_) | AlgorithmKind.Method(_, _) =>
          val params = a.kind match
            case AlgorithmKind.Getter(_) | AlgorithmKind.Constructor(_) =>
              BuiltinParams :+ WjiParam("|NewTarget|")
            case _ => BuiltinParams
          a.copy(
            params = params,
            body = newTargetCheck(a.kind) ++
              unpackArgumentsList(a.params) ++
              givenValueBinding(a.kind) ++ createThisBinding(a.kind) ++
              wrapReturnValues(a.kind, a.body) ++ returnThisBinding(a.kind),
          )
        case AlgorithmKind.Plain => a
    }
