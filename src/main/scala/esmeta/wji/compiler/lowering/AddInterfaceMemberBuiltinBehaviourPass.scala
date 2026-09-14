package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, AlgorithmKind, Cond, Expr, Instr, WjiParam}
import esmeta.wji.compiler.Compiler
import esmeta.error.UnsupportedSpecShape

/** Reshapes every Getter/Setter/Constructor/Method/NamespaceMethod/
  * NamespaceGetter-kind [[Algorithm]] — the 4 kinds WebIDL calls an interface
  * "member" (per `webidl/index.bs`'s own "Members" section: "The constructor
  * steps, getter steps, setter steps, and method steps ... have access to a
  * this value"), plus a namespace's own operations/attributes, which share the
  * same calling convention without sharing a receiver — into the `<BUILTIN>:`
  * calling convention mainline's own `Call`/`BuiltinCallOrConstruct` machinery
  * expects, the same two fix-ups [[AddBuiltinBehaviourPass]] applies to a
  * hoisted `CreateBuiltinFunction` closure, for the same reason (a
  * calling-convention requirement, not conditional on whether the algorithm
  * itself can abruptly complete):
  *
  * `Method` here is only ever a *real interface* member (`Table.get`,
  * `Global.valueOf`, ...); a namespace member (`WebAssembly.instantiate`/
  * `compile`/`validate`) is `NamespaceMethod` instead —
  * `esmeta.wji.extractor.Extractor` already restamps any `Method(for)` whose
  * `for` names a WebIDL `namespace` rather than an `interface` before lowering
  * ever runs (`AlgorithmKind.NamespaceMethod`'s own doc has the full rationale
  * for why the two need to stay distinct kinds rather than collapsing into one:
  * `webidl/index.bs` itself treats a namespace's own operations and an
  * interface's members as products of two genuinely different algorithms
  * building and populating two different objects). What that distinction means
  * *here*: a `NamespaceMethod` skips every receiver-related step below
  * ([[newTargetCheck]] — not a `Constructor`; [[brandingCheck]] — no
  * `**this**`-implements-interface guard, since a namespace operation has no
  * interface to implement; [[givenValueBinding]] — not a `Setter`;
  * [[createThisBinding]]/[[returnEpilogue]]'s `Constructor` case — not a
  * `Constructor`, and unlike a `Getter`/`Method`/`Setter` a namespace
  * operation's `**this**` is simply never read at all, so nothing needs
  * binding; [[returnEpilogue]]'s `"undefined"`-return case does still apply to
  * a `NamespaceMethod`, in principle — no corpus occurrence has that declared
  * return type today, but nothing about it depends on having a receiver) but
  * still goes through [[unpackArgumentsList]] and [[wrapReturnValues]] exactly
  * like a `Method` does — and, in `Compiler`, registers under
  * `INTRINSICS.<namespace>.<name>` (no `.prototype` segment:
  * `WebAssembly.instantiate`, never `WebAssembly.prototype.instantiate`) — see
  * `Compiler.compileAlgo`'s own `NamespaceMethod` case.
  *
  * `AlgorithmKind.NamespaceGetter` is `NamespaceMethod`'s exact counterpart for
  * a namespace attribute's getter (e.g. `WebAssembly.JSTag`) — restamped from
  * `Getter(for)` the same way, skips the same receiver-related steps for the
  * same reason, and registers under `INTRINSICS.get:<namespace>.<name>` (no
  * `.prototype` segment, and no `WebAssembly.` prefix baked into the name the
  * way the ordinary `Getter` case's `INTRINSICS.get:WebAssembly.<iface>.
  * prototype.<name>` template has — that literal prefix is exactly why `JSTag`
  * used to compile under the doubled, wrong `INTRINSICS.get:
  * WebAssembly.WebAssembly.prototype.JSTag` before this kind existed).
  *
  * Before this kind existed, every `WebAssembly.instantiate`/`compile`/
  * `validate` fell to `Plain` (an ordinary free function, attached to nothing)
  * instead of reaching this pass at all — `compile`/`validate` were entirely
  * absent from the runtime `WebAssembly` object as a result
  * (`interface.any.js`'s `assert_equals(typeof propdesc, "object")` failing
  * with `"undefined"`), and `instantiate` only existed because of a
  * hand-written `manuals/funcs/INTRINSICS.WebAssembly.instantiate.ir` glue file
  * working around the gap one operation at a time — itself still imperfect,
  * since it fell back to mainline's own generic `INTRINSICS.<base>.
  * <prop>`-name auto-attach (`esmeta.es.Initialize.addPropBuiltinFuncs`) for
  * actually becoming a property, which hardcodes `Enumerable: false` (right for
  * an ordinary ECMA-262 builtin method, wrong for a WebIDL namespace operation,
  * which WebIDL's own "create a namespace object" preamble installs as `{
  * [[Writable]]: true, [[Enumerable]]: true, [[Configurable]]: true }`) and
  * computes `.length` from `func.head` (`None` for a hand-written glue file
  * with no spec prose behind it, so always `0`). This pass now produces a real
  * `<BUILTIN>:INTRINSICS.WebAssembly.<name>` function for all three uniformly,
  * the same way it already does for every interface member —
  * `manuals/intrinsics` still needs an explicit `[TTT]`-flagged declaration
  * (mirroring `Module.exports`'s own entry) to get the correct descriptor
  * instead of falling through to that generic fallback, and an explicit
  * `length` override (same pattern as `Memory.prototype.grow`'s own), but no
  * more hand-written call-unpacking glue. See `docs/hardcodes.md` #7.
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
  * [[createThisBinding]]/[[returnEpilogue]]'s `Constructor` case mechanize
  * exactly that preamble/epilogue. The object itself still reuses the same
  * `Expr.New(iface)` → `ERecord(iface, ordinaryObjectFields(iface))`
  * construction `esmeta.wji.compiler.Compiler.compileExpr` already uses for the
  * "Let |x| be a new Y." shape inside algorithm bodies (see `docs/hardcodes.md`
  * #7), but its `[[Prototype]]` gets overwritten right after — WebIDL's real
  * preamble is `? OrdinaryCreateFromConstructor(NewTarget,
  * "%<iface>.prototype%")` (`webidl/index.bs`), whose whole point is reading
  * `NewTarget`'s own `"prototype"` property first (falling back to the default
  * intrinsic only when that isn't an Object) — exactly what makes `class Sub
  * extends WebAssembly.Module {}; new Sub(...) instanceof Sub` true.
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
    * in the first place — including that same "fewer arguments actually
    * supplied than declared" guard (`global/value-get-set.any.js`'s "Calling
    * setter without argument" calls the underlying builtin function object
    * directly with zero arguments, `setter.call(global)` — an accessor
    * function's own `[[Call]]` never enforces arity the way `[[Construct]]`/
    * ordinary property assignment implicitly does, so `ArgumentsList` can
    * really be empty here). Ordinary ECMAScript missing-parameter semantics
    * (bind to `undefined`) apply directly with no `IdlType`/default-value
    * detour of its own — "the given value" is never a declared WebIDL parameter
    * (so has neither `optional`/`default`, unlike [[unpackArgumentsList]]'s
    * params), and the spec text itself only ever converts it once, inline in
    * the setter's own body (`? ToWebAssemblyValue( **the given value**,
    * |valuetype|)`) — so this always just binds the raw value (or `undefined`),
    * never `omittedBranch`'s dictionary-default path.
    */
  private def givenValueBinding(kind: AlgorithmKind): List[Instr] = kind match
    case AlgorithmKind.Setter(_) =>
      List(
        Instr.IfChain(
          List(
            Cond.Compare(
              Expr.Num("0"),
              Cond.CompareOp.Lt,
              Expr.Length(Expr.Var("ArgumentsList")),
            ) -> List(
              Instr.Let(
                Expr.Var("givenValue"),
                Expr.Index(Expr.Var("ArgumentsList"), Expr.Num("0")),
              ),
            ),
          ),
          List(Instr.Let(Expr.Var("givenValue"), Expr.SpecTerm("undefined"))),
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
    * needed) — `BuiltinParams` already binds `|NewTarget|` for every one of
    * this pass's algorithms, just unchecked until now. `Cond.IsMissing`
    * compiles to exactly `NewTarget == undefined` (`Compiler.compileCond`), so
    * this is a one-guard check, same shape as [[brandingCheck]] just below.
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
    *
    * Skipped entirely for a *static* `Method` (`AlgorithmKind.Method.static`,
    * e.g. `WebAssembly.Module.exports(moduleObject)`): its real receiver is its
    * own first argument, never `**this**` (a normal call site's `**this**`
    * there is just the `Module` constructor function object itself, not a
    * `Module` instance -- checking it would reject every legitimate call).
    * `WebIdlConversion.toIdlValue`'s `"Module"` case does the equivalent check
    * on that argument instead, as part of its own IDL conversion.
    */
  private def brandingCheck(kind: AlgorithmKind): List[Instr] =
    val iface = kind match
      case AlgorithmKind.Getter(i)         => Some(i)
      case AlgorithmKind.Setter(i)         => Some(i)
      case AlgorithmKind.Method(i, static) => Option.unless(static)(i)
      case _                               => None
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
    * implicit `Return **this**`, see [[returnEpilogue]], is already a real
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
      case AlgorithmKind.Getter(_)          => true
      case AlgorithmKind.Method(_, _)       => true
      case AlgorithmKind.NamespaceMethod(_) => true
      case AlgorithmKind.NamespaceGetter(_) => true
      case _                                => false
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
        // "... and return the result." parses straight to this shape at parse
        // time (`InstrParser`'s `PerformAndReturnSuffix`), never as
        // `Return(Some(AlgoCall(...)))` -- so the case above alone never
        // catches a tail-call return written this way (e.g. `instantiate`'s
        // "Instantiate |promiseOfModule| with imports |importObject| and
        // return the result."). Same rewrite, just from a different starting
        // shape: bind the call's result, convert it, then return the bound
        // name explicitly.
        case Instr.Perform(
              func,
              args,
              Instr.PerformOutcome.ReturnResult,
              nested,
            ) =>
          val v = freshName()
          List(
            Instr.Perform(func, args, Instr.PerformOutcome.BindResult(v)),
            Instr.Perform(
              "converted_to_a_javascript_value",
              List(Expr.Var(v)),
              Instr.PerformOutcome.BindResult(v),
            ),
            Instr.Return(Some(Expr.Var(v)), transform(nested)),
          )
        case other => List(other.mapBody(transform))
      }
      transform(body)

  /** The matching epilogue for an algorithm whose spec prose never ends in an
    * explicit `Return`, appended unconditionally after `wrapReturnValues` (so,
    * unlike an explicit `Return` in the body, neither of these two cases ever
    * goes through `converted_to_a_javascript_value` -- see each case's own note
    * for why that's fine here):
    *
    *   - `Constructor`: every js-api constructor algorithm ends by mutating
    *     `**this**`'s fields with no explicit `Return`, relying on WebIDL's
    *     outer wrapper to return the object it created -- mechanized here as an
    *     explicit, still-raw `Return **this**.` instead. `**this**` is already
    *     a real object, never worth converting.
    *   - `Method`/`NamespaceMethod` whose own WebIDL-declared return type is
    *     `"undefined"` (`Algorithm.idlReturnType`, stamped by
    *     `esmeta.wji.extractor.Extractor.enrichParamTypes`) -- the WebIDL
    *     equivalent of the same gap, for a different member kind:
    *     `Table.prototype.set` (the corpus's one occurrence, index.bs:1008's
    *     `undefined set(AddressValue index, optional any value);`) never writes
    *     an explicit `Return` either, relying on its declared `undefined`
    *     return type instead. Left unmechanized, `Table.prototype. set`'s real
    *     JS-visible return value fell through to `WrapCompletionReturnsPass`'s
    *     own generic fallback for a body with no terminal `Return` on some path
    * -- `NormalCompletion(~unused~)`, ECMA-262's own internal "no meaningful
    * return value" sentinel -- which then leaked out completely unconverted
    * (`WebIdlConversion. toJsValue` has no case for it, so it's passed through
    * as-is): dead code paths that never read the return value never noticed,
    * but `assert_equals(table.set(0, fn), undefined, ...)` does, crashing with
    * a bare `typeof`-internal assertion failure (`(? val: Record[Object])`)
    * with no clue this sentinel was ever involved. Appending a real, explicit
    * `Return undefined.` here -- `Expr.SpecTerm("undefined")` compiles straight
    * to `EUndef()` (`Compiler.compileExpr`), a genuine already-converted
    * ECMAScript value
    * -- sidesteps the sentinel entirely, the same way `**this**` sidesteps
    * needing a WebIDL value conversion of its own: WebIDL's own calling
    * convention already guarantees an `undefined`-typed operation returns real
    * `undefined`, so this makes that guarantee explicit rather than relying on
    * whatever ECMA-262's own generic fallback happens to produce for a body
    * that merely falls off the end.
    *   - `Setter`: WebIDL's setter algorithms are written the same void-return
    *     way as `Table.prototype.set` above (no declared return type at all to
    *     read from `idlReturnType` -- setters have none -- but no explicit
    *     `Return` in the prose either), on the assumption that a setter's
    *     return value is never observed: ordinary property-assignment (`x.value
    * = v`) evaluates to the *assignment expression*'s own RHS, never to
    * whatever `[[Set]]` invoking the setter actually returns, so that
    * assumption holds for everyday code. It's wrong for code that calls the
    * underlying builtin function object directly instead --
    * `global/value-get-set.any.js`'s `assert_equals(setter.call(global,
    * undefined), undefined)` is exactly this — which hits the identical
    * `~unused~`-leak failure mode `Table.prototype.set` did, for the identical
    * reason. Same fix, unconditionally (a setter has no `idlReturnType` to gate
    * on either way).
    *
    * Every other kind (`Getter`/`NamespaceGetter`, and any `Method`/
    * `NamespaceMethod` with a real declared return type) needs no epilogue of
    * its own here: a getter/non-void method's spec prose always ends in an
    * explicit `Return`. `CompletionAlgorithms` seeds every
    * `Constructor`/`Method`/`NamespaceMethod` as `returnsCompletion = true`
    * unconditionally (see class doc), so `WrapCompletionReturnsPass` wraps
    * whichever of these cases fired along with the rest of the body, uniformly.
    */
  private def returnEpilogue(algo: Algorithm): List[Instr] = algo.kind match
    case AlgorithmKind.Constructor(_) => List(Instr.Return(Some(Expr.This)))
    case AlgorithmKind.Setter(_) =>
      List(Instr.Return(Some(Expr.SpecTerm("undefined"))))
    case AlgorithmKind.Method(_, _) | AlgorithmKind.NamespaceMethod(_)
        if algo.idlReturnType.contains("undefined") =>
      List(Instr.Return(Some(Expr.SpecTerm("undefined"))))
    case _ => Nil

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map { a =>
      a.kind match
        case AlgorithmKind.Getter(_) | AlgorithmKind.Setter(_) |
            AlgorithmKind.Constructor(_) | AlgorithmKind.Method(_, _) |
            AlgorithmKind.NamespaceMethod(_) | AlgorithmKind.NamespaceGetter(
              _,
            ) =>
          a.copy(
            params = BuiltinParams,
            body = newTargetCheck(a.kind) ++ brandingCheck(a.kind) ++
              unpackArgumentsList(a.params) ++
              givenValueBinding(a.kind) ++ createThisBinding(a.kind) ++
              wrapReturnValues(a.kind, a.body) ++ returnEpilogue(a),
          )
        case AlgorithmKind.Plain => a
    }
