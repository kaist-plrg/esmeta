package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, AlgorithmKind, Cond, Expr, Instr, WjiParam}

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
  * *here*: a `NamespaceMethod` skips [[returnEpilogue]]'s `Constructor` case
  * (unlike a `Getter`/`Method`/`Setter` a namespace operation's `**this**` is
  * simply never read at all; [[returnEpilogue]]'s `"undefined"`-return case
  * does still apply to a `NamespaceMethod`, in principle — no corpus occurrence
  * has that declared return type today, but nothing about it depends on having
  * a receiver) but still goes through [[unpackArgumentsList]] exactly like a
  * `Method` does — and, in `Compiler`, registers under `INTRINSICS.<namespace>.<name>` (no `.prototype` segment:
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
  *   - '''parameters''': each kind takes exactly what WebIDL's own caller of
  *     its steps closure passes (`Initialize.seedHostDefined`'s
  *     `getterSteps`/`setterSteps`/`constructorSteps`/`methodSteps` fields),
  *     regardless of what parameters the algorithm itself declares (e.g.
  *     `Table.get(|index|)`, `Instance(|moduleObject|, |importObject|)`) — see
  *     [[builtinParams]]:
  *     - `Getter`/`NamespaceGetter`: `(this)` — "the [=getter steps=] of
  * |attribute| with |idlObject| as [=this=]".
  *   - `Setter`: `(this, givenValue)` — "... with |idlObject| as [=this=] and
  *     |idlValue| as [=the given value=]". `**the given value**`
  *     (`Expr.GivenValue`) compiles directly to the `givenValue` local that
  *     parameter declares (`esmeta.wji.compiler.Compiler`'s `Expr.GivenValue`
  *     case), so no binding prefix is needed.
  *   - `Constructor`/`Method`/`NamespaceMethod`: `(this, ArgumentsList)` — "...
  *     with |object| as [=this=] and |values| as the argument values". Every
  *     originally-declared `|param|` is unpacked from `ArgumentsList`
  *     positionally, mirroring [[AddBuiltinBehaviourPass.unpackArgumentsList]]
  *     (same shape, just reading the capitalized `ArgumentsList`/`this` names
  *     this convention uses instead of that pass's lowercase `argumentsList`/
  *     `thisArgument`, a different convention for hoisted closures rather than
  *     top-level interface members).
  *
  * `**this**` compiles directly to the same local the `|this|` parameter
  * declares (see `esmeta.wji.compiler.Compiler`'s `Expr.This` case), so simply
  * declaring `|this|` as a parameter is already enough to bind it. That holds
  * for a `Constructor` too: the `NewTarget` check and allocating the object are
  * WebIDL's own "create an interface object" steps (`webidl/index.bs`,
  * "internally create a new object implementing the interface" before "Perform
  * the constructor steps ... with object as this"), outside the
  * constructor-steps text itself, so the constructor steps just receive the
  * already-created object as `this` and never see `NewTarget` at all.
  *   - '''Completion-record wrapping''': every exit path must return a real
  *     Completion Record, same as [[AddBuiltinBehaviourPass]]'s own reason
  *     (mainline's Call machinery always expects one back, regardless of
  *     whether the algorithm itself can abruptly complete).
  *     [[CompletionAlgorithms.compute]] seeds `returnsCompletion = true`
  *     unconditionally for every interface member — every kind shares the same
  *     unconditional calling-convention requirement — so
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

  /** the parameters each kind's steps closure is called with — see class doc's
    * "parameters" item.
    */
  private def builtinParams(kind: AlgorithmKind): List[WjiParam] = kind match
    case AlgorithmKind.Getter(_) | AlgorithmKind.NamespaceGetter(_) =>
      List(WjiParam("|this|"))
    case AlgorithmKind.Setter(_) =>
      List(WjiParam("|this|"), WjiParam("|givenValue|"))
    case _ =>
      List(WjiParam("|this|"), WjiParam("|ArgumentsList|"))

  private def stripPipes(s: String): String =
    s.stripPrefix("|").stripSuffix("|")

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

  /** the `params.zipWithIndex` prefix instructions that unpack `ArgumentsList`
    * positionally into the algorithm's own originally-declared parameter names
    * — mirrors [[AddBuiltinBehaviourPass.unpackArgumentsList]] (see that
    * method's own doc); duplicated rather than shared since the two conventions
    * use differently-cased names for the list itself.
    *
    * `ArgumentsList` is the `values` list WebIDL's own caller of the steps
    * closure got back from its "overload resolution algorithm"
    * (`manuals/funcs/overload_resolution_algorithm.ir`): one entry per declared
    * param, each already converted to its declared IDL type, with a missing
    * optional argument already replaced by `undefined` or its default — so
    * nothing but the binding itself (and [[nonObjectCheck]]) is left to do
    * here. Converting the return value back to a JavaScript value, and
    * rejecting instead of throwing for a promise-typed operation, are that
    * same caller's own steps too.
    */
  private def unpackArgumentsList(params: List[WjiParam]): List[Instr] =
    params.zipWithIndex.flatMap {
      case (p, i) =>
        val name = stripPipes(p.name)
        Instr.Let(
          Expr.Var(name),
          Expr.Index(Expr.Var("ArgumentsList"), Expr.Num(i.toString)),
        ) :: p.idlType.toList.flatMap(nonObjectCheck(_, name))
    }

  /** The matching epilogue for an algorithm whose spec prose never ends in an
    * explicit `Return`, appended unconditionally after the body:
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
            AlgorithmKind.NamespaceMethod(_) |
            AlgorithmKind.NamespaceGetter(_) =>
          a.copy(
            params = builtinParams(a.kind),
            body = unpackArgumentsList(a.params) ++ a.body ++ returnEpilogue(a),
          )
        case AlgorithmKind.Plain => a
    }
