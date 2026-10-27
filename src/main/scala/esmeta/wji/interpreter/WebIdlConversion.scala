package esmeta.wji.interpreter

import esmeta.cfg.{CFG, Call}
import esmeta.es.builtin.symbolAddr
import esmeta.error.{NoAddr, NoList, NoMathValue, UnknownConversion}
import esmeta.interpreter.{%%, Interpreter}
import esmeta.ir.{GLOBAL_EXECUTION_STACK, Name}
import esmeta.parser.ESValueParser
import esmeta.state.*
import esmeta.ty.AbruptT

/** Native Scala dispatch target for [[esmeta.ir.Inst.ICallConvert]] — WebIDL's
  * "converted to an IDL value" / "converted to a JavaScript value" abstract
  * operations, ported 1:1 from the former
  * `manuals/funcs/converted_to_an_idl_value.ir` /
  * `converted_to_a_javascript_value.ir` stubs. Same scope as before the port
  * (see `docs/hardcodes.md` #1/#2), since extended to cover: `"unsigned long"`;
  * the WebAssembly dictionaries `MemoryDescriptor`/`TableDescriptor`/
  * `GlobalDescriptor`/`TagType`/`ExceptionOptions`/`WebAssemblyCompileOptions`
  * (via `readDictionary`); the interface types `Module`/`Tag` (via
  * `implementsInterface`); a bare `sequence<T>` parameter (via `toSequence`);
  * and the union type `AllowSharedBufferSource` (via `isBufferSource`). Every
  * other IDL type not listed above is still identity passthrough — leaving
  * `options`/whatever the raw ECMAScript value as-is means `exists`/`Get` on
  * one of its members only behaves correctly for internal slots (a
  * `RecordObj`'s own top-level fields), not real object properties (which live
  * in the nested `"__MAP__"` field's own `MapObj`) — see
  * `webAssemblyCompileOptionsMembers`'s own doc for a case where exactly this
  * bit a not-yet-converted dictionary. Dictionary member reads go through a
  * real `Get` (prototype chain, getters, and any exception a getter throws all
  * work), and a required member found absent throws a real `TypeError` right
  * there — see `readDictionary` and `Interpreter.invokeCallable`.
  */
object WebIdlConversion:

  /** `converted_to_an_idl_value` returns the plain converted `Value` on
    * success, same as before (`webidl/index.bs`'s real `react` algorithm --
    * `Promise.prototype.then`-style reaction handling -- has its own genuine,
    * unmarked (no `[=?=]`) "Let value be the result of ... converting ..." call
    * site, extracted as ordinary spec text rather than synthesized by
    * `AddInterfaceMemberBuiltinBehaviourPass`; wrapping every result in
    * `NormalCompletion` unconditionally, tried first, broke exactly that call
    * site -- nothing there expected or unwrapped a completion). Only the
    * *abrupt* case -- a required member found absent, an invalid enum value, or
    * a getter/`toString` that itself threw -- returns a genuine
    * `ThrowCompletion` Record instead of the plain value; a caller that cares
    * (`manuals/funcs/overload_resolution_algorithm.ir`'s per-argument
    * conversion, `AddInstantiateOverloadDispatchPass.convert`) tells the two
    * apart with an abrupt-completion check, which is false for every ordinary
    * converted value (a `MapObj` address, `Number`, `Str`, ... -- none of them
    * a `CompletionRecord`). `converted_to_a_javascript_value` can't throw
    * either (unlike `Get`, `CreateArrayFromList` -- the one reentrant call
    * `toJsValue` itself makes, see its own doc -- is always `!`-marked in real
    * spec text, never abrupt) and was never affected by the completion-wrapping
    * question above.
    */
  def call(
    interp: Interpreter,
    callSite: Call,
    fname: String,
    args: List[Value],
  ): Value =
    val st = interp.st
    (fname, args) match
      case ("converted_to_an_idl_value", List(argument, ty)) =>
        toIdlValue(interp, callSite, argument, ty) match
          case Right(value) => value
          case Left(abrupt) => abrupt
      case ("converted_to_a_javascript_value", List(argument)) =>
        toJsValue(interp, callSite, argument)
      case _ => throw UnknownConversion(fname)

  /** invokes the real ECMA-262 `Get(O, P)` abstract operation, reentrantly (see
    * [[Interpreter.invokeCallable]]) — so a dictionary member backed by an
    * accessor property actually runs its getter, prototype-chain lookups work,
    * and a getter that itself throws produces a real abrupt Completion Record
    * rather than a native crash reading a nonexistent `"Value"` field.
    */
  private def getProperty(
    interp: Interpreter,
    callSite: Call,
    obj: Value,
    key: Value,
  ): Value =
    interp.invokeCallable(
      Clo(interp.st.cfg.getFunc("Get"), Map.empty),
      List(obj, key),
      callSite,
    )

  /** invokes the real ECMA-262 `ToString(V)` abstract operation, reentrantly —
    * same rationale as [[getProperty]]: a `DOMString`/enum-typed member's raw
    * value might be an object with its own `toString`/`valueOf` (which could
    * itself throw), not already a plain string.
    */
  private def toStringValue(
    interp: Interpreter,
    callSite: Call,
    v: Value,
  ): Value =
    interp.invokeCallable(
      Clo(interp.st.cfg.getFunc("ToString"), Map.empty),
      List(v),
      callSite,
    )

  /** invokes the real ECMA-262 `ToNumber(V)`/`ToBigInt(V)` abstract operations,
    * reentrantly — same rationale as [[toStringValue]]. Idempotent on an
    * already-`Number`/`BigInt` input (mainline's own implementation returns
    * immediately without ever reaching `ToPrimitive`/`valueOf`), which is
    * exactly what makes [[readDictionary]]'s `AddressValue` case below safe to
    * call ahead of `AddressValueToU64`'s own later, real one — see that case's
    * own doc.
    */
  private def toNumberValue(
    interp: Interpreter,
    callSite: Call,
    v: Value,
  ): Value =
    interp.invokeCallable(
      Clo(interp.st.cfg.getFunc("ToNumber"), Map.empty),
      List(v),
      callSite,
    )
  private def toBigIntValue(
    interp: Interpreter,
    callSite: Call,
    v: Value,
  ): Value =
    interp.invokeCallable(
      Clo(interp.st.cfg.getFunc("ToBigInt"), Map.empty),
      List(v),
      callSite,
    )

  /** invokes the real ECMA-262 `ToBoolean(V)` abstract operation, reentrantly
    * -- same rationale as [[toStringValue]]. Unlike `ToString`/`ToNumber`/
    * `ToBigInt`, `ToBoolean` is a total function over every value type (never
    * reaches `ToPrimitive`/`valueOf`, never throws), so its result is never a
    * completion record -- no `isAbrupt` check needed at the call site.
    */
  private def toBooleanValue(
    interp: Interpreter,
    callSite: Call,
    v: Value,
  ): Value =
    interp.invokeCallable(
      Clo(interp.st.cfg.getFunc("ToBoolean"), Map.empty),
      List(v),
      callSite,
    )

  private def isAbrupt(st: State, v: Value): Boolean =
    AbruptT.contains(v, st.heap)

  /** pins values accumulated by a native Scala buffer across reentrant
    * `invokeCallable` calls (`readDictionary`'s `pairs`/`readEnumSequence`'s
    * `converted`) so a `GC` firing mid-loop can't sweep an already-converted
    * member before this function gets a chance to store it anywhere `GC` itself
    * walks (`personal/TODO.md` #67's root cause). Rather than extending
    * `Interpreter`/`GC` with a notion of "extra roots", this binds a real
    * heap-allocated `ListObj` into `st.context.locals` -- `GC` already walks
    * `st.context` (`UnitWalker.walk(context)` covers `locals`), and
    * `st.context` is exactly the object `Interpreter.invokeCallable` saves and
    * restores intact across reentrant calls, however deep -- so this needs no
    * mainline change at all. `sync` is called after every accumulation step to
    * keep the pinned list's contents current. A nested dictionary/sequence
    * conversion (e.g. `WebAssemblyCompileOptions.builtins`) shadows an outer
    * pin under the same key, so the inner list also holds the outer one, and
    * the outer one is restored in `finally`.
    */
  private def withGcPin[T](
    interp: Interpreter,
  )(body: (Iterable[Value] => Unit) => T): T =
    val st = interp.st
    val pinVar = Name("__wjiGcPin")
    val previous = st.context.locals.get(pinVar)
    // holds the outer pin, which would otherwise be unreachable (and
    // collected) while this one shadows it
    val pinAddr = st.allocList(previous.toList)
    st.context.locals += pinVar -> pinAddr
    def sync(values: Iterable[Value]): Unit = st(pinAddr) match
      case l: ListObj => l.values = (previous ++ values).toVector
      case _          => ()
    try body(sync)
    finally
      previous match
        case Some(v) => st.context.locals += pinVar -> v
        case None    => st.context.locals -= pinVar

  /** WebIDL's "implements" (`webidl/index.bs:13799-13802`), the same check
    * `ExpandImplementsPass` compiles `Cond.Implements` into: `v` is a platform
    * object (an Object with a `[[PrimaryInterface]]` slot, set by "internally
    * create a new object implementing the interface" -- its `tname` is just
    * `MakeBasicObject`'s generic `"Object"`) and `iface` is among the
    * [=inclusive inherited interfaces=] of that slot's value, walked via each
    * interface record's `inherit` field exactly like
    * `inclusive_inherited_interfaces` does (see `Initialize.seedHostDefined`).
    * Native here since it's a pure heap walk, no reentrant call needed.
    */
  private def implementsInterface(st: State, v: Value, iface: String): Boolean =
    def inherits(interface: Value): Boolean = interface match
      case addr: Addr =>
        st(addr) match
          case r: RecordObj =>
            r.get("id") == Some(Str(iface)) ||
            r.get("inherit").exists(inherits)
          case _ => false
      case _ => false
    v match
      case addr: Addr =>
        st(addr) match
          case r: RecordObj if r.tname != "Symbol" =>
            r.get("PrimaryInterface").exists(inherits)
          case _ => false
      case _ => false

  /** `AllowSharedBufferSource`'s own union-membership test -- see that case in
    * `toIdlValue` for why this checks slot presence rather than `tname`.
    */
  private def isBufferSource(st: State, v: Value): Boolean = v match
    case addr: Addr =>
      st(addr) match
        case r: RecordObj =>
          r.map.contains("ArrayBufferData") || r.map.contains(
            "ViewedArrayBuffer",
          )
        case _ => false
    case _ => false

  /** WebIDL's plain `object` type: `Type(V) is Object` (ECMA-262 Type(V), the
    * seven-way language-type tag) -- true iff `v` is a genuine ECMAScript
    * object. A real object is always heap-allocated as a `RecordObj` in this
    * state model (the same shape `implementsInterface`/ `isBufferSource` above
    * key off); `ListObj`/`MapObj` are Infra-spec-only internal structures never
    * exposed as a JS value directly, so a `v` pointing at one of those would be
    * a WJI-internal bug reaching here, not a real "is it an object" case to
    * accept. Symbol values are ALSO `RecordObj`s in this state model (`SymbolT
    * = RecordT("Symbol")`, `ty/package.scala`), but `Type(Symbol())` is
    * `Symbol`, not `Object` -- so `tname == "Symbol"` must be excluded here.
    */
  private def isObjectValue(st: State, v: Value): Boolean = v match
    case addr: Addr =>
      st(addr) match
        case r: RecordObj => r.tname != "Symbol"
        case _            => false
    case _ => false

  /** builds a genuine `ThrowCompletion(TypeError)`, the same two-step idiom
    * `manuals/funcs/ConvertToInt.ir` and `CompletionWrapping`'s compiled output
    * both use (`__NEW_ERROR_OBJ__` then `ThrowCompletion`) — reused here so a
    * required dictionary member that's absent throws for real, from the same
    * place that already knows "absent" (own-property missing, or present but
    * `Get` returned `undefined`) — see `readDictionary`.
    */
  private def typeError(interp: Interpreter, callSite: Call): Value =
    val errObj = interp.invokeCallable(
      Clo(interp.st.cfg.getFunc("__NEW_ERROR_OBJ__"), Map.empty),
      List(Str("%TypeError.prototype%")),
      callSite,
    )
    interp.invokeCallable(
      Clo(interp.st.cfg.getFunc("ThrowCompletion"), Map.empty),
      List(errObj),
      callSite,
    )

  // ── converted to an IDL value ──────────────────────────────────────────────

  /** one dictionary member: its name, whether WebIDL declares it `required`
    * (absent -- including present-but-`undefined`, per WebIDL's own
    * undefined-collapsing -- throws a real `TypeError` rather than silently
    * omitting it, see `readDictionary`), IDL default (`None` for a `required`
    * member or a non-required one with no declared default -- either way,
    * absent-and-not-required just means left out of the result), whether its
    * own IDL type is `sequence<T>` (so the raw value, if present, needs
    * converting from a JS array-like into a real internal `List` before it's
    * usable by anything past this point -- see `toSequence`), and, if it's a
    * WebIDL enum (a WebAssembly `ValueType`/`TableKind`/`AddressType` name like
    * `"i32"`/`"anyfunc"`, e.g. `TableDescriptor.element`), its own set of
    * allowed values. WebIDL enum conversion is `? ToString(V)` (so a non-string
    * argument's own `toString`/`valueOf` still runs, invoked for its
    * side-effects even -- see `readDictionary`) followed by checking the result
    * against exactly this set, throwing a `TypeError` for anything else
    * (`spectec/test/js-api/memory/constructor.any.js`'s `{ "address": "none"
    * }`, expecting `TypeError`) -- skipping the check would let an invalid
    * value flow into `ToValueType`/similar downstream algorithms, which just
    * compare against their own literal strings and assert unreachable
    * otherwise, crashing natively instead.
    */
  private case class Member(
    name: String,
    required: Boolean = false,
    default: Option[Value] = None,
    isSequence: Boolean = false,
    enumValues: Option[Set[String]] = None,
    isAddressValue: Boolean = false,
    isBoolean: Boolean = false,
  )

  private val addressTypeValues = Set("i32", "i64")
  private val tableKindValues = Set("externref", "anyfunc")
  private val valueTypeValues =
    Set("i32", "i64", "f32", "f64", "v128", "externref", "anyfunc")

  // Listed in each dictionary's *own constructor algorithm's* first-reference
  // order, not IDL declaration order or alphabetical order -- see
  // `readDictionary`'s own doc for why (`address` before `initial`/`maximum`,
  // matching `Memory`'s "If |descriptor|["address"] ... Let |initial| be ...
  // AddressValueToU64(|descriptor|["initial"], ...)").
  private val memoryDescriptorMembers = List(
    Member("address", enumValues = Some(addressTypeValues)),
    Member("initial", required = true, isAddressValue = true),
    Member("maximum", isAddressValue = true),
  )
  // `Table`'s constructor reads `element` before `address` (index.bs:1037-1040
  // -- "Let |elementtype| be ... ToValueType(|descriptor|["element"]). ...
  // If |descriptor|["address"] ..."), unlike `Memory`/`Global` where the
  // algorithm's own order happens to already be alphabetical -- confirmed
  // against real Node (`node --experimental-wasm-*` not even needed, plain
  // `WebAssembly.Table` already gives this order) and
  // `spectec/test/js-api/table/constructor.any.js`'s own "Order of evaluation
  // for descriptor" expects exactly `[element, ..., address, ..., initial,
  // ..., maximum, ...]`. Whichever member came first in IDL declaration order
  // (`dictionary TableDescriptor { required TableKind element; required
  // AddressValue initial; AddressValue maximum; AddressType address; }`)
  // isn't it either -- `address` is declared *last* there, but read *second*
  // by the algorithm.
  private val tableDescriptorMembers = List(
    Member("element", required = true, enumValues = Some(tableKindValues)),
    Member("address", enumValues = Some(addressTypeValues)),
    Member("initial", required = true, isAddressValue = true),
    Member("maximum", isAddressValue = true),
  )
  // `boolean mutable = false;` -- the only member across these four
  // dictionaries with an actual IDL default (the js-api spec's other
  // non-required members have none, so an absent one is correctly left out
  // of the result entirely -- see `readDictionary`). Listed `mutable` before
  // `value` -- `Global`'s own constructor (index.bs:1191-1192) reads
  // `|descriptor|["mutable"]` a full step before `|descriptor|["value"]`.
  private val globalDescriptorMembers = List(
    Member("mutable", default = Some(Bool(false)), isBoolean = true),
    Member("value", required = true, enumValues = Some(valueTypeValues)),
  )
  // `required sequence<ValueType> parameters;` -- element-wise ValueType
  // enum validation (see `Member.enumValues`'s doc), same as a scalar
  // enum-typed member -- an invalid element (e.g. `"i16"`,
  // `spectec/test/js-api/tag/constructor.tentative.any.js`'s "Invalid type
  // parameter") must throw `TypeError` here rather than flow unvalidated into
  // `ToValueType`, which just asserts unreachable and crashes natively.
  private val tagTypeMembers =
    List(
      Member(
        "parameters",
        required = true,
        isSequence = true,
        enumValues = Some(valueTypeValues),
      ),
    )
  // `boolean traceStack = false;` -- `Exception`'s constructor's third
  // parameter (`optional ExceptionOptions options = {}`), no required members.
  private val exceptionOptionsMembers =
    List(Member("traceStack", default = Some(Bool(false)), isBoolean = true))

  // `dictionary WebAssemblyCompileOptions { USVString? importedStringConstants;
  // sequence<USVString> builtins; };` (index.bs:364-367) -- `Module`'s
  // constructor and the `compile`/`instantiate`/`validate` namespace
  // operations' shared last parameter. Neither member has an IDL default, so
  // both are correctly left out of the converted dictionary entirely when
  // absent (`Member.default = None`, `required = false`) -- `SpecPatch` #17's
  // own `[=map/exists=]` guard on each is what depends on that (`docs/
  // spec_errors.md` #12). Listed in the constructor algorithm's own
  // first-reference order (`builtins` before `importedStringConstants`,
  // index.bs:100-105), not IDL declaration order (which is reversed).
  //
  // Previously left as identity passthrough like every other unlisted IDL
  // type, which left `options` as the raw JS object -- `exists options.
  // builtins` (`esmeta.state.Obj.exists`) only ever checks a `RecordObj`'s own
  // top-level field map, never a real object's actual own properties (those
  // live in the nested `"__MAP__"` field's own `MapObj`), so the check was
  // always false regardless of what was actually passed. Routing this through
  // `readDictionary` (which builds a real `MapObj` via `st.allocMap`, keyed
  // directly by member name) makes `exists options.builtins` correct the same
  // way it already is for `MemoryDescriptor`/etc.
  private val webAssemblyCompileOptionsMembers = List(
    Member("builtins", isSequence = true),
    Member("importedStringConstants"),
  )

  /** `ty` names the declared IDL type — almost always a literal `Str` (a param
    * record's `ty`, `Initialize.seedHostDefined`'s `paramRecord`, read by
    * `overload_resolution_algorithm.ir`), but a direct spec-text "converted to
    * an IDL value of type X" reference (e.g. inside a hoisted `react` closure)
    * compiles the type name as `Expr.SpecTerm`/`EEnum` instead. Matching on
    * either shape (never coercing/crashing on a third one) mirrors the old
    * `.ir` version's own tolerance: `if (= T "...")` just evaluates false —
    * never throws — for a `T` that isn't the literal string it expects.
    */
  def toIdlValue(
    interp: Interpreter,
    callSite: Call,
    argument: Value,
    ty: Value,
  ): Either[Value, Value] = ty match
    case Str("unsigned long") | Enum("unsigned long") =>
      toUnsignedLong(interp, callSite, argument)
    case Str("MemoryDescriptor") | Enum("MemoryDescriptor") =>
      toDictionary(interp, callSite, argument, memoryDescriptorMembers)
    case Str("TableDescriptor") | Enum("TableDescriptor") =>
      toDictionary(interp, callSite, argument, tableDescriptorMembers)
    case Str("GlobalDescriptor") | Enum("GlobalDescriptor") =>
      toDictionary(interp, callSite, argument, globalDescriptorMembers)
    case Str("TagType") | Enum("TagType") =>
      toDictionary(interp, callSite, argument, tagTypeMembers)
    case Str("ExceptionOptions") | Enum("ExceptionOptions") =>
      toDictionary(interp, callSite, argument, exceptionOptionsMembers)
    case Str("WebAssemblyCompileOptions") | Enum("WebAssemblyCompileOptions") =>
      toDictionary(
        interp,
        callSite,
        argument,
        webAssemblyCompileOptionsMembers,
      )
    // an interface-typed argument (`Module.{exports,imports,customSections}`'s
    // `moduleObject`, and `Exception`'s constructor's `exceptionTag`): real
    // WebIDL interface-type conversion requires the value to actually
    // implement the named interface, throwing `TypeError` otherwise --
    // `implementsInterface` is the same `[[PrimaryInterface]]` check
    // `ExpandImplementsPass` compiles `Cond.Implements` into, just run
    // natively here since there's no reentrant call needed for it (no
    // getter/JS execution involved).
    case Str("Module") | Enum("Module") =>
      if implementsInterface(interp.st, argument, "Module") then Right(argument)
      else Left(typeError(interp, callSite))
    case Str("Tag") | Enum("Tag") =>
      if implementsInterface(interp.st, argument, "Tag") then Right(argument)
      else Left(typeError(interp, callSite))
    // `typedef (ArrayBuffer or SharedArrayBuffer or [AllowShared]
    // ArrayBufferView) AllowSharedBufferSource` (webidl/index.bs:15087) --
    // unlike `Module`/`Tag` above, a union of *built-in* ES types, which
    // aren't platform objects (no `[[PrimaryInterface]]` slot
    // `implementsInterface` could key off) -- so this checks the same
    // internal slot `get_a_copy_of_the_buffer_source`
    // (webidl/index.bs:9312-9327) itself keys off: `[[ArrayBufferData]]` for
    // an ArrayBuffer/SharedArrayBuffer, `[[ViewedArrayBuffer]]` for any typed
    // array/DataView view. Without this, an invalid argument (a plain
    // `Number`, `{}`, the `ArrayBuffer` constructor itself, ...) sailed
    // straight through as identity passthrough into that algorithm's own
    // internal `Assert`, which -- being `A || (yet "{{SharedArrayBuffer}}
    // object")` -- can never actually fail (`IAssert` catches *any*
    // `Throwable` from evaluating an assert expression as "not yet compiled,
    // skip", so the `yet` branch silently no-ops the whole assert instead of
    // surfacing the rejection) and so never threw the `TypeError` real
    // engines do here.
    case Str("AllowSharedBufferSource") | Enum("AllowSharedBufferSource") =>
      if isBufferSource(interp.st, argument) then Right(argument)
      else Left(typeError(interp, callSite))
    // plain WebIDL `object` -- every occurrence in this corpus is `optional
    // object importObject` with no default (index.bs:375/378, 779). Without
    // this case, a non-object `importObject` (a number, a string, ...)
    // sailed through as identity passthrough; `read the imports` only ever
    // checks `importObject === undefined`, and an empty-imports module never
    // even reaches a `Get` on it, so the bogus argument was silently
    // accepted instead of throwing the `TypeError` real WebIDL "convert
    // ECMAScript value to object" conversion requires (`1. If Type(V) is not
    // Object, throw a TypeError.`). `Undef` is special-cased to pass through
    // rather than throw: `overload_resolution_algorithm.ir` already binds
    // `undefined` without converting for an omitted or explicitly-`undefined`
    // optional argument, but `AddInstantiateOverloadDispatchPass.convert`
    // converts `instantiate`'s `importObject` unconditionally, so an
    // `undefined` one does reach here -- and the
    // spec text itself (`read the imports`, index.bs:485: "If ... and
    // |importObject| is undefined, throw a TypeError exception" -- only when
    // the module actually has imports) treats that as a legitimate, later-
    // checked value, not something rejected at this conversion step.
    // Confirmed against `instance/constructor.any.js`'s own "Empty module
    // with undefined imports argument" subtest, which explicitly passes
    // `undefined` and expects success.
    case Str("object") | Enum("object") =>
      argument match
        case Undef => Right(argument)
        case _ =>
          if isObjectValue(interp.st, argument) then Right(argument)
          else Left(typeError(interp, callSite))
    // a bare `sequence<T>` parameter (as opposed to one nested inside a
    // dictionary, see `Member.isSequence`) -- so far only
    // `Exception`'s constructor's `sequence<any> payload`. Matched by prefix
    // rather than the exact element type, same "identity passthrough for the
    // element type" simplification as everywhere else in this object.
    case Str(t) if t.startsWith("sequence<") =>
      toSequence(interp, callSite, argument)
    case Enum(t) if t.startsWith("sequence<") =>
      toSequence(interp, callSite, argument)
    case _ => Right(argument)

  /** WebIDL dictionary conversion's own first step, "If V is not undefined,
    * null or an Object, then throw a TypeError", ahead of [[readDictionary]]
    * -- whose `Get` calls would crash natively on anything else (a `false`/
    * number/string/`Symbol()` argument, `memory/constructor.any.js`'s "Invalid
    * descriptor argument"). Used to be a separate guard injected after argument
    * unpacking (`AddInterfaceMemberBuiltinBehaviourPass`'s former
    * `nonObjectCheck`), which broke once `overload_resolution_algorithm.ir`
    * started converting arguments first: by then the value is already the
    * converted dictionary (a `MapObj`), never an Object.
    */
  private def toDictionary(
    interp: Interpreter,
    callSite: Call,
    argument: Value,
    members: List[Member],
  ): Either[Value, Value] = argument match
    case Undef | Null => readDictionary(interp, callSite, argument, members)
    case _ if isObjectValue(interp.st, argument) =>
      readDictionary(interp, callSite, argument, members)
    case _ => Left(typeError(interp, callSite))

  private def toMathValue(v: Value): Math = v match
    case n: Math   => n
    case Number(d) => Math(d)
    case Str(s)    => Math(ESValueParser.str2number(s).double)
    case v         => throw NoMathValue(v)

  /** `[EnforceRange] unsigned long` conversion -- so far only `Exception.
    * prototype.getArg`'s `index` param (`index.bs:1681`), the sole "unsigned
    * long" occurrence in this corpus as an actual WebIDL param type, and always
    * `[EnforceRange]` there -- so, unlike a scalar enum member's `? ToString`,
    * this reuses `manuals/funcs/ConvertToInt.ir` (the same `? ToNumber` +
    * NaN/±Infinity/range `TypeError` logic `AddressValueToU64` already calls,
    * `docs/hardcodes.md` #14) rather than reimplementing it here a second time.
    * `bitLength`/`signedness`/`extendedAttribute` are accepted but ignored by
    * that function (it only ever implements the one combination both call sites
    * need: 32-bit, unsigned, EnforceRange), so their exact values here don't
    * matter beyond documenting intent.
    */
  private def toUnsignedLong(
    interp: Interpreter,
    callSite: Call,
    argument: Value,
  ): Either[Value, Value] =
    val result = interp.invokeCallable(
      Clo(interp.st.cfg.getFunc("ConvertToInt"), Map.empty),
      List(argument, Math(32), Str("unsigned"), Str("EnforceRange")),
      callSite,
    )
    if isAbrupt(interp.st, result) then Left(result)
    else Right(interp.st(result, Str("Value")))

  /** reads `members` off `argument` via a real `Get(argument, key)` for each —
    * prototype chain and accessor properties (getters) both work, mirroring
    * WebIDL dictionary conversion's own "Let value be ? Get(V, key)." step.
    * `Get` returning `undefined` (own property absent, or present but actually
    * `undefined` -- WebIDL's own undefined-collapsing treats both the same)
    * means the member is absent: a `required` member (e.g.
    * `TableDescriptor.element`) throws a real `TypeError` right here — the only
    * place that actually knows "absent" after undefined-collapsing, so a
    * separate compile-time own-property check can't substitute for it (that
    * used to be `AddInterfaceMemberBuiltinBehaviourPass.requiredMemberChecks`,
    * now removed) — a non-required member instead fills in `Member.default` if
    * it has one (so far only `GlobalDescriptor.mutable = false`) or is simply
    * left out of the result.
    *
    * `argument` being `undefined`/`null` is a real, common case (an omitted or
    * explicitly-`undefined` dictionary argument -- WebIDL treats either the
    * same as an empty ordinary object `{}`), not an error: every member below
    * reads as absent rather than calling `Get` on a non-object base.
    *
    * Stops at the first member that's abrupt -- either a required member found
    * absent, or a getter that itself threw -- and propagates that completion as
    * `Left` instead of reading any later member, mirroring how a real
    * `?`-marked step sequence would never reach its later steps either.
    *
    * Collects `(key, value)` pairs into a plain Scala buffer and only builds
    * the real heap `MapObj` once, at the very end, via a single `st.allocMap`,
    * rather than allocating it up front and filling it in as we go — nothing
    * here needs an unfinished dictionary to be independently reachable
    * mid-loop, and building it in one shot is simpler than threading a
    * partially-filled `Addr` through every branch below. (`Interpreter.
    * invokeCallable`'s own `suspendedFrames` is what actually keeps a value
    * like this alive across `getProperty`/`toStringValue`'s reentrant calls
    * either way -- this ordering is not load-bearing for that.)
    */
  private def readDictionary(
    interp: Interpreter,
    callSite: Call,
    argument: Value,
    members: List[Member],
  ): Either[Value, Value] =
    val st = interp.st
    var abrupt: Option[Value] = None
    val pairs = scala.collection.mutable.ListBuffer.empty[(Value, Value)]
    // Textbook WebIDL "convert ECMAScript value to dictionary" reads members
    // in lexicographic (alphabetical) order -- tried first here, and it even
    // fit `Memory`/`Global`'s own observable order (`spectec/test/js-api/
    // memory/constructor.any.js`'s "Order of evaluation for descriptor"
    // reads "address"/"mutable" before their alphabetically-later siblings).
    // But `Table`'s own version of that same test reads `element` — declared
    // *after* `initial`/`maximum`/`address` in the IDL, and not even
    // alphabetically first among the four — *before* any of them: confirmed
    // empirically against real Node, and it's exactly index.bs:1037's own
    // first constructor step ("Let |elementtype| be ...
    // ToValueType(|descriptor|["element"])"). So it isn't dictionary-member
    // order at all; each `descriptor["key"]` is resolved lazily, in the
    // constructor algorithm's *own* prose reference order for that
    // particular dictionary -- alphabetical for `Memory`/`Global` only
    // because their algorithms happen to read members in that order too.
    // `memoryDescriptorMembers`/`tableDescriptorMembers`/
    // `globalDescriptorMembers` above are each listed in exactly that
    // per-algorithm order already, so plain list order is what to use here,
    // not a generic re-sort.
    val it = members.iterator
    // `pairs` accumulates across this loop's own repeated reentrant calls
    // (`getProperty`/`toStringValue`/`toBigIntValue`/`toNumberValue`/
    // `toBooleanValue`, each a real `invokeCallable`) -- a GC firing mid-loop
    // must see everything converted *so far*, or it can collect an already-
    // converted member's value before this function ever gets to store it
    // somewhere `GC` itself tracks (`personal/TODO.md` #67's root cause: `pin`
    // below keeps a real heap root current with exactly that, see
    // `withGcPin`'s own doc).
    withGcPin(interp) { pin =>
      def addPair(key: Value, value: Value): Unit =
        pairs += key -> value
        pin(pairs.flatMap((k, v) => List(k, v)))
      while abrupt.isEmpty && it.hasNext do
        val member = it.next()
        val key = Str(member.name)
        def absent(): Unit = member.default match
          case Some(d) => addPair(key, d)
          case None if member.required =>
            abrupt = Some(typeError(interp, callSite))
          case None => ()
        argument match
          case Undef | Null => absent()
          case _ =>
            val result = getProperty(interp, callSite, argument, key)
            if isAbrupt(st, result) then abrupt = Some(result)
            else
              st(result, Str("Value")) match
                case Undef => absent()
                case raw if member.isSequence && member.enumValues.isDefined =>
                  readEnumSequence(
                    interp,
                    callSite,
                    raw,
                    member.enumValues.get,
                  ) match
                    case Left(a)     => abrupt = Some(a)
                    case Right(list) => addPair(key, list)
                case raw if member.enumValues.isDefined =>
                  val strResult = toStringValue(interp, callSite, raw)
                  if isAbrupt(st, strResult) then abrupt = Some(strResult)
                  else
                    st(strResult, Str("Value")) match
                      case s @ Str(v) if member.enumValues.get(v) =>
                        addPair(key, s)
                      case _ => abrupt = Some(typeError(interp, callSite))
                // `AddressValue` (`initial`/`maximum`) is declared `any` --
                // real WebIDL "any" conversion is pure identity, no side effect
                // -- but `spectec/test/js-api/{memory,table}/constructor.any.js`'s
                // "Order of evaluation for descriptor" empirically shows real
                // engines resolving each member's `valueOf`/`toBigInt` inline,
                // right where that member itself is read (in `members`' own
                // algorithm-reference order, see that list's own doc), not
                // deferred until whatever later step happens to consume it --
                // confirmed directly against Node before writing this. `pairs`
                // (built so far, in that same single pass) already has
                // `address` converted by this point in both `Memory`/`Table` --
                // `memoryDescriptorMembers`/`tableDescriptorMembers` both list
                // it before `initial`/`maximum` for exactly this reason --
                // default to "i32" per the constructor's own "otherwise let
                // addrtype be 'i32'" when it's genuinely absent.
                // `ToNumber`/`ToBigInt` (not the full `AddressValueToU64` --
                // that's still called for real, later, by the algorithm body
                // itself) are enough to reproduce the observed side effect: both
                // are idempotent identity on an already-Number/BigInt input (no
                // `ToPrimitive`/`valueOf` reached a second time), so storing the
                // coerced primitive here just makes the *first* call the one
                // that actually runs `valueOf` -- `AddressValueToU64`'s own
                // later `? ToNumber`/`? ToBigInt` still does its real
                // EnforceRange/range-check work on that primitive, unaffected.
                case raw if member.isAddressValue =>
                  val addrtype = pairs
                    .collectFirst {
                      case (Str("address"), Str(v)) => v
                    }
                    .getOrElse("i32")
                  val coerced =
                    if addrtype == "i64" then
                      toBigIntValue(interp, callSite, raw)
                    else toNumberValue(interp, callSite, raw)
                  if isAbrupt(st, coerced) then abrupt = Some(coerced)
                  else addPair(key, st(coerced, Str("Value")))
                // `boolean` (`GlobalDescriptor.mutable`/`ExceptionOptions.
                // traceStack`) -- real WebIDL boolean conversion is `ToBoolean`,
                // not identity, so a truthy-but-not-literal-`true` value (e.g.
                // `mutable: 1`/`mutable: "x"`) must still convert to `true`
                // rather than being compared against a `true` literal later and
                // silently losing.
                case raw if member.isBoolean =>
                  addPair(key, toBooleanValue(interp, callSite, raw))
                case raw if member.isSequence =>
                  toSequence(interp, callSite, raw) match
                    case Left(a)     => abrupt = Some(a)
                    case Right(list) => addPair(key, list)
                case raw => addPair(key, raw)
    }
    abrupt match
      case Some(a) => Left(a)
      case None    => Right(st.allocMap(pairs.toList))

  /** WebIDL's ES-to-IDL `sequence<T>` conversion ("create a sequence from an
    * iterable"): a non-object, or an object with no `@@iterator`, is a
    * TypeError; otherwise the elements come from the real iterator protocol
    * (`GetMethod`, `GetIteratorFromMethod`, `IteratorToList`, reentrantly), so
    * any iterable works and a throwing iterator propagates. `Left` is the
    * abrupt completion.
    */
  private def iterateSequence(
    interp: Interpreter,
    callSite: Call,
    value: Value,
  ): Either[Value, List[Value]] =
    val st = interp.st
    def callAO(name: String, args: List[Value]): Value =
      interp.invokeCallable(
        Clo(st.cfg.getFunc(name), Map.empty),
        args,
        callSite,
      )
    if !isObjectValue(st, value) then Left(typeError(interp, callSite))
    else
      withGcPin(interp) { pin =>
        val method = callAO("GetMethod", List(value, symbolAddr("iterator")))
        if isAbrupt(st, method) then Left(method)
        else
          st(method, Str("Value")) match
            case Undef => Left(typeError(interp, callSite))
            case m =>
              pin(List(m))
              val iter = callAO("GetIteratorFromMethod", List(value, m))
              if isAbrupt(st, iter) then Left(iter)
              else
                val record = st(iter, Str("Value"))
                pin(List(m, record))
                val list = callAO("IteratorToList", List(record))
                if isAbrupt(st, list) then Left(list)
                else
                  st(list, Str("Value")) match
                    case addr: Addr =>
                      st(addr) match
                        case l: ListObj => Right(l.values.toList)
                        case other      => throw NoList(other)
                    case other => throw NoAddr(other)
      }

  /** [[iterateSequence]] into a genuine internal `List`, the element type
    * (`any`) passing through as is.
    */
  private def toSequence(
    interp: Interpreter,
    callSite: Call,
    value: Value,
  ): Either[Value, Value] =
    iterateSequence(interp, callSite, value).map(interp.st.allocList(_))

  /** same iteration as [[toSequence]], but for a `sequence<T>` whose element
    * type `T` is itself a WebIDL enum (so far only `TagType. parameters`'s
    * `sequence<ValueType>`) -- each raw element goes through the same `?
    * ToString` + membership check as a scalar enum member (see
    * `Member.enumValues`'s doc), short-circuiting on the first abrupt result or
    * invalid value.
    */
  private def readEnumSequence(
    interp: Interpreter,
    callSite: Call,
    value: Value,
    allowed: Set[String],
  ): Either[Value, Value] =
    val st = interp.st
    iterateSequence(interp, callSite, value).flatMap { raws =>
      val converted = scala.collection.mutable.ListBuffer.empty[Value]
      var abrupt: Option[Value] = None
      val it = raws.iterator
      // same reentrant-accumulator hazard as `readDictionary`'s `pairs` (see
      // `withGcPin`); `raws` come from a fresh list nothing else holds, so
      // they're pinned too.
      withGcPin(interp) { pin =>
        pin(raws)
        while abrupt.isEmpty && it.hasNext do
          val strResult = toStringValue(interp, callSite, it.next())
          if isAbrupt(st, strResult) then abrupt = Some(strResult)
          else
            st(strResult, Str("Value")) match
              case s @ Str(v) if allowed(v) =>
                converted += s
                pin(raws ++ converted)
              case _ => abrupt = Some(typeError(interp, callSite))
      }
      abrupt match
        case Some(a) => Left(a)
        case None    => Right(st.allocList(converted.toList))
    }

  // ── converted to a JavaScript value ────────────────────────────────────────

  /** mirrors the `.ir` version's `if (? argument: Map) { ... } return argument`
    * — only a `MapObj` (this project's own internal dictionary representation)
    * gets built into a real ordinary object, and a `ListObj` (a `sequence<T>`
    * return value, e.g. `Module.exports`'s `sequence<ModuleExportDescriptor>`)
    * into a real `Array` -- everything else, including an already-real
    * ECMAScript value, passes through unchanged. Each element/entry-value is
    * itself recursively converted first (a `sequence<Dictionary>`'s elements
    * are still raw `MapObj`s at this point), then `CreateArrayFromList` (a real
    * mechanized closure, `? CreateArrayFromList(elements)` per its own spec,
    * never abrupt) builds the actual `Array` -- reentrant like `Get`/
    * `ToString`/etc. elsewhere in this object (`Interpreter.invokeCallable`),
    * since it needs `ArrayCreate`/`CreateDataPropertyOrThrow` machinery this
    * object has no reason to reimplement natively.
    *
    * A `RecordObj("PromiseCapabilityRecord", ...)` is a third, separate case
    * from either of those: `webidl/index.bs`'s exported term "a new promise"
    * (`Return [=?=] [$NewPromiseCapability$] (|constructor|).`) returns the raw
    * mainline-compiled Completion-record-shaped `PromiseCapabilityRecord`
    * itself, not its `.[[Promise]]` field, so every namespace operation
    * following `Let |promise| be [=a new promise=]. ... Return |promise|.`
    * (`WebAssembly.compile`/`instantiate`) would otherwise return that raw
    * record — not a `MapObj` (WJI's own dictionary representation, the only
    * thing the case above matches), so it would fall all the way through to
    * identity passthrough unconverted, same bug either way. Unwrapped here
    * instead of at each call site, mirroring what the one hand-written
    * `manuals/funcs/INTRINSICS.WebAssembly.instantiate.ir` glue (now deleted)
    * used to do with a bare `%2 = %1.Promise` — but done once, generically, so
    * `WebAssembly.compile` (which never had that glue, since it never even
    * existed as a real property before `AlgorithmKind.NamespaceMethod`) gets it
    * for free too. See `docs/hardcodes.md` #7.
    */
  def toJsValue(interp: Interpreter, callSite: Call, argument: Value): Value =
    val st = interp.st
    argument match
      case addr: Addr =>
        st(addr) match
          case RecordObj("PromiseCapabilityRecord", fields) =>
            fields("Promise")
          case MapObj(entries) =>
            given CFG = st.cfg
            val objAddr = newOrdinaryObject(st)
            val objMap = st(objAddr, Str("__MAP__"))
            for (key, rawValue) <- entries do
              val value = toJsValue(interp, callSite, rawValue)
              val pdAddr = st.allocRecord(
                "PropertyDescriptor",
                List(
                  "Value" -> value,
                  "Writable" -> Bool(true),
                  "Enumerable" -> Bool(true),
                  "Configurable" -> Bool(true),
                ),
              )
              st.update(objMap, key, pdAddr)
            objAddr
          case ListObj(values) =>
            val converted = values.toList.map(toJsValue(interp, callSite, _))
            val listAddr = st.allocList(converted)
            interp.invokeCallable(
              Clo(st.cfg.getFunc("CreateArrayFromList"), Map.empty),
              List(listAddr),
              callSite,
            )
          case _ => argument
      case _ => argument

  /** the internal-method closure every one of `__NEW_OBJ__.ir`'s fields names,
    * resolved the same way `Interpreter`'s own `EClo` evaluation does
    * (`cfg.getFunc(name)`, no captured variables).
    */
  private def ordinaryMethod(cfg: CFG, name: String): Value =
    Clo(cfg.getFunc(s"Record[OrdinaryObject].$name"), Map.empty)

  /** mirrors `__NEW_OBJ__.ir` + `converted_to_a_javascript_value.ir`'s own
    * `obj.Prototype = intrinsics["%Object.prototype%"]` / `obj.Extensible =
    * true` follow-up — a fresh ordinary object with no own properties yet.
    */
  private def newOrdinaryObject(st: State)(using CFG): Addr =
    val cfg = st.cfg
    val objAddr = st.allocRecord(
      "Object",
      List(
        "GetPrototypeOf" -> ordinaryMethod(cfg, "GetPrototypeOf"),
        "SetPrototypeOf" -> ordinaryMethod(cfg, "SetPrototypeOf"),
        "IsExtensible" -> ordinaryMethod(cfg, "IsExtensible"),
        "PreventExtensions" -> ordinaryMethod(cfg, "PreventExtensions"),
        "GetOwnProperty" -> ordinaryMethod(cfg, "GetOwnProperty"),
        "DefineOwnProperty" -> ordinaryMethod(cfg, "DefineOwnProperty"),
        "HasProperty" -> ordinaryMethod(cfg, "HasProperty"),
        "Get" -> ordinaryMethod(cfg, "Get"),
        "Set" -> ordinaryMethod(cfg, "Set"),
        "Delete" -> ordinaryMethod(cfg, "Delete"),
        "OwnPropertyKeys" -> ordinaryMethod(cfg, "OwnPropertyKeys"),
        "PrivateElements" -> st.allocList(Nil),
        "__MAP__" -> st.allocMap(Nil),
      ),
    )
    st.update(objAddr, Str("Prototype"), objectPrototype(st))
    st.update(objAddr, Str("Extensible"), Bool(true))
    objAddr

  /** mirrors `@EXECUTION_STACK[0].Realm.Intrinsics["%Object.prototype%"]`. */
  private def objectPrototype(st: State): Value =
    val stack = st(GLOBAL_EXECUTION_STACK)
    val ctx = st(stack, Math(0))
    val realm = st(ctx, Str("Realm"))
    val intrinsics = st(realm, Str("Intrinsics"))
    st(intrinsics, Str("%Object.prototype%"))
