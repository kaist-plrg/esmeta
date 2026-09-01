package esmeta.wji.interpreter

import esmeta.cfg.{CFG, Call}
import esmeta.error.{NoMathValue, UnknownConversion}
import esmeta.interpreter.{%%, Interpreter}
import esmeta.ir.GLOBAL_EXECUTION_STACK
import esmeta.parser.ESValueParser
import esmeta.state.*
import esmeta.ty.AbruptT

/** Native Scala dispatch target for [[esmeta.ir.Inst.ICallConvert]] — WebIDL's
  * "converted to an IDL value" / "converted to a JavaScript value" abstract
  * operations, ported 1:1 from the former
  * `manuals/funcs/converted_to_an_idl_value.ir` /
  * `converted_to_a_javascript_value.ir` stubs. Same scope as before the port
  * (see `docs/hardcodes.md` #1/#2), extended with `TagType`: only `"unsigned
  * long"` and four WebAssembly dictionaries (`MemoryDescriptor`,
  * `TableDescriptor`, `GlobalDescriptor`, `TagType`) genuinely convert; every
  * other IDL type is still identity passthrough. Dictionary member reads go
  * through a real `Get` (prototype chain, getters, and any exception a getter
  * throws all work), and a required member found absent throws a real
  * `TypeError` right there — see `readDictionary` and
  * `Interpreter.invokeCallable`.
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
    * (`AddInterfaceMemberBuiltinBehaviourPass.convertedIdlValueBinding`) tells
    * the two apart with `Cond.IsType(_, "AbruptCompletion")`, which is false
    * for every ordinary converted value (a `MapObj` address, `Number`, `Str`,
    * ... -- none of them a `CompletionRecord`).
    * `converted_to_a_javascript_value` can't throw (no `Get` involved) and was
    * never affected.
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
        toJsValue(st, argument)
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

  private def isAbrupt(st: State, v: Value): Boolean =
    AbruptT.contains(v, st.heap)

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
  )

  private val addressTypeValues = Set("i32", "i64")
  private val tableKindValues = Set("externref", "anyfunc")
  private val valueTypeValues =
    Set("i32", "i64", "f32", "f64", "v128", "externref", "anyfunc")

  private val memoryDescriptorMembers = List(
    Member("initial", required = true),
    Member("maximum"),
    Member("address", enumValues = Some(addressTypeValues)),
  )
  private val tableDescriptorMembers = List(
    Member("element", required = true, enumValues = Some(tableKindValues)),
    Member("initial", required = true),
    Member("maximum"),
    Member("address", enumValues = Some(addressTypeValues)),
  )
  // `boolean mutable = false;` -- the only member across these four
  // dictionaries with an actual IDL default (the js-api spec's other
  // non-required members have none, so an absent one is correctly left out
  // of the result entirely -- see `readDictionary`).
  private val globalDescriptorMembers = List(
    Member("value", required = true, enumValues = Some(valueTypeValues)),
    Member("mutable", default = Some(Bool(false))),
  )
  // `required sequence<ValueType> parameters;` -- element-wise ValueType
  // conversion is skipped (still identity passthrough, same as every other
  // enum-shaped IDL type here); only the outer JS-array-to-List step is done.
  private val tagTypeMembers =
    List(Member("parameters", required = true, isSequence = true))
  // `boolean traceStack = false;` -- `Exception`'s constructor's third
  // parameter (`optional ExceptionOptions options = {}`), no required members.
  private val exceptionOptionsMembers =
    List(Member("traceStack", default = Some(Bool(false))))

  /** `ty` names the declared IDL type — almost always a literal `Str` (from
    * `AddInterfaceMemberBuiltinBehaviourPass.unpackArgumentsList`'s
    * `WjiParam.idlType`-driven call), but a direct spec-text "converted to an
    * IDL value of type X" reference (e.g. inside a hoisted `react` closure)
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
      Right(toUnsignedLong(argument))
    case Str("MemoryDescriptor") | Enum("MemoryDescriptor") =>
      readDictionary(interp, callSite, argument, memoryDescriptorMembers)
    case Str("TableDescriptor") | Enum("TableDescriptor") =>
      readDictionary(interp, callSite, argument, tableDescriptorMembers)
    case Str("GlobalDescriptor") | Enum("GlobalDescriptor") =>
      readDictionary(interp, callSite, argument, globalDescriptorMembers)
    case Str("TagType") | Enum("TagType") =>
      readDictionary(interp, callSite, argument, tagTypeMembers)
    case Str("ExceptionOptions") | Enum("ExceptionOptions") =>
      readDictionary(interp, callSite, argument, exceptionOptionsMembers)
    // a bare `sequence<T>` parameter (as opposed to one nested inside a
    // dictionary, see `Member.isSequence`) -- so far only
    // `Exception`'s constructor's `sequence<any> payload`. Matched by prefix
    // rather than the exact element type, same "identity passthrough for the
    // element type" simplification as everywhere else in this object.
    case Str(t) if t.startsWith("sequence<") =>
      Right(toSequence(interp.st, argument))
    case Enum(t) if t.startsWith("sequence<") =>
      Right(toSequence(interp.st, argument))
    case _ => Right(argument)

  private val TWO_32: BigDecimal = BigDecimal(4294967296L)

  private def toMathValue(v: Value): Math = v match
    case n: Math   => n
    case Number(d) => Math(d)
    case Str(s)    => Math(ESValueParser.str2number(s).double)
    case v         => throw NoMathValue(v)

  private def toUnsignedLong(argument: Value): Value =
    val m = toMathValue(argument)
    val n =
      if m < Math.zero then Math.zero - Interpreter.floor(Interpreter.abs(m))
      else Interpreter.floor(m)
    Math(n.decimal %% TWO_32)

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
    // WebIDL's dictionary conversion algorithm reads members in lexicographic
    // (alphabetical) order, not declaration order -- `memoryDescriptorMembers`
    // etc. above list them in the more readable declaration order instead, so
    // sort here rather than asking every list to already be alphabetized.
    // Observable via evaluation-order side effects: `spectec/test/js-api/
    // memory/constructor.any.js`'s "Order of evaluation for descriptor" reads
    // "address" (declared last) before "initial"/"maximum" for exactly this
    // reason.
    val it = members.sortBy(_.name).iterator
    while abrupt.isEmpty && it.hasNext do
      val member = it.next()
      val key = Str(member.name)
      def absent(): Unit = member.default match
        case Some(d) => pairs += key -> d
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
              case raw if member.enumValues.isDefined =>
                val strResult = toStringValue(interp, callSite, raw)
                if isAbrupt(st, strResult) then abrupt = Some(strResult)
                else
                  st(strResult, Str("Value")) match
                    case s @ Str(v) if member.enumValues.get(v) =>
                      pairs += key -> s
                    case _ => abrupt = Some(typeError(interp, callSite))
              case raw =>
                val value =
                  if member.isSequence then toSequence(st, raw) else raw
                pairs += key -> value
    abrupt match
      case Some(a) => Left(a)
      case None    => Right(st.allocMap(pairs.toList))

  /** converts a JS array-like `value` (own `"length"` + own indexed properties,
    * e.g. a real `Array` literal) into a genuine internal `List` — mirrors
    * `CreateListFromArrayLike`'s own simple read loop (length, then each index
    * in turn), rather than the full WebIDL "sequence" conversion (which
    * iterates via `Symbol.iterator`): every actual call site so far passes a
    * literal array, so the two agree, and this avoids driving the iterator
    * protocol from native code just for that.
    */
  private def toSequence(st: State, value: Value): Value =
    val mapField = st(value, Str("__MAP__"))
    val length = toMathValue(st(st(mapField, Str("length")), Str("Value")))
    val elements = (0 until length.decimal.toInt).toList.map { i =>
      st(st(mapField, Str(i.toString)), Str("Value"))
    }
    st.allocList(elements)

  // ── converted to a JavaScript value ────────────────────────────────────────

  /** mirrors the `.ir` version's `if (? argument: Map) { ... } return argument`
    * — only a `MapObj` (this project's own internal dictionary representation)
    * gets built into a real ordinary object; everything else, including an
    * already-real ECMAScript value, passes through unchanged.
    */
  def toJsValue(st: State, argument: Value): Value = argument match
    case addr: Addr =>
      st(addr) match
        case MapObj(entries) =>
          given CFG = st.cfg
          val objAddr = newOrdinaryObject(st)
          val objMap = st(objAddr, Str("__MAP__"))
          for (key, rawValue) <- entries do
            val value = toJsValue(st, rawValue)
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
