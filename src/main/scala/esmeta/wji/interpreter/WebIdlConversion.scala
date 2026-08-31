package esmeta.wji.interpreter

import esmeta.cfg.CFG
import esmeta.error.{NoMathValue, UnknownConversion}
import esmeta.interpreter.{%%, Interpreter}
import esmeta.ir.GLOBAL_EXECUTION_STACK
import esmeta.parser.ESValueParser
import esmeta.state.*

/** Native Scala dispatch target for [[esmeta.ir.Inst.ICallConvert]] — WebIDL's
  * "converted to an IDL value" / "converted to a JavaScript value" abstract
  * operations, ported 1:1 from the former
  * `manuals/funcs/converted_to_an_idl_value.ir` /
  * `converted_to_a_javascript_value.ir` stubs. Same scope as before the port
  * (see `docs/hardcodes.md` #1/#2), extended with `TagType`: only `"unsigned
  * long"` and four WebAssembly dictionaries (`MemoryDescriptor`,
  * `TableDescriptor`, `GlobalDescriptor`, `TagType`) genuinely convert; every
  * other IDL type is still identity passthrough, and dictionary reads are
  * own-property only (no prototype chain, no getters, no required-member
  * validation -- see `readDictionary`).
  */
object WebIdlConversion:

  def call(st: State, fname: String, args: List[Value]): Value =
    (fname, args) match
      case ("converted_to_an_idl_value", List(argument, ty)) =>
        toIdlValue(st, argument, ty)
      case ("converted_to_a_javascript_value", List(argument)) =>
        toJsValue(st, argument)
      case _ => throw UnknownConversion(fname)

  // ── converted to an IDL value ──────────────────────────────────────────────

  /** one dictionary member: its name, IDL default (`None` for a `required`
    * member or one with no declared default -- either way, absent just means
    * absent in the result, see `readDictionary`), and whether its own IDL type
    * is `sequence<T>` (so the raw value, if present, needs converting from a JS
    * array-like into a real internal `List` before it's usable by anything past
    * this point -- see `toSequence`).
    */
  private case class Member(
    name: String,
    default: Option[Value] = None,
    isSequence: Boolean = false,
  )

  private val memoryDescriptorMembers =
    List(Member("initial"), Member("maximum"), Member("address"))
  private val tableDescriptorMembers = List(
    Member("element"),
    Member("initial"),
    Member("maximum"),
    Member("address"),
  )
  // `boolean mutable = false;` -- the only member across these four
  // dictionaries with an actual IDL default (the js-api spec's other
  // non-required members have none, so an absent one is correctly left out
  // of the result entirely -- see `readDictionary`).
  private val globalDescriptorMembers =
    List(Member("value"), Member("mutable", default = Some(Bool(false))))
  // `required sequence<ValueType> parameters;` -- element-wise ValueType
  // conversion is skipped (still identity passthrough, same as every other
  // enum-shaped IDL type here); only the outer JS-array-to-List step is done.
  private val tagTypeMembers = List(Member("parameters", isSequence = true))
  // `boolean traceStack = false;` -- `Exception`'s constructor's third
  // parameter (`optional ExceptionOptions options = {}`).
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
  def toIdlValue(st: State, argument: Value, ty: Value): Value = ty match
    case Str("unsigned long") | Enum("unsigned long") =>
      toUnsignedLong(argument)
    case Str("MemoryDescriptor") | Enum("MemoryDescriptor") =>
      readDictionary(st, argument, memoryDescriptorMembers)
    case Str("TableDescriptor") | Enum("TableDescriptor") =>
      readDictionary(st, argument, tableDescriptorMembers)
    case Str("GlobalDescriptor") | Enum("GlobalDescriptor") =>
      readDictionary(st, argument, globalDescriptorMembers)
    case Str("TagType") | Enum("TagType") =>
      readDictionary(st, argument, tagTypeMembers)
    case Str("ExceptionOptions") | Enum("ExceptionOptions") =>
      readDictionary(st, argument, exceptionOptionsMembers)
    // a bare `sequence<T>` parameter (as opposed to one nested inside a
    // dictionary, see `Member.isSequence`) -- so far only
    // `Exception`'s constructor's `sequence<any> payload`. Matched by prefix
    // rather than the exact element type, same "identity passthrough for the
    // element type" simplification as everywhere else in this object.
    case Str(t) if t.startsWith("sequence<")  => toSequence(st, argument)
    case Enum(t) if t.startsWith("sequence<") => toSequence(st, argument)
    case _                                    => argument

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

  /** reads `members` straight off `argument`'s own `__MAP__` (own data
    * properties only) into a fresh internal `MapObj` — mirrors the `.ir`
    * version's `argument.__MAP__[key].Value`/`exists` reads exactly, plus IDL
    * default values (`Member.default`): when a member is absent, `None`
    * (matches every js-api non-required member without an explicit IDL default,
    * e.g. `MemoryDescriptor.maximum`) leaves it out of the result entirely,
    * same as before; `Some(default)` (so far only `GlobalDescriptor.mutable =
    * false`) fills it in instead. A *required* member (e.g.
    * `TableDescriptor.element`) still just goes missing when absent rather than
    * throwing a real `TypeError` -- WebIDL dictionary conversion is supposed to
    * reject that case, but nothing here can raise a catchable ECMAScript
    * exception yet.
    *
    * `argument` being `undefined`/`null` is a real, common case (an omitted or
    * explicitly-`undefined` dictionary argument -- WebIDL treats either the
    * same as an empty ordinary object `{}`), not an error: `mapField` is `None`
    * for those, so every member below reads as absent rather than `st(argument,
    * "__MAP__")` throwing `InvalidRefBase` on a non-object base.
    */
  private def readDictionary(
    st: State,
    argument: Value,
    members: List[Member],
  ): Value =
    val mapField = argument match
      case Undef | Null => None
      case _            => Some(st(argument, Str("__MAP__")))
    val dictAddr = st.allocMap(Nil)
    for member <- members do
      val key = Str(member.name)
      if mapField.exists(mf => st.exists(mf, key)) then
        val pd = st(mapField.get, key)
        val raw = st(pd, Str("Value"))
        val value = if member.isSequence then toSequence(st, raw) else raw
        st.update(dictAddr, key, value)
      else member.default.foreach(st.update(dictAddr, key, _))
    dictAddr

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
