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

  private val memoryDescriptorMembers =
    List("initial" -> None, "maximum" -> None, "address" -> None)
  private val tableDescriptorMembers = List(
    "element" -> None,
    "initial" -> None,
    "maximum" -> None,
    "address" -> None,
  )
  // `boolean mutable = false;` -- the only member across these four
  // dictionaries with an actual IDL default (the js-api spec's other
  // non-required members have none, so an absent one is correctly left out
  // of the result entirely -- see `readDictionary`).
  private val globalDescriptorMembers =
    List("value" -> None, "mutable" -> Some(Bool(false)))
  private val tagTypeMembers = List("parameters" -> None)

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
    case _ => argument

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
    * default values (`member -> Some(default)`): when a member is absent, a
    * `None` default (matches every js-api non-required member without an
    * explicit IDL default, e.g. `MemoryDescriptor.maximum`) leaves it out of
    * the result entirely, same as before; a `Some(default)` (so far only
    * `GlobalDescriptor.mutable = false`) fills it in instead. A *required*
    * member (e.g. `TableDescriptor.element`) still just goes missing when
    * absent rather than throwing a real `TypeError` -- WebIDL dictionary
    * conversion is supposed to reject that case, but nothing here can raise a
    * catchable ECMAScript exception yet (see `personal/TODO.md` #14).
    */
  private def readDictionary(
    st: State,
    argument: Value,
    members: List[(String, Option[Value])],
  ): Value =
    val mapField = st(argument, Str("__MAP__"))
    val dictAddr = st.allocMap(Nil)
    for (member, default) <- members do
      val key = Str(member)
      if st.exists(mapField, key) then
        val pd = st(mapField, key)
        st.update(dictAddr, key, st(pd, Str("Value")))
      else default.foreach(st.update(dictAddr, key, _))
    dictAddr

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
