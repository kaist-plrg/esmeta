package esmeta.wji.lang

/** A WebIDL `interface` block extracted from a `<pre class="idl">` in
  * js-api/index.bs (e.g. `Module`, `Instance`, `Memory`, `Table`, `Global`,
  * `Tag`, `Exception`) — not a `dictionary`/`enum` block, which share the same
  * `<pre class="idl">` container but are a separate WebIDL construct (see
  * [[InterfaceExtractor]]).
  *
  * @param name
  *   the interface name, e.g. `"Instance"`
  * @param members
  *   each member declaration's raw text (e.g. `"constructor(Module module,
  *   optional object importObject)"`, `"readonly attribute object exports"`),
  *   unparsed — like [[Algorithm.head]], since nothing today needs more than
  *   the interface name (see [[esmeta.wji.spec.Spec.registerInterfaceTypes]])
  */
case class Interface(name: String, members: List[String])
