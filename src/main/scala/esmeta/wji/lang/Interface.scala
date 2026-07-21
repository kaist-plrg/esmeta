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
  *   unparsed — like [[Algorithm.head]]. Nothing today needs more than the
  *   interface name (see [[esmeta.wji.spec.Spec.registerInterfaceTypes]]), so
  *   these are kept only as a foundation for later work.
  *
  * TODO: actually parse these (attribute/method/constructor signatures) once
  * something needs to — e.g. wiring a `Getter`/`Method`/`Constructor`
  * [[Algorithm]] to its real WebIDL member declaration, or building the
  * ordinary-object scaffolding `Expr.New(iface)` doesn't yet construct (see
  * `esmeta.wji.compiler.Compiler`'s `Expr.New` case).
  */
case class Interface(name: String, members: List[String])
