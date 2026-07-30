package esmeta.wji.lang

/** A WebIDL `namespace`/`interface` block extracted from a `<pre
  * class="idl">` in js-api/index.bs (e.g. `WebAssembly`, `Module`,
  * `Instance`, `Memory`, `Table`, `Global`, `Tag`, `Exception`) — not a
  * `dictionary`/`enum` block, which share the same `<pre class="idl">`
  * container but are a separate WebIDL construct (see
  * [[esmeta.wji.extractor.DefinitionExtractor]]).
  *
  * `members` holds each member declaration parsed into an [[Operation]]
  * (method/constructor) or [[Attribute]] — see
  * [[esmeta.wji.extractor.DefinitionExtractor]] for the parser.
  *
  * TODO: wire a `Getter`/`Method`/`Constructor` [[Algorithm]] to its real
  * WebIDL member declaration, or build the ordinary-object scaffolding
  * `Expr.New(iface)` doesn't yet construct (see `esmeta.wji.compiler.Compiler`'s
  * `Expr.New` case) — nothing consumes `members` for that yet.
  */

case class ExtendedAttribute(id: String, value: Option[String])

case class Param(
  ty: String,
  optional: Boolean,
  default: String,
  extAttribute: List[ExtendedAttribute],
)

enum MemberKind:
  case Static
  case Regular
  case Constructor

enum DefinitionKind:
  case Namespace
  case Interface

sealed trait Member
case class Operation(
  id: String,
  params: List[Param],
  ret: String,
  kind: MemberKind = MemberKind.Regular,
  extAttr: List[ExtendedAttribute] = Nil,
) extends Member
case class Attribute(
  id: String,
  ty: String,
  readonly: Boolean,
  kind: MemberKind = MemberKind.Regular,
  extAttr: List[ExtendedAttribute] = Nil,
) extends Member
case class Constant() extends Member // TODO

case class Definition (
  name: String,
  members: List[Member],
  kind: DefinitionKind,
  extAttr: List[ExtendedAttribute],
)
