package esmeta.wji.lang

/** What kind of thing an [[Algorithm]] implements, per WebIDL's Members
  * convention (see `webidl/index.bs`'s "Members" section: constructor/getter/
  * setter/method steps all have an implicit `**this**`, distinct from a
  * free-standing operation). Not yet used to change how [[Algorithm]]s compile
  * (`esmeta.wji.compiler.Compiler` still treats every kind as a plain function)
  * beyond disambiguating names that would otherwise collide (a getter and
  * setter for the same attribute both derive the same `name` from their shared
  * `<dfn attribute for=...>` — see [[AlgorithmExtractor]]); kept as real data
  * now because building the WebIDL "internally create a new object implementing
  * the interface" preamble (`this`-binding automation — see
  * `personal/constructor.md`) will need exactly this information later.
  */
enum AlgorithmKind:
  /** an operation not tied to any interface, e.g. "To <dfn>compile a
    * WebAssembly module</dfn> from ..., perform the following steps:"
    */
  case Plain

  /** "The <dfn method for="X">name(...)</dfn> method, when invoked, ..." */
  case Method(interface: String)

  /** "The getter of the <dfn attribute for="X">name</dfn> attribute of {{X}},
    * ..."
    */
  case Getter(interface: String)

  /** "The setter of the name attribute of {{X}}, ..." — never has its own `<dfn
    * attribute for=...>` (Bikeshed dfns must be unique by `(type, for, linking
    * text)`, and the getter's dfn already claims it; WebIDL's own spec text
    * follows the same one-dfn-per-attribute convention), so this is recognized
    * from the "The setter of ... attribute of {{X}}" prose pattern directly.
    * See [[AlgorithmExtractor]].
    */
  case Setter(interface: String)

  /** "The <dfn constructor for="X">name(...)</dfn> constructor, ..." */
  case Constructor(interface: String)

/** An algorithm extracted from a `<div algorithm>` block.
  *
  * @param id
  *   the value of the `algorithm="..."` attribute, if the `<div>` tag has one
  *   (e.g. `"read-the-imports"`)
  * @param name
  *   the algorithm/operation name, taken from the first `<dfn>` element found
  *   in `head`, if any (e.g. `"compile a WebAssembly module"`, or `"validate"`
  *   for `<dfn>validate(|bytes|, |options|)</dfn>` - any trailing `(|arg1|,
  * |arg2|)`/`()` parameter list is stripped, since it is captured in `params`
  * instead)
  * @param params
  *   every distinct `|variable|` reference in `head`, in order of first
  *   appearance
  * @param head
  *   the descriptive text preceding the step list, e.g. "To <dfn>compile a
  *   WebAssembly module</dfn> from source bytes
  * |bytes|, perform the following steps:"
  * @param body
  *   the algorithm's steps, parsed into [[Instr]]s, in source order
  * @param kind
  *   what this algorithm implements — see [[AlgorithmKind]]. Defaults to
  *   [[AlgorithmKind.Plain]] for algorithms synthesized outside
  *   [[AlgorithmExtractor]] (e.g. `ExpandQueueATaskPass`'s job closures), which
  *   are never a WebIDL interface member.
  */
case class Algorithm(
  id: Option[String],
  name: Option[String],
  params: List[String],
  head: String,
  body: List[Instr],
  kind: AlgorithmKind = AlgorithmKind.Plain,
)
