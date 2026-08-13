package esmeta.wji.extractor

import esmeta.util.SystemUtils.*
import esmeta.wji.lang.{
  Algorithm,
  AlgorithmKind,
  Definition,
  DefinitionKind,
  MemberKind,
  Operation,
  Param,
}
import esmeta.wji.spec.{Spec, SpecFile, SpecPatch}

/** Unified entry point producing a [[Spec]] from the WebAssembly JS-API spec
  * sources — mirrors mainline's `esmeta.extractor.Extractor` (which takes a
  * parsed HTML `Document`); this one works directly on raw Bikeshed text
  * instead, like every other WJI extractor, since WJI has no separate
  * HTML-parsing step.
  */
object Extractor:
  def apply(): Spec =
    // patched once and reused for algorithms/definitions/anchors alike,
    // rather than re-reading and re-patching the same file 3 times over
    val jsApiSource = SpecPatch(readFile(SpecFile.jsApiIndex.toString))
    val jsApiAlgorithms = AlgorithmExtractor.extract(jsApiSource)
    val webidlAlgorithms = AlgorithmExtractor
      .extractFromFile(SpecFile.webidlIndex)
      .filter(a => a.name.exists(SpecFile.webidlFilter.contains))
    val definitions = DefinitionExtractor.extract(jsApiSource)
    val anchors = AnchorExtractor.extract(jsApiSource)
    // `AlgorithmExtractor` produces `AlgorithmKind.Method(for)` for any "The
    // <dfn method for="X">..." dfn uniformly, without knowing whether `X` is a
    // real WebIDL interface or a *namespace* (`WebAssembly` itself) — that
    // distinction only exists once `definitions` (computed above) is in
    // hand, via each one's `DefinitionKind`. It matters structurally, not
    // just for naming: WebIDL's "create a namespace object" installs a
    // namespace's own operations directly on the namespace object itself,
    // while "create an interface object"/"create an interface prototype
    // object" installs an interface's members on a *separate* interface
    // prototype object — two different algorithms building and populating
    // two different objects, not one mechanism with two names.
    // `AddInterfaceMemberBuiltinBehaviourPass` only mechanizes the
    // interface-prototype-object shape, so a namespace method is downgraded
    // to `Plain` here rather than reaching it — see that pass's own doc and
    // `docs/hardcodes.md` #7 for the concrete bug this would otherwise hit
    // (`WebAssembly.instantiate`'s underlying `a new promise` term returning
    // an un-unwrapped `PromiseCapabilityRecord`).
    val interfaceNames = definitions
      .filter(_.kind == DefinitionKind.Interface)
      .map(_.name)
      .toSet
    val algorithms = (jsApiAlgorithms ++ webidlAlgorithms)
      .map { a =>
        a.kind match
          case AlgorithmKind.Method(forName) if !interfaceNames(forName) =>
            a.copy(kind = AlgorithmKind.Plain)
          case _ => a
      }
      .map(enrichParamTypes(_, definitions))
    Spec(algorithms, definitions, anchors)

  /** Finds `algo`'s matching WebIDL operation (its interface's `Definition`,
    * looked up by `AlgorithmKind.Method`/`Constructor`'s own `interface` name,
    * then the `Operation` member matching `algo.name`/its `Constructor` kind)
    * and, if found and its positional param count agrees, stamps each
    * `WjiParam.idlType`/`WjiParam.optional` with that operation's declared
    * WebIDL type text/`optional` keyword — see [[esmeta.wji.lang.WjiParam]]'s
    * own doc for what consumes these. `optional` here is WebIDL's own
    * declaration (e.g. `optional any value`), a different source from
    * `AlgorithmExtractor.extractParams`'s "using optional X |Y|" prose
    * detection (which a `Method`/`Constructor` dfn's own head, e.g.
    * `grow(|delta|, |value|)`, never spells out — only the separate `<pre
    * class=idl>` block does) — `p.optional` is OR'd with the WebIDL flag rather
    * than overwritten, so either source marking a param optional is enough. A
    * `Getter`/`Setter`/`Plain` algorithm is left untouched: getters take no
    * arguments, and a setter's implicit "the given value" isn't a positional
    * `WjiParam` at all (see
    * `esmeta.wji.compiler.lowering.AddInterfaceMemberBuiltinBehaviourPass.givenValueBinding`),
    * so neither has anything here to stamp.
    */
  private def enrichParamTypes(
    algo: Algorithm,
    definitions: List[Definition],
  ): Algorithm =
    def operationParams(
      iface: String,
      matches: Operation => Boolean,
    ): Option[List[Param]] =
      definitions
        .find(d => d.kind == DefinitionKind.Interface && d.name == iface)
        .flatMap(_.members.collectFirst {
          case op: Operation if matches(op) => op.params
        })
    val webidlParams: Option[List[Param]] = algo.kind match
      case AlgorithmKind.Method(iface) =>
        operationParams(
          iface,
          op =>
            op.kind != MemberKind.Constructor && op.id == algo.name.getOrElse(
              "",
            ),
        )
      case AlgorithmKind.Constructor(iface) =>
        operationParams(iface, _.kind == MemberKind.Constructor)
      case _ => None
    webidlParams match
      case Some(ps) if ps.length == algo.params.length =>
        algo.copy(params = algo.params.zip(ps).map {
          case (p, wp) =>
            p.copy(idlType = Some(wp.ty), optional = p.optional || wp.optional)
        })
      case _ => algo
