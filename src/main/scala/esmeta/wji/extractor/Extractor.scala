package esmeta.wji.extractor

import esmeta.util.SystemUtils.*
import esmeta.wji.lang.{AlgorithmKind, DefinitionKind}
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
    val algorithms = (jsApiAlgorithms ++ webidlAlgorithms).map { a =>
      a.kind match
        case AlgorithmKind.Method(forName) if !interfaceNames(forName) =>
          a.copy(kind = AlgorithmKind.Plain)
        case _ => a
    }
    Spec(algorithms, definitions, anchors)
