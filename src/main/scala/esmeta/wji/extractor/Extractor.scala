package esmeta.wji.extractor

import esmeta.util.SystemUtils.*
import esmeta.wji.lang.{
  AlgorithmExtractor,
  InterfaceExtractor,
  SpecAnchors,
  SpecFile,
  SpecPatch,
}
import esmeta.wji.spec.Spec

/** Unified entry point producing a [[Spec]] from the WebAssembly JS-API spec
  * sources — mirrors mainline's `esmeta.extractor.Extractor` (which takes a
  * parsed HTML `Document`); this one works directly on raw Bikeshed text
  * instead, like every other WJI extractor, since WJI has no separate
  * HTML-parsing step.
  */
object Extractor:
  def apply(): Spec =
    // patched once and reused for algorithms/interfaces/anchors alike, rather
    // than re-reading and re-patching the same file 3 times over
    val jsApiSource = SpecPatch(readFile(SpecFile.jsApiIndex.toString))
    val jsApiAlgorithms = AlgorithmExtractor.extract(jsApiSource)
    val webidlAlgorithms = AlgorithmExtractor
      .extractFromFile(SpecFile.webidlIndex)
      .filter(a => a.name.exists(SpecFile.webidlFilter.contains))
    val interfaces = InterfaceExtractor.extract(jsApiSource)
    val anchors = SpecAnchors.extract(jsApiSource)
    Spec(jsApiAlgorithms ++ webidlAlgorithms, interfaces, anchors)
