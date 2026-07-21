package esmeta.wji.spec

import esmeta.wji.lang.{Algorithm, Interface, SpecAnchors}

/** The WebAssembly JS-API specification, mirroring mainline's
  * `esmeta.spec.Spec` — a plain container for everything
  * [[esmeta.wji.extractor.Extractor]] pulls out of the spec source.
  */
case class Spec(
  algorithms: List[Algorithm] = Nil,
  interfaces: List[Interface] = Nil,
  anchors: List[SpecAnchors.Anchor] = Nil,
):

  /** mapping from interface names to interfaces */
  lazy val interfaceMap: Map[String, Interface] =
    interfaces.map(i => i.name -> i).toMap
