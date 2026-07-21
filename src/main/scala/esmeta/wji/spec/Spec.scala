package esmeta.wji.spec

import esmeta.util.ManualInfo
import esmeta.wji.lang.{Algorithm, Anchor, Interface}

/** The WebAssembly JS-API specification, mirroring mainline's
  * `esmeta.spec.Spec` — a plain container for everything
  * [[esmeta.wji.extractor.Extractor]] pulls out of the spec source.
  */
case class Spec(
  algorithms: List[Algorithm] = Nil,
  interfaces: List[Interface] = Nil,
  anchors: List[Anchor] = Nil,
):

  /** mapping from interface names to interfaces */
  lazy val interfaceMap: Map[String, Interface] =
    interfaces.map(i => i.name -> i).toMap

  /** Registers every extracted interface as a dynamic subtype of `"Object"`
    * (see `esmeta.ty.TyModel.registerDynamicSubtype`) — fixes mainline's
    * `Type(v)`/`HasPrimitiveBase` classification for WJI-constructed interface
    * objects (e.g. a WebAssembly.Instance), which `ManualInfo`'s static,
    * file-based type hierarchy has no way to know about. Must be called before
    * the interpreter runs (safe to call any time before that — see `TyModel`'s
    * doc for why this can't just be a static `manuals/types` entry).
    */
  def registerInterfaceTypes(): Unit =
    interfaces.foreach(i =>
      ManualInfo.tyModel.registerDynamicSubtype(i.name, "Object"),
    )
