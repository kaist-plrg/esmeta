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

  lazy val interfaceMap: Map[String, Interface] =
    interfaces.map(i => i.name -> i).toMap

  /** Registers every extracted interface as a dynamic subtype of `"Object"`
    * (see `esmeta.ty.TyModel.registerDynamicSubtype`) — fixes mainline's
    * `Type(v)`/`HasPrimitiveBase` classification for WJI-constructed interface
    * objects (e.g. a WebAssembly.Instance), which `ManualInfo`'s static,
    * file-based type hierarchy has no way to know about. Must be called before
    * the interpreter runs (safe to call any time before that — see `TyModel`'s
    * doc for why this can't just be a static `manuals/types` entry).
    *
    * `CompileError`/`LinkError`/`RuntimeError` are `<dfn exception>`s, not
    * WebIDL interfaces, so they never show up in `interfaces` above — but
    * `Compiler.ordinaryObjectFields` fabricates `Expr.New("CompileError")`
    * -style records for them the exact same way it does for real interfaces
    * (see its own `namesWithPrototypeIntrinsic` doc), so they need the same
    * dynamic-subtype registration or a bare `typeof`/`instanceof` on a caught
    * `CompileError` crashes (`Assert: Type(val) is Object` fails). Registered
    * under `"ErrorObject"` rather than bare `"Object"` — `manuals/types`
    * already declares `type ErrorObject extends OrdinaryObject` for real
    * ECMA-262 error instances (`TypeError` etc.), and these three really are
    * errors (`_NativeError_` family in `manuals/intrinsics`), not just
    * arbitrary objects; `isDynamicSubTy` recurses through `ErrorObject`'s own
    * static ancestry (`OrdinaryObject` → `Object`), so this is no weaker a
    * subtype-of-`"Object"` fact than registering directly, just a more precise
    * one.
    */
  def registerInterfaceTypes(): Unit =
    interfaces.foreach(i =>
      ManualInfo.tyModel.registerDynamicSubtype(i.name, "Object"),
    )
    List("CompileError", "LinkError", "RuntimeError").foreach(name =>
      ManualInfo.tyModel.registerDynamicSubtype(name, "ErrorObject"),
    )
