package esmeta.editor.analyzer

// type-based allocsite
sealed trait AllocSite
sealed trait SpecAllocSite extends AllocSite
case object ListAllocSite extends SpecAllocSite
case class RecordAllocSite(ty: String) extends SpecAllocSite
case object SymbolAllocSite extends AllocSite
case class ObjAllocSite(ty: String) extends AllocSite
