package esmeta.editor.analyzer

case class AbsTransfer[AbsState, AbsRet](sem: AbsSemantics[AbsState, AbsRet]) {

  def apply(cp: ControlPoint): Unit = ()
}
