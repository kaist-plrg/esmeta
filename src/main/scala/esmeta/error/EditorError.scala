package esmeta.error

sealed abstract class EditorError(msg: String)
  extends ESMetaError(s"[Editor Error] $msg")

case object AnalysisTimeoutError
  extends EditorError(
    "timeout during the abstract interpretation.",
  )

case class AnalysisImpreciseError(msg: String = "") extends EditorError(msg)
