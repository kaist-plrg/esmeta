package esmeta.error

import esmeta.ir.Func
import esmeta.analyzer.domain.*

sealed abstract class AnalysisError(msg: String)
  extends ESMetaError(s"[Analysis Error] $msg")

// not supported
case class NotSupportedOperation(obj: Any, method: String)
  extends AnalysisError(s"${obj.getClass.getName}.$method is not supported")

// imprecise
case class AnalysisImprecise(msg: String) extends AnalysisError(msg)

// arity mismatches
case class AnalysisRemainingParams(ps: List[Func.Param])
  extends AnalysisError(s"remaining parameters: ${ps.mkString(", ")}")
case class AnalysisRemainingArgs(as: List[AbsValue])
  extends AnalysisError(s"remaining arguments: ${as.mkString(", ")}")