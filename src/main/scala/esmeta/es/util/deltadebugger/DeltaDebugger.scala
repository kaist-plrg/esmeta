package esmeta.es.util.deltadebugger

import esmeta.es.Ast
import esmeta.js.Target

trait DeltaDebugger[T](initial: T):
  def current: T
  def tryReduce(): Option[T]

type Script = String

class ESReducer(val initial: Script, val target: Target)
  extends DeltaDebugger[Script](initial):
  private var curr: Script = initial
  private var finished = false

  private def uncheckedTryReduce(): Option[Script] =
    throw NotImplementedError()

  def current: Script = curr

  def tryReduce(): Option[Script] =
    if finished then None
    else
      uncheckedTryReduce() match
        case None =>
          finished = true
          None
        case Some(reduced) =>
          curr = reduced
          Some(reduced)
