package esmeta.js

import esmeta.util.BaseUtils.*

case class Target(
  val name: String,
  val isTrans: Boolean,
) {
  lazy val cmd = optional {
    if (!isTrans)
      JSEngine.defaultCmd(name)
    else
      JSTrans.defaultCmd(name)
  }.getOrElse(name)

  override def toString = name
}
