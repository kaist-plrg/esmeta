package esmeta.js

import esmeta.util.BaseUtils.*

case class Target(
  val name: String,
  val version: String,
  val isTrans: Boolean,
) {
  lazy val cmd = optional {
    if (!isTrans)
      JSEngine.defaultCmd(name)(version)
    else
      JSTrans.defaultCmd(name)(version)
  }.getOrElse(name)

  override def toString = s"$name@$version"
}

object Target {
  def from(arg: String, isTrans: Boolean): Target =
    val Array(name, version) = arg.split("@")
    Target(name, version, isTrans)
}
