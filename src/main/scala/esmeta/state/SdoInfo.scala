package esmeta.state

import esmeta.cfg.*
import esmeta.es.*
import esmeta.ir.Name
import esmeta.spec.*

/** syntax directed operation information */
case class SdoInfo(
  ast: Syntactic,
  func: Func,
  methodName: String,
) extends StateElem
