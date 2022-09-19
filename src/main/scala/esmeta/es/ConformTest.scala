package esmeta.es

import esmeta.*
import esmeta.util.UId

/** conformance test */
case class ConformTest(
  id: Int,
  script: String,
  assertions: Vector[Assertion],
) extends ESElem
  with UId
