package esmeta.es

import esmeta.*
import esmeta.util.UId
import esmeta.es.util.injector.ExitTag

/** conformance test */
case class ConformTest(
  id: Int,
  script: String,
  exitTag: ExitTag,
  isAsync: Boolean,
  assertions: Vector[Assertion],
) extends ESElem
  with UId
