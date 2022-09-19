package esmeta.es

import esmeta.*
import esmeta.util.UId
import esmeta.es.util.injector.ExitTag

/** conformance test */
case class ConformTest(
  id: Int,
  script: String,
  exitTag: ExitTag,
  defs: Boolean,
  isAsync: Boolean,
  assertions: Vector[Assertion],
) extends ESElem
  with UId
