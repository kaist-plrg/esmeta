package esmeta.fuzzer

import esmeta.*
import esmeta.util.UId

/** conformance test */
class Test(
  val id: Int,
  val script: String,
  val assertions: List[Assertion],
) extends FuzzerElem
  with UId
