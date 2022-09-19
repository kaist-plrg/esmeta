package esmeta.fuzzer

import esmeta.fuzzer.util.*
import esmeta.util.BaseUtils.*

/** fuzzer elements */
trait FuzzerElem {
  override def toString: String =
    import Stringifier.elemRule
    stringify(this)
}
