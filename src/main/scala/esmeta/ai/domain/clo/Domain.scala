package esmeta.ai.domain.clo

import esmeta.ai.*
import esmeta.ai.domain.*
import esmeta.state.Clo

/** abstract closure domain */
trait Domain extends domain.Domain[AClo] {

  /** abstraction functions for an original closure */
  def alpha(clo: Clo): Elem = alpha(AClo.from(clo))
}
