package esmeta.wji.compiler.lowering

import esmeta.extractor.Extractor
import esmeta.spec.AbstractOperationHead
import esmeta.ty.{CompT, ValueTy}

/** ECMA-262 abstract operations whose declared return type can't be a
  * Completion Record (e.g. `OrdinaryObjectCreate ( ... ): an Object`), by
  * lowercased name. Read off ECMA-262's own algorithm headers, extracted once
  * per JVM, so a WJI call into one needs no completion check. One whose return
  * type isn't fully known (an `Unknown[...]` type) is left out.
  */
object EcmaAbstractOps:
  lazy val neverCompletion: Set[String] =
    Extractor().algorithms
      .map(_.head)
      .collect {
        case h: AbstractOperationHead if neverCompletion(h) =>
          h.name.toLowerCase
      }
      .toSet

  private def neverCompletion(h: AbstractOperationHead): Boolean =
    h.retTy.ty match
      case ty: ValueTy => (ty && CompT).isBottom
      case _           => false
