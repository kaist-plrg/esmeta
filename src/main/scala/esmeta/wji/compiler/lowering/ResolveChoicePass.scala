package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Expr}
import esmeta.wji.lang.walker.{UnitWalker, Walker}

/** Resolves every `Expr.Choice` ("either A or B", a choice the spec leaves to
  * the implementation) to its first option that is fully mechanized, i.e.
  * contains nothing still unparsed (`Unknown`/`Described`/...): any option is a
  * valid result, so one that can actually run is the one to take. E.g. "either
  * a {{DOMString}} representation of the current call stack or undefined"
  * becomes `undefined` (`docs/hardcodes.md` #26). With no such option, the
  * first one is kept, so it surfaces as not yet mechanized.
  *
  * Category: Spec-dependent — WJI.
  */
object ResolveChoicePass extends LoweringPass:

  override def requires: Set[LoweringPass] = Set(ResolveLinksPass)

  private object resolver extends Walker:
    override def walk(expr: Expr): Expr = expr match
      case Expr.Choice(options) =>
        walk(options.find(isMechanized).getOrElse(options.head))
      case other => super.walk(other)

  private def isMechanized(expr: Expr): Boolean =
    var ok = true
    new UnitWalker {
      override def walk(e: Expr): Unit = e match
        case _: Expr.Unknown | _: Expr.UnknownNew | _: Expr.Described |
            _: Expr.SuchThat =>
          ok = false
        case other => super.walk(other)
    }.walk(expr)
    ok

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = a.body.map(resolver.walk)))
