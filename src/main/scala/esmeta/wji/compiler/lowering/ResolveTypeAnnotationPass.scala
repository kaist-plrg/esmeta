package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Expr}
import esmeta.wji.lang.walker.Walker

/** Resolves every `Expr.TypeAnnotated` node `ExprParser` produces uniformly for
  * a `"the [=TERM=] EXPR"` (or `"the {{TERM}} value EXPR"`) prefix — see that
  * node's own doc for why `ExprParser` can't make this call itself.
  *
  * Drops TERM whenever EXPR already carries its own tag (anything other than an
  * untagged `Expr.Seq_`) — the common case, and exactly `TypeAnnotatedPrefix`'s
  * old parse-time behavior, restored here instead. Leaves `TypeAnnotated`
  * wrapping a `Seq_` untouched for now: assigning TERM as the real SpecTec tag
  * there is deferred to a future normalize pass (mirrors `Seq_`'s own doc), not
  * this one's job — this pass only resolves the "drop or keep" ambiguity, not
  * what to do with a kept one.
  *
  * Runs first in the pipeline, before every other pass, so none of them ever
  * has to know `TypeAnnotated` exists: they all expect to match `Expr.Link`/
  * `Expr.Case`/... at the top level directly (`ResolveLinksPass` chief among
  * them), the same as if this node had never existed.
  *
  * Category: Housekeeping.
  */
object ResolveTypeAnnotationPass extends LoweringPass:
  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = a.body.map(resolver.walk)))

  private object resolver extends Walker:
    override def walk(expr: Expr): Expr = expr match
      case Expr.TypeAnnotated(term, e @ Expr.Seq_(_)) =>
        Expr.TypeAnnotated(term, walk(e))
      case Expr.TypeAnnotated(_, e) => walk(e)
      case other                    => super.walk(other)
