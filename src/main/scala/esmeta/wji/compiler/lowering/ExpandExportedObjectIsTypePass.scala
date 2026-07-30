package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond}
import esmeta.wji.lang.walker.Walker

/** Converts a `Cond.IsType(e, name)` for one of WebIDL's "exotic wasm object"
  * kinds (`Exported Function`, `Exported GC Object`) into the `Cond.HasSlot`
  * check its own spec definition actually reduces to — these aren't genuine
  * ECMAScript types `Compiler.typeOf`/mainline `TyModel` know about, so left as
  * `IsType`, they'd compile to `ETypeCheck(_, UnknownTy(...))`, which
  * unconditionally throws `NotSupported` (see `UnknownTy.contains`) the moment
  * it's actually evaluated.
  *
  * `CondParser.ArticleLink` parses "EXPR is a/an [=NOUN=]" uniformly into
  * `IsType(expr, NOUN)` regardless of whether NOUN is a real ECMAScript type
  * (`Number`, `String`, ...) or one of these WJI-specific exotic kinds —
  * English "X is a NOUN" is always kind-membership either way, so there's no
  * reason to parse them differently. This pass is what tells the two apart
  * afterward, once it's known which NOUNs the corpus actually needs: an
  * "Exported Function" (index.bs:1241: "Built-in Function Objects ... which
  * have a \[[FunctionAddress]] internal slot") and an "Exported GC Object"
  * (index.bs:1574: "contains an \[[ObjectAddress]] internal slot") are each
  * defined purely by the one internal slot they carry, so "is a/an X" and "has
  * [[SLOT]]" are the exact same claim there.
  *
  * Every other `IsType` name (including ones not yet mechanized, e.g.
  * `reftype`) passes through untouched — this pass only ever narrows the set of
  * names `Compiler`/`TyModel` are expected to resolve, never widens it.
  *
  * Category: Spec-dependent — WJI.
  */
object ExpandExportedObjectIsTypePass extends LoweringPass:

  private val slotOf: Map[String, String] = Map(
    "Exported Function" -> "FunctionAddress",
    "Exported GC Object" -> "ObjectAddress",
  )

  private object rewriter extends Walker:
    override def walk(cond: Cond): Cond = cond match
      case Cond.IsType(e, name, neg) if slotOf.contains(name) =>
        Cond.HasSlot(walk(e), slotOf(name), neg)
      case other => super.walk(other)

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = a.body.map(rewriter.walk)))
