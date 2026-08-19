package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr}
import esmeta.wji.lang.walker.Walker

/** Converts a `Cond.IsType(e, name)` for a WJI-specific NOUN — one
  * `Compiler.typeOf`/mainline `TyModel` don't know as a genuine ECMAScript type
  * — into the real `Cond` its own spec definition actually reduces to. Left as
  * `IsType`, these would compile to `ETypeCheck(_, UnknownTy(...))`, which
  * unconditionally throws `NotSupported` (see `UnknownTy.contains`) the moment
  * it's actually evaluated.
  *
  * `CondParser.ArticleLink` parses "EXPR is a/an [=NOUN=]" uniformly into
  * `IsType(expr, NOUN)` regardless of whether NOUN is a real ECMAScript type
  * (`Number`, `String`, ...) or one of these WJI-specific kinds — English "X is
  * a NOUN" is always kind-membership either way, so there's no reason to parse
  * them differently. This pass is what tells them apart afterward, once it's
  * known which NOUNs the corpus actually needs, in two independent families:
  *
  *   - WebIDL's "exotic wasm object" kinds (`Exported Function`, `Exported GC
  *     Object`) become a `Cond.HasSlot` check: an "Exported Function"
  *     (index.bs:1241: "Built-in Function Objects ... which have a
  *     \[[FunctionAddress]] internal slot") and an "Exported GC Object"
  *     (index.bs:1574: "contains an \[[ObjectAddress]] internal slot") are each
  *     defined purely by the one internal slot they carry, so "is a/an X" and
  *     "has [[SLOT]]" are the exact same claim there.
  *   - WebIDL member-declaration kinds (`regular operation`, `static
  *     operation`) become a `Cond.Eq` against the operation record's own `kind`
  *     field: `esmeta.wji.Initialize`'s `operationRecord` seeds every
  *     `operation` record with `"kind" -> Enum(op.kind.toString)`
  *     (`esmeta.wji.lang.MemberKind`'s case name, e.g. `RegularOperation`), so
  *     "|op| is a [=regular operation=]" is a direct field-equality check
  *     against a tag that's already there — no new record shape needed.
  *   - `reftype` (js-api's `Table` constructor, index.bs:1038: "If
  * |elementtype| is not a [=reftype=], ...") becomes a `Cond.Eq` against
  * `Expr.CaseTag(e)`: `ToValueType` (the only producer of the value this ever
  * checks) only ever returns one of 7 fixed valtypes
  * (i32/i64/f32/f64/v128/funcref/externref, index.bs:1172-1180), and
  * `NormalizeSpecTecCaseShapePass` has already reshaped every one of those into
  * a real `Case` by the time this pass runs — the two ref ones
  * (funcref/externref) tagged `"REF"`, the other five something else — so "is a
  * reftype" is exactly "the runtime value's own tag reads `REF`", read directly
  * off it rather than needing to know SpecTec's actual `reftype` grammar
  * production.
  *
  * Every other `IsType` name passes through untouched — this pass only ever
  * narrows the set of names `Compiler`/`TyModel` are expected to resolve, never
  * widens it.
  *
  * Category: Spec-dependent — WJI.
  */
object ExpandWjiIsTypePass extends LoweringPass:

  private val slotOf: Map[String, String] = Map(
    "Exported Function" -> "FunctionAddress",
    "Exported GC Object" -> "ObjectAddress",
  )

  private val memberKindOf: Map[String, String] = Map(
    "regular operation" -> "RegularOperation",
    "static operation" -> "StaticOperation",
  )

  private object rewriter extends Walker:
    override def walk(cond: Cond): Cond = cond match
      case Cond.IsType(e, name, neg) if slotOf.contains(name) =>
        Cond.HasSlot(walk(e), slotOf(name), neg)
      case Cond.IsType(e, name, neg) if memberKindOf.contains(name) =>
        Cond.Eq(Expr.Field(walk(e), "kind"), Expr.Enum(memberKindOf(name)), neg)
      case Cond.IsType(e, "reftype", neg) =>
        Cond.Eq(Expr.CaseTag(walk(e)), Expr.Str("REF"), neg)
      case other => super.walk(other)

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = a.body.map(rewriter.walk)))
