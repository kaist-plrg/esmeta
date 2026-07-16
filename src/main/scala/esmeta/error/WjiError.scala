package esmeta.error

/** errors from `esmeta.wji`, ESMeta's WebAssembly JS-API compiler pipeline */
sealed abstract class WjiError(msg: String) extends ESMetaError(msg, "WjiError")

/** An AST shape that an earlier `esmeta.wji.compiler.lowering` pass guarantees
  * eliminates every occurrence of before `esmeta.wji.compiler.Compiler` ever
  * runs. Thrown immediately when `Compiler` reaches such a shape, unlike `EYet`
  * (which defers failure to IR evaluation), so a regression in the lowering
  * pipeline is caught the moment the offending algorithm is compiled, rather
  * than only when some later interpreter run happens to exercise that exact
  * path.
  */
case class UnreachableAfterLowering(msg: String)
  extends WjiError(s"unreachable after lowering: $msg")

/** An AST shape that a `esmeta.wji.compiler.lowering` pass only partially
  * covers by design — e.g. `NewByteSequence` only in direct `Let`/`Return` RHS
  * position, a destructuring `Let` only when every bound name is a bare `Var` —
  * because every spec occurrence seen so far fits that narrower shape. Thrown
  * by the pass itself the moment it finds an occurrence outside that documented
  * shape, rather than silently leaving it for `Compiler`'s own, much later (and
  * less specific) `EYet` fallback: failing here names the exact pass and
  * construct that needs to be generalized, at the point in the pipeline where
  * that's actually knowable.
  */
case class UnsupportedSpecShape(pass: String, msg: String)
  extends WjiError(s"[$pass] $msg")

/** `esmeta.wji.compiler.lowering.Lowering.pipeline`'s actual order doesn't
  * satisfy some pass's declared `LoweringPass.requires`/`mustPrecede`. Thrown
  * by `Lowering.run` before any algorithm is touched, so a pipeline reordering
  * that breaks a real dependency fails immediately and by name, instead of
  * silently mis-lowering (or not lowering at all) whatever depended on it.
  */
case class PipelineOrderError(msg: String) extends WjiError(msg)
