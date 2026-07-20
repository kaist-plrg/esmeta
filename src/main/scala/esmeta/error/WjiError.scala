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
