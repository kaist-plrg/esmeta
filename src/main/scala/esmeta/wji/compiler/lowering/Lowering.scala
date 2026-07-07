package esmeta.wji.compiler.lowering

import esmeta.wji.lang.Algorithm

object Lowering:
  val pipeline: List[LoweringPass] = List(
    ResolveSpecTermsPass,
    DropNotesPass,
    GroupIfChainPass,
    ExpandAbruptPass,
    ExpandForEachPass,
    ExpandDestructuringLetPass,
    NormalizeAlgoCallPass,
    ExtractInlineAlgoCallPass,
    ExpandThrowsPass,
    ExpandPerformReturnResultPass,
    ExpandAbbreviatedCondPass,
  )

  def run(algos: List[Algorithm]): List[Algorithm] =
    pipeline.foldLeft(algos)((acc, pass) => pass.run(acc))
