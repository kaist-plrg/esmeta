package esmeta.wji.compiler.desugar

import esmeta.wji.lang.Algorithm

object Desugar:
  val pipeline: List[DesugarPass] = List(
    ResolveSpecTermsPass,
    ReplaceSpaceWithUnderscore,
    DropNotesPass,
    GroupIfChainPass,
    ExpandAbruptPass,
    ExpandForEachPass,
    ExpandDestructuringLetPass,
    NormalizeAlgoCallPass,
    ExtractInlineAlgoCallPass,
    ExpandPerformReturnResultPass,
    ExpandAbbreviatedCondPass,
  )

  def run(algos: List[Algorithm]): List[Algorithm] =
    pipeline.foldLeft(algos)((acc, pass) => pass.run(acc))
