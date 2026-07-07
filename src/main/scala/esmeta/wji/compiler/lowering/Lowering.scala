package esmeta.wji.compiler.lowering

import esmeta.wji.lang.Algorithm

object Lowering:
  val pipeline: List[LoweringPass] = List(
    ResolveLinksPass,
    DropNotesPass,
    GroupIfChainPass,
    ExpandAbruptPass,
    ExpandForEachPass,
    ExpandForPass,
    ExpandNewByteSequencePass,
    ExpandDestructuringLetPass,
    NormalizeAlgoCallPass,
    ExtractInlineAlgoCallPass,
    ExpandThrowsPass,
    ExpandPerformReturnResultPass,
    ExpandAbbreviatedCondPass,
    ExpandQueueATaskPass,
    ReplaceSpaceWithUnderscore,
  )

  def run(algos: List[Algorithm]): List[Algorithm] =
    pipeline.foldLeft(algos)((acc, pass) => pass.run(acc))
