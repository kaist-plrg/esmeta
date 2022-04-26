package esmeta.editor.util

import esmeta.cfg.CFG
import esmeta.editor.sview.*
import esmeta.util.PerformanceRecorder
import scala.collection.mutable.{Map => MMap, Set => MSet}

// program information after evaluation
case class ProgramInfo(
  annoMap: Map[Int, Set[Annotation]],
  algoMap: Map[Int, Set[Int]],
  astList: List[SimpleAst],
  builtinSet: Set[Int],
) {
  val (astMap, prodMap) = {
    val m0: MMap[Int, SimpleAst] = MMap()
    val m1: MMap[Int, MMap[Int, MMap[Int, MSet[Int]]]] = MMap()

    for { ast <- astList } simpleAstUnitWalker(
      ast,
      { ast0 =>
        m0 += ast0.id -> ast0
        val idxMap = m1.getOrElseUpdate(ast0.nameIdx, MMap())
        val subIdxMap = idxMap.getOrElseUpdate(ast0.idx, MMap())
        val astSet = subIdxMap.getOrElseUpdate(ast0.subIdx, MSet())
        astSet += ast0.id
      },
    )
    (m0, m1)
  }

  def matches(sview: SimpleAst, algoId: Int, cfg: CFG): Boolean =
    val idxMap = PerformanceRecorder("idxMap")(
      prodMap.getOrElseUpdate(sview.nameIdx, MMap()),
    )
    val subIdxMap = PerformanceRecorder("subIdxMap")(
      idxMap.getOrElseUpdate(sview.idx, MMap()),
    )
    val astSet = PerformanceRecorder("astSet")(
      subIdxMap.getOrElseUpdate(sview.subIdx, MSet()),
    )

    for {
      astId <- astSet if algoMap.contains(astId)
      ast <- astMap.get(astId)
      if PerformanceRecorder("ast matches")(ast.matches(sview, annoMap, cfg))
      conc <- ast.getConcreteParts(sview)
      concAlgoSet <- algoMap.get(conc)
    } if (concAlgoSet contains algoId) return true

    false
}
