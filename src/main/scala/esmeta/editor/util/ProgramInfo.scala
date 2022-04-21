package esmeta.editor.util

import esmeta.editor.sview.*
import scala.collection.mutable.{Map => MMap, Set => MSet}

// program information after evaluation
case class ProgramInfo(
  annoMap: Map[Int, Set[Annotation]],
  algoMap: Map[Int, Set[Int]],
  astList: List[SimpleAst],
) {
  lazy val (astMap, prodMap) = {
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
}
