package esmeta.editor.util

import scala.collection.mutable.{Map => MMap, Set => MSet}
import esmeta.editor.sview.*

// TODO try bitset?
// TODO more raw encoding of key of prods?
case class ProgramIndex(
  prods: MMap[Int, MMap[Int, MMap[Int, MSet[Int]]]] = MMap(),
  algos: MMap[Int, MSet[Int]] = MMap(),
) {
  def update(
    programIdx: Int,
    info: ProgramInfo,
  ): Unit =
    updateAlgos(programIdx, info.astAlgoMap)
    updateAlgos(programIdx, info.builtinAlgoMap)
    updateProds(programIdx, info.astList)

  def updateAlgos(programIdx: Int, algoMap: Map[Int, Set[Int]]): Unit =
    val algoSet: MSet[Int] = MSet()
    for { astAlgoSet <- algoMap.values } algoSet ++= astAlgoSet
    for { algoId <- algoSet } {
      val programIdxSet = algos.getOrElseUpdate(algoId, MSet())
      programIdxSet += programIdx
    }

  def updateProds(programIdx: Int, astList: List[SimpleAst]): Unit =
    def aux(ast: SimpleAst): Unit =
      val idxMap = prods.getOrElseUpdate(ast.nameIdx, MMap())
      val subIdxMap = idxMap.getOrElseUpdate(ast.idx, MMap())
      val programIdxSet = subIdxMap.getOrElseUpdate(ast.subIdx, MSet())
      programIdxSet += programIdx
      ast match
        case syn: SimpleSyntactic =>
          for { child <- syn.children } aux(child)
        case _ =>
    for { ast <- astList } aux(ast)

  def getProgramSet(sview: SimpleAst): Set[Int] =
    val idxMap = prods.getOrElseUpdate(sview.nameIdx, MMap())
    val subIdxMap = idxMap.getOrElseUpdate(sview.idx, MMap())
    val programIdxSet = subIdxMap.getOrElseUpdate(sview.subIdx, MSet())
    programIdxSet.toSet
}
