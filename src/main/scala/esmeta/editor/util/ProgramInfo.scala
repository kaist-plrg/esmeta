package esmeta.editor.util

import esmeta.cfg.CFG
import esmeta.editor.sview.*
import esmeta.util.PerformanceRecorder
import scala.collection.mutable.{Map => MMap, Set => MSet}

// program information after evaluation
case class ProgramInfo(
  builtinMap: Map[Int, Set[Int]],
  astInfoMap: Map[Int, List[AstInfo]],
  astList: List[SimpleAst],
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

  def matches(sview: SimpleAst, algoId: Int, cfg: CFG): Boolean = ???

  // {
  //   val idxMap = prodMap.getOrElseUpdate(sview.nameIdx, MMap())
  //   val subIdxMap = idxMap.getOrElseUpdate(sview.idx, MMap())
  //   val astSet = subIdxMap.getOrElseUpdate(sview.subIdx, MSet())

  //   for {
  //     astId <- astSet if astAlgoMap.contains(astId)
  //     ast <- astMap.get(astId)
  //     if PerformanceRecorder("ast matches")(ast.matches(sview, annoMap, cfg))

  //     // TODO fix here
  //     conc <- ast.getConcreteParts(sview)
  //     concAlgoSet <- astAlgoMap.get(conc)
  //   } if (concAlgoSet contains algoId) return true

  //   false
  // }

  case object NotMatched extends Throwable
  def getAlgos(sview: SimpleAst, cfg: CFG): MSet[Int] = {
    val idxMap = prodMap.getOrElseUpdate(sview.nameIdx, MMap())
    val subIdxMap = idxMap.getOrElseUpdate(sview.idx, MMap())
    val astSet = subIdxMap.getOrElseUpdate(sview.subIdx, MSet())

    // get algo set if eval type is matched
    def aux(
      info: AstInfo,
      sview: SimpleAst,
    ): Set[Int] = sview match {
      case lex: SimpleLexical => info.algoSet
      case syn: SimpleSyntactic =>
        var matched = true
        var set: Set[Int] = info.algoSet
        (info.children zip syn.children).foreach {
          case ((childId, Some(i)), sviewChild) =>
            if (matched) {
              val childInfo = astInfoMap(childId)(i)
              try { set ++= aux(childInfo, sviewChild) }
              catch { case NotMatched => matched = false }
            }
          case ((_, None), _) => /* do nothing */
        }
        if (matched) set else throw NotMatched
      case absSyn: SimpleAbsSyntactic =>
        for { evalType <- info.evalTypeOpt }
          if (!evalType.subType(absSyn.annotation))
            throw NotMatched
        Set()
    }

    val result: MSet[Int] = MSet()
    for {
      astId <- astSet
      astInfos <- astInfoMap.get(astId)
      ast <- astMap.get(astId)
      if ast.matches(sview, cfg) // check shape
      astInfo <- astInfos
    }
      try { result ++= aux(astInfo, sview) }
      catch { case NotMatched => /* do nothing */ }

    result
  }
}
