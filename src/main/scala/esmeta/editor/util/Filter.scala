package esmeta.editor.util

import esmeta.*
import esmeta.cfg.CFG
import esmeta.editor.sview.*
import esmeta.js.{Ast, Lexical => JsLexical, Syntactic => JsSyntactic}
import esmeta.js.util.{JsonProtocol => JsJsonProtocol}
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import scala.collection.mutable.{Map => MMap, Set => MSet}

case class FilterResult(
  indexedSize: Int,
  indexTime: Long,
  loadingTime: Long,
  filterTime: Long,
  data: MMap[Int, MSet[Int]],
)

case class Filter(cfg: CFG, dataDir: String) {
  import JsonProtocol.given
  import JsJsonProtocol.given

  val tests = readFile(s"$dataDir/test262-list").split(LINE_SEP)
  val index = readJson[ProgramIndex](s"$dataDir/data/index.json")

  // filter function
  def apply(sview: SyntacticView, algoId: Int): Set[String] = {
    // val (t0, pIndex) = time(readJson[ProgramIndex](s"$dataDir/data/index.json"))
    // val (t1, pData) = time {
    //   var map: Map[Int, ProgramInfo] = Map()
    //   for (idx <- 0 until tests.size) {
    //     val path = s"$dataDir/data/$idx.json"
    //     map += (idx -> readJson[ProgramInfo](path))
    //   }
    //   map
    // }

    var result: Set[Int] = Set()

    val simplified = sview.simplify(cfg)
    val programIdxSet = index.getProgramSet(simplified)

    for {
      idx <- programIdxSet
      pdata = PerformanceRecorder("load")(
        readJson[ProgramInfo](s"$dataDir/data/$idx.json"),
      )
    } {
      val matched =
        PerformanceRecorder("matching")(pdata.matches(simplified, algoId, cfg))
      if (matched) result += idx
    }
    println(programIdxSet.size)
    println(result.size)

    // PerformanceRecorder.printStat()
    result.map(tests(_))
  }

  def getAlgos(sview: SyntacticView): Set[Int] = {
    val tests = readFile(s"$dataDir/test262-list").split(LINE_SEP)
    var algoSet: Set[Int] = Set()

    val progress = ProgressBar("filter test262", 0 until tests.size)
    for (idx <- progress) {
      val (annoMap, algoMap, astList) =
        readJson[(Map[Int, Set[Annotation]], Map[Int, Set[Int]], List[Ast])](
          s"$dataDir/data/$idx.json",
        )

      for {
        ast <- astList
        conc <- ast.getConcreteParts(sview, annoMap)
        astId <- conc.idOpt
        aids <- algoMap.get(astId)
      } algoSet ++= aids
    }

    algoSet
  }

  def apply(sview: SyntacticView): FilterResult = apply(sview.simplify(cfg))
  private def apply(sview: SimpleAst): FilterResult = {
    val (indexTime, indexSet) = time(index.getProgramSet(sview))

    // filtering
    var loadingTime = 0L
    var filterTime = 0L
    var data: MMap[Int, MSet[Int]] = MMap()
    for {
      idx <- indexSet
      (idxLoadingTime, info) = time(
        readJson[ProgramInfo](s"$dataDir/data/$idx.json"),
      )
    } {
      // get algos in current program
      val (idxFilterTime, algoSet) = time(info.getAlgos(sview, cfg))

      // collect data
      for { algoId <- algoSet } {
        val programSet = data.getOrElseUpdate(algoId, MSet())
        programSet += idx
      }
      loadingTime += idxLoadingTime
      filterTime += idxFilterTime
    }

    // final result
    FilterResult(indexSet.size, indexTime, loadingTime, filterTime, data)
  }

  def loadBasicSview(): Map[String, SimpleAst] = {
    val cfgHelper = CFGHelper(cfg)
    (for {
      (viewName, sview) <- (new BasicSyntacticView(cfgHelper)).diffViewSet
    } yield viewName -> sview.simplify(cfg)).toMap
  }

  def dumpEvaluataionData(): Unit = {
    val viewSet = loadBasicSview()

    mkdir(s"$dataDir/evaluation")
    val nfSummary = getPrintWriter(s"$dataDir/evaluation/summary.csv")
    def summary(item: Any*): Unit = {
      nfSummary.println(item.mkString(","))
      nfSummary.flush()
    }

    summary(
      "name",
      "indexed size",
      "indexing time",
      "loading time",
      "filter time",
      "algo #",
      "test #",
      "avg test # / algo",
    )

    val progress = ProgressBar("filter test262", viewSet)
    for { pair <- progress } {
      val (viewName, sview) = pair
      val result = apply(sview)
      val testCount = (for {
        set <- result.data.values
        pid <- set
      } yield pid).toSet.size
      val avgTestCount = ((for {
        set <- result.data.values
      } yield set.size).toList.sum.toDouble) / result.data.size
      summary(
        viewName,
        result.indexedSize,
        result.indexTime,
        result.loadingTime,
        result.filterTime,
        result.data.size,
        testCount,
        if (result.data.size == 0) "-" else avgTestCount,
      )

      dumpJson(
        result,
        s"$dataDir/evaluation/$viewName.json",
        noSpace = true,
      )
    }

    nfSummary.close()
  }

  def dumpSoundnessData(): Unit = {
    val viewSet = loadBasicSview()

    mkdir(s"$dataDir/basic_result")

    var data: MMap[String, MSet[Int]] = MMap()
    val progress = ProgressBar("filter test262", 0 until tests.size)
    for (idx <- progress) {
      val info = readJson[ProgramInfo](s"$dataDir/data/$idx.json")
      for { (viewName, sview) <- viewSet } {
        val algoSet = data.getOrElseUpdate(viewName, MSet())
        val pAlgoSet = info.getAlgos(sview, cfg)
        algoSet ++= pAlgoSet
      }
    }

    println("----------------------------------------")
    for { (viewName, algoSet) <- data } {
      val algoList = algoSet.toList.sorted
      println((viewName, algoList.size))

      dumpFile(
        algoList.mkString(LINE_SEP),
        s"$dataDir/soundness/$viewName",
      )
    }
  }
}
