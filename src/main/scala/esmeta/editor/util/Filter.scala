package esmeta.editor.util

import esmeta.*
import esmeta.cfg.CFG
import esmeta.editor.sview.*
import esmeta.js.{Ast, Lexical => JsLexical, Syntactic => JsSyntactic}
import esmeta.js.util.{JsonProtocol => JsJsonProtocol}
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*

case class Filter(cfg: CFG, dataDir: String) {
  import JsonProtocol.given
  import JsJsonProtocol.given

  // filter function
  def apply(sview: SyntacticView, algoId: Int): Set[String] = {
    val tests = readFile(s"$dataDir/test262-list").split(LINE_SEP)

    val (t0, pIndex) = time(readJson[ProgramIndex](s"$dataDir/data/index.json"))
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
    val programIdxSet = pIndex.getProgramSet(simplified)

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

  // def experiment1(
  //   cfg: CFG,
  //   dataDir: String,
  // ): Set[Int] = {
  //   val tests = readFile(s"$dataDir/test262-list").split(LINE_SEP)
  //   var algoSet: Set[Int] = Set()

  //   val progress = ProgressBar("filter test262", 0 until tests.size)
  //   for (idx <- progress) {
  //     val (annoMap, algoMap, astList) =
  //       readJson[(Map[Int, Set[Annotation]], Map[Int, Set[Int]], List[Ast])](
  //         s"$dataDir/data/$idx.json",
  //       )

  //     for { ast <- astList } {
  //       println("----------------------------------------")
  //       println(ast)
  //       println("----------------------------------------")
  //       println(ast.simplify(cfg))
  //       println("----------------------------------------")
  //       println(ast.toString(grammar = Some(cfg.grammar)))
  //       println("----------------------------------------")
  //       println(ast.simplify(cfg).toString(grammar = Some(cfg.grammar)))

  //       ???
  //     }

  //     // def gatherChains(ast: Ast): List[List[Ast]] =
  //     //   ast match
  //     //     case _: JsLexical => List()
  //     //     case syn: JsSyntactic =>
  //     //       val res = syn.children.flatten.map(gatherChains(_)).flatten
  //     //       val currChains = syn.chains
  //     //       if (currChains.size > 1) currChains :: res else res

  //     // for {
  //     //   ast <- astList
  //     //   chain <- gatherChains(ast)
  //     //   middle <- chain.drop(1).dropRight(1)
  //     //   middleId <- middle.idOpt
  //     // } if (algoMap contains middleId) {
  //     //   println(("!!!", idx))
  //     //   println((chain.head.idOpt.get, middleId, chain.reverse.head.idOpt.get))
  //     //   ???
  //     // }
  //   }

  //   algoSet
  // }

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

  def dumpBasicResult(): Unit = {
    val cfgHelper = CFGHelper(cfg)
    val viewSet = (new BasicSyntacticView(cfgHelper)).viewSet

    mkdir(s"$dataDir/basic_result")

    val tests = readFile(s"$dataDir/test262-list").split(LINE_SEP)

    var data: Map[String, Set[Int]] = Map()
    val progress = ProgressBar("filter test262", 0 until tests.size)
    for (idx <- progress) {
      val (annoMap, algoMap, astList) =
        readJson[(Map[Int, Set[Annotation]], Map[Int, Set[Int]], List[Ast])](
          s"$dataDir/data/$idx.json",
        )

      for { (viewName, sview) <- viewSet } {
        var algoSet: Set[Int] = data.getOrElse(viewName, Set())

        for {
          ast <- astList
          conc <- ast.getConcreteParts(sview, annoMap)
          astId <- conc.idOpt
          aids <- algoMap.get(astId)
        } algoSet ++= aids

        data += viewName -> algoSet
      }
    }

    println("----------------------------------------")
    for { (viewName, algoSet) <- data } {
      val algoList = algoSet.toList.sorted
      println((viewName, algoList.size))

      dumpFile(
        algoList.mkString(LINE_SEP),
        s"$dataDir/basic_result/$viewName",
      )
    }

  }
}
