package esmeta.editor.util

import esmeta.*
import esmeta.cfg.CFG
import esmeta.editor.sview.*
import esmeta.js.{Ast, Lexical => JsLexical, Syntactic => JsSyntactic}
import esmeta.js.util.{JsonProtocol => JsJsonProtocol}
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*

object Filter {
  import JsonProtocol.given
  import JsJsonProtocol.given

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

  def getAlgos(
    cfg: CFG,
    dataDir: String,
    sview: SyntacticView,
  ): Set[Int] = {
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

  def dumpBasicResult(cfg: CFG, dataDir: String): Unit = {
    val cfgHelper = CFGHelper(cfg)
    val viewSet = (new BasicSyntacticView(cfgHelper)).viewSet

    // for { (viewName, sview) <- viewSet }
    //   println((viewName, sview))
    // ???

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
