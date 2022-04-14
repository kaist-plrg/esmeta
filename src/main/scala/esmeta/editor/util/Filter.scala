package esmeta.editor.util

import esmeta.*
import esmeta.cfg.CFG
import esmeta.editor.sview.*
import esmeta.js.Ast
import esmeta.js.util.JsonProtocol
import esmeta.spec.*
import esmeta.test262.*
import esmeta.test262.util.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

case class Filter(
  cfg: CFG,
  dataDir: String,
) {
  // load test262 data
  // val TODOs = List(18252)
  val tests = readFile(s"$dataDir/test262-list").split(LINE_SEP)
  val test262Data = {
    import JsonProtocol.given
    val buf = ArrayBuffer[(Map[Int, Set[Int]], List[Ast])]()
    val progress = ProgressBar("load test262 data", 0 until tests.size)
    for (idx <- progress)
      // if (TODOs contains idx)
      buf.append(
        readJson[(Map[Int, Set[Int]], List[Ast])](s"$dataDir/data/$idx.json"),
      )
    buf.toArray
  }

  // filter function
  def apply(sview: SyntacticView): List[(String, Set[Int])] = {
    val filtered: ListBuffer[(String, Set[Int])] = ListBuffer()

    val progress = ProgressBar("filter test262", 0 until tests.size)
    for (idx <- progress) {
      // if (TODOs contains idx) {
      val name = tests(idx)
      // val (algoMap, astList) = test262Data(TODOs.indexOf(idx))
      val (algoMap, astList) = test262Data(idx)

      def getTouched(ast: Ast, view: SyntacticView): Set[Int] =
        for {
          conc <- ast.getConcreteParts(view)
          algoId <- conc.idOpt match
            case Some(astId) => algoMap.getOrElse(astId, Set())
            case None        => Set()
        } yield algoId

      val matched = astList.filter(_ contains sview).toSet
      val touched = matched.flatMap(getTouched(_, sview))
      if (touched.size > 0) {
        filtered += ((name, touched))

        // println("----------------------------------------")
        // println((name, touched))
      }
      // }
    }

    // dumpFile(filtered.toList.sorted.mkString(LINE_SEP), ".filtered.log")
    filtered.toList
  }

}
