package esmeta.editor.util

import esmeta.*
import esmeta.cfg.CFG
import esmeta.editor.sview.*
import esmeta.js.Ast
import esmeta.js.util.{JsonProtocol => JsJsonProtocol}
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*

object Filter {
  import JsonProtocol.given
  import JsJsonProtocol.given

  def apply(
    cfg: CFG,
    dataDir: String,
    sview: SyntacticView,
  ): (List[String], Set[Int]) = {
    val tests = readFile(s"$dataDir/test262-list").split(LINE_SEP)
    var testSet: Set[String] = Set()
    var algoSet: Set[Int] = Set()
    val progress = ProgressBar("filter test262", 0 until tests.size)

    var t0 = 0L
    var t1 = 0L

    for (idx <- progress) {
      val (ltime, (annoMap, algoMap, astList)) =
        time(
          readJson[(Map[Int, Set[Annotation]], Map[Int, Set[Int]], List[Ast])](
            s"$dataDir/data/$idx.json",
          ),
        )

      val (ctime, _) = time(
        for {
          ast <- astList
          conc <- ast.getConcreteParts(sview, annoMap)
          astId <- conc.idOpt
          aids <- algoMap.get(astId)
        } { algoSet ++= aids; testSet += tests(idx) },
      )

      t0 += ltime
      t1 += ctime
    }

    println(t0)
    println(t1)

    (testSet.toList.sorted, algoSet)
  }
}
