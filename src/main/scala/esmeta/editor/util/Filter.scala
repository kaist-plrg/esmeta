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
}
