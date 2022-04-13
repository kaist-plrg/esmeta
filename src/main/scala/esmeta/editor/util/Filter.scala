package esmeta.editor.util

import esmeta.*
import esmeta.cfg.CFG
import esmeta.editor.sview.*
import esmeta.js.Ast
import esmeta.spec.*
import esmeta.test262.*
import esmeta.test262.util.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import scala.collection.mutable.ListBuffer

case class Filter(
  cfg: CFG,
  test262List: Option[String] = None,
  useRegex: Boolean = false,
  nidOpt: Option[Int] = None,
) {
  lazy val test262 = Test262(cfg.spec)
  lazy val test262Config =
    test262List.fold(test262.config)(TestFilter.fromFile)
  lazy val tests = test262Config.normal.toArray
  lazy val jsParser = cfg.jsParser("Script")

  // generate regexp that matches syntactic view
  private def genRegex(sv: SyntacticView): String = sv match
    case _: AbsSyntactic => ".*"
    case Syntactic(name, _, rhsIdx, children) =>
      val prod = cfg.grammar.nameMap(name)
      val rhs = prod.rhsList(rhsIdx)
      val arr = children.toArray
      var ntIdx = 0
      (rhs.symbols
        .flatMap {
          case Terminal(term) => Some(s"\\Q$term\\E")
          case sym =>
            sym.getNt match
              case Some(nt) =>
                val res = arr(ntIdx).fold("")(genRegex)
                ntIdx += 1
                Some(res)
              case None => None
        })
        .mkString("\\s*")
    case lex: Lexical => s"\\Q${lex.str}\\E"

  // filter function
  def apply(sview: SyntacticView): List[String] = {
    val progress = ProgressBar("filter test262", 0 until tests.size)
    val regexOpt = if (useRegex) Some(s"(?s)${genRegex(sview)}".r) else None

    val filtered: ListBuffer[String] = ListBuffer()
    var totalSize: Long = 0L
    for (idx <- progress) {
      val NormalConfig(name, includes) = tests(idx)
      val jsName = s"$TEST262_TEST_DIR/$name"
      for {
        ast <- regexOpt match
          case Some(regex) =>
            val sourceText = readFile(jsName)
            regex.findFirstIn(sourceText).map(m => jsParser.from(sourceText))
          case None =>
            val parsed = jsParser.fromFile(jsName)
            totalSize += parsed.size
            Some(parsed)
        if ast contains sview
      } nidOpt match {
        case None => filtered += name
        case Some(nid) =>
          val (_, testAst) = test262.loadTest(ast, includes)
          ???
        // if (testAst.touched(cfg, sview, nid)) filtered += name
      }
    }
    // println(("!!!", totalSize / tests.size.toFloat))
    dumpFile(filtered.toList.sorted.mkString(LINE_SEP), ".filtered.log")
    filtered.toList
  }

}
