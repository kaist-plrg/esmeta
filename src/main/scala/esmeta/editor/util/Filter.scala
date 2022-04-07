package esmeta.editor.util

import esmeta.test262.*
import esmeta.editor.sview.*
import esmeta.cfg.CFG
import esmeta.spec.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.util.BaseUtils.*
import esmeta.js.Ast
import esmeta.*
import scala.collection.mutable.ListBuffer

case class Filter(
  cfg: CFG,
  config: ConfigSummary,
  useRegex: Boolean = false,
) {
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

  // caching ast of given test262 tests
  private val tests = config.normal.toArray
  private val jsParser = cfg.jsParser("Script")

  // filter function
  def apply(sview: SyntacticView): List[String] = {
    val progress = ProgressBar("filter test262", 0 until tests.size)
    val regexOpt = if (useRegex) Some(s"(?s)${genRegex(sview)}".r) else None

    val filtered: ListBuffer[String] = ListBuffer()
    for (idx <- progress) {
      val NormalConfig(name, _) = tests(idx)
      val jsName = s"$TEST262_TEST_DIR/$name"
      for {
        ast <- regexOpt match
          case Some(regex) =>
            val sourceText = readFile(jsName)
            regex.findFirstIn(sourceText).map(m => jsParser.from(sourceText))
          case None => Some(jsParser.fromFile(jsName))
        if ast contains sview
      } filtered += name
    }
    dumpFile(filtered.toList.sorted.mkString(LINE_SEP), ".filtered.log")
    filtered.toList
  }

}
