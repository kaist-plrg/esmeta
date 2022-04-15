package esmeta.editor.util

import esmeta.cfg.*
import esmeta.cfg.CFG
import esmeta.editor.sview.*
import esmeta.interp.*
import esmeta.ir.*
import esmeta.js.util.{JsonProtocol => JsJsonProtocol}
import esmeta.js.{Ast, Initialize}
import esmeta.spec.*
import esmeta.test262.*
import esmeta.test262.util.TestFilter
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.{TEST262_TEST_DIR, BASE_DIR, LINE_SEP}
import scala.collection.mutable.ListBuffer

case class Coverage(
  cfg: CFG,
  test262List: Option[String] = None,
  dumpDirOpt: Option[String] = None,
  loadDirOpt: Option[String] = None,
) {
  lazy val test262 = Test262(cfg.spec)
  lazy val test262Config =
    test262List.fold(test262.config)(TestFilter.fromFile)
  lazy val tests = test262Config.normal.toArray
  lazy val jsParser = cfg.jsParser("Script")

  // prepare to dump data
  dumpDirOpt.foreach { dumpDir =>
    mkdir(dumpDir)
    mkdir(s"$dumpDir/data")
    dumpFile(
      tests.map(_.name).mkString(LINE_SEP),
      s"$dumpDir/test262-list",
    )
    dumpFile(cfg.spec.version, s"$dumpDir/ecma262-version")
    dumpFile(currentVersion(BASE_DIR), s"$dumpDir/esmeta-version")
  }

  def initState(sourceText: String, ast: Ast): State =
    Initialize(cfg, sourceText, Some(ast))

  // get touched nodes
  def touchedNodes: Set[Int] = {
    // progress bar
    val progress =
      ProgressBar("measure test262 coverage", 0 until tests.size)

    // total touched node id
    var totalTouched: Set[Int] = Set()

    // run interp
    def getTouched(sourceText: String, ast: Ast): Set[Int] = {
      val st = initState(sourceText, ast)
      var touched: Set[Int] = Set()

      // run interp
      new Interp(st, Nil) {
        override def interp(node: Node): Unit = {
          touched += node.id; super.interp(node)
        }
      }.fixpoint

      // check exit and return result
      st(GLOBAL_RESULT) match
        case comp: Comp if comp.ty == CONST_NORMAL => touched
        case v                                     => error(s"not normal exit")
    }

    // run test262 and get touched nodes for each test
    for (idx <- progress) {
      val NormalConfig(name, includes) = tests(idx)
      val touched =
        try {
          val (sourceText, ast) =
            test262.loadTestFromFile(s"$TEST262_TEST_DIR/$name")
          loadDirOpt match
            case None          => getTouched(sourceText, ast)
            case Some(loadDir) => readJson[Set[Int]](s"$loadDir/data/$idx.json")
        } catch { case _: Throwable => Set() }
      dumpDirOpt.foreach { dumpDir =>
        dumpJson(touched, s"$dumpDir/data/$idx.json", noSpace = true)
      }
      totalTouched ++= touched
    }
    totalTouched
  }

  // get touched algorithms per ast
  def touchedAlgos = {
    import JsJsonProtocol.given
    import JsonProtocol.given

    def getTouched(testBody: Ast, includes: List[String]) = {
      var nextAstId = testBody.setId(0)
      val (sourceText, ast) = test262.loadTest(testBody, includes)
      val st = initState(sourceText, ast)

      // run interp
      var algoMap: Map[Int, Set[Int]] = Map()
      var annoMap: Map[Int, Set[Annotation]] = Map()
      var astList: ListBuffer[Ast] = ListBuffer(testBody)
      new Interp(st, Nil) {
        private def contexts =
          this.st.context :: this.st.callStack.map(_.context)
        private def astStack = contexts.flatMap(_.astOpt)

        // handle dynamically created ast
        override def interp(expr: Expr): Value = {
          val v = super.interp(expr)
          (expr, v) match
            case (_: EParse, AstValue(ast))
                if astStack.headOption.fold(false)(_.idOpt.isDefined) =>
              nextAstId += ast.setId(nextAstId)
              astList.append(ast)
              AstValue(ast)
            case _ => v
        }

        // save algo id of top-most evaluation
        override def interp(node: Node): Unit = {
          super.interp(node)
          node match {
            case _: Call =>
              // get top-most ast
              val targets = contexts.flatMap { c =>
                if (c.name endsWith ".Evaluation") c.astOpt
                else None
              }

              // save algo id of current context
              for {
                ast <- targets.headOption
                astId <- ast.idOpt
                algoIds = algoMap.getOrElse(astId, Set())
              } algoMap += (astId -> (algoIds + this.st.context.func.id))
            case _ => /* do nothing */
          }
        }

        // if current context is evaluation, save type of value
        override def setReturn(value: Value): Unit = {
          super.setReturn(value)

          if (this.st.context.name endsWith ".Evaluation") {
            for {
              ast <- this.st.context.astOpt
              astId <- ast.idOpt
              annotation <- value.toAnnotation(this.st)
              annotationSet = annoMap.getOrElse(astId, Set())
            } annoMap += (astId -> (annotationSet + annotation))
          }
        }
      }.fixpoint

      // check exit and return result
      st(GLOBAL_RESULT) match
        case comp: Comp if comp.ty == CONST_NORMAL =>
          (annoMap, algoMap, astList.toList)
        case v => error(s"not normal exit")
    }

    // progress bar
    val progress =
      ProgressBar("measure algorithms per ast", 0 until tests.size)

    for (idx <- progress) {
      val NormalConfig(name, includes) = tests(idx)
      val (annoMap, algoMap, astList) = loadDirOpt match
        case None =>
          val testBody = jsParser.fromFile(s"$TEST262_TEST_DIR/$name")
          getTouched(testBody, includes)
        case Some(loadDir) =>
          readJson[(Map[Int, Set[Annotation]], Map[Int, Set[Int]], List[Ast])](
            s"$loadDir/data/$idx.json",
          )
      dumpDirOpt.foreach { dumpDir =>
        dumpJson(
          (annoMap, algoMap, astList),
          s"$dumpDir/data/$idx.json",
          noSpace = true,
        )
      }
    }
  }
}
