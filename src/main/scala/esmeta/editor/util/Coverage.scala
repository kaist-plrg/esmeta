package esmeta.editor.util

import esmeta.cfg.*
import esmeta.editor.sview.*
import esmeta.editor.models.*
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
) {
  lazy val test262 = Test262(cfg.spec)
  lazy val test262Config =
    test262List.fold(test262.config)(TestFilter.fromFile)
  lazy val tests = test262Config.normal.toArray
  lazy val jsParser = cfg.jsParser("Script")

  // prepare to dump data
  dumpDirOpt.foreach { dumpDir =>
    mkdir(dumpDir)
    // mkdir(s"$dumpDir/data")
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
    def run(sourceText: String, ast: Ast): Set[Int] = {
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
          run(sourceText, ast)
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

    // initialize
    val dumpDir = dumpDirOpt.get
    var nextAstId = 0

    def run(testBody: Ast, includes: List[String]) = {
      nextAstId += testBody.setId(nextAstId)
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

        def getAncestors(ast: Ast, until: Option[Ast]): List[Ast] =
          ast.parent match
            case Some(parent) =>
              if (until.fold(false)(_ == parent)) List(ast)
              else ast :: getAncestors(parent, until)
            case None => List(ast)

        // if current context is evaluation, save type of value
        override def setReturn(value: Value): Unit = {
          super.setReturn(value)

          if (this.st.context.name endsWith ".Evaluation") {
            // handle evaluation chain
            val parentEvalAstOpt =
              contexts.tail
                .filter(c => c.name endsWith ".Evaluation")
                .flatMap(_.astOpt)
                .headOption
            for {
              currentAst <- this.st.context.astOpt
              ast <- getAncestors(currentAst, parentEvalAstOpt)
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

    val conn = SqliteConnection(s"$dumpDir/data.db")
    val progress =
      ProgressBar("measure algorithms per ast", 0 until tests.size)

    for (idx <- progress) {
      val NormalConfig(name, includes) = tests(idx)
      val testBody = jsParser.fromFile(s"$TEST262_TEST_DIR/$name")
      val (annoMap, algoMap, astList) = run(testBody, includes)

      // store execution info to database
      for { ast <- astList } { conn.storeAst(ast, idx) }
      for {
        (astId, annoSet) <- annoMap
        anno <- annoSet
      } conn.storeAstAnno(astId, anno.toInt)
      for {
        (astId, algoSet) <- algoMap
        algoId <- algoSet
      } conn.storeAstAlgo(astId, algoId)

      // dumpDirOpt.foreach { dumpDir =>
      //   dumpJson(
      //     (annoMap, algoMap, astList),
      //     s"$dumpDir/data/$idx.json",
      //     noSpace = true,
      //   )
      // }
    }

    // close connection
    conn.executeAndCommit(
      "CREATE INDEX name_idx ON Ast(name)",
    )
    conn.close()
  }
}
