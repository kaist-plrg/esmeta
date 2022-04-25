package esmeta.editor.util

import esmeta.cfg.*
import esmeta.cfg.CFG
import esmeta.editor.sview.*
import esmeta.interp.*
import esmeta.ir.*
import esmeta.js.util.{JsonProtocol => JsJsonProtocol}
import esmeta.js.{
  Ast,
  Initialize,
  Syntactic => JsSyntactic,
  Lexical => JsLexical,
}
import esmeta.spec.*
import esmeta.test262.*
import esmeta.test262.util.TestFilter
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.{TEST262_TEST_DIR, BASE_DIR, LINE_SEP}
import scala.collection.mutable.{ListBuffer, Map => MMap, Set => MSet}

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

  // simplifying evaluation info
  class Simplifier(
    annoMap: Map[Int, Set[Annotation]],
    algoMap: Map[Int, Set[Int]],
    astList: List[Ast],
    builtinSet: Set[Int],
  ) {
    lazy val result = {
      val simplified = astList.map(simplifyAst(_))
      ProgramInfo(
        for { (origId, annoSet) <- annoMap } yield idMap(origId) -> annoSet,
        for { (origId, algoSet) <- algoMap } yield idMap(origId) -> algoSet,
        simplified,
        for { origId <- builtinSet } yield idMap(origId),
      )
    }

    private var nextId = 0
    private def reassign(origId: Int): Int =
      val newId = nextId;
      nextId += 1
      idMap += origId -> newId
      newId
    private var idMap: Map[Int, Int] = Map()

    // trim until *.Evaluation
    private def trim(ast: Ast): Ast =
      ast match
        case syn @ JsSyntactic(name, _, rhsIdx, children) =>
          val cs = children.flatten

          cs match {
            case List(child) =>
              val rhs = cfg.grammar.nameMap(name).rhsList(rhsIdx)
              if (rhs.terminals.isEmpty) {
                val fname = s"${name}[${rhsIdx},${syn.subIdx(cfg)}].Evaluation"
                cfg.fnameMap.get(fname) match
                  case _: Some[_] => ast
                  case None       => trim(child)
              } else ast
            case _ => ast
          }
        case _ => ast

    // simplify ast and re-assign id
    private def simplifyAst(ast: Ast): SimpleAst = {
      val newId = reassign(ast.idOpt.get)
      val subIdx = ast.subIdx(cfg)
      val nameIdx = cfg.grammar.getNameIdx(ast.name)
      ast match
        case JsLexical(_, str) => SimpleLexical(nameIdx, str).setId(newId)
        case JsSyntactic(_, _, idx, children) =>
          SimpleSyntactic(
            nameIdx,
            idx,
            subIdx,
            children.flatten.map(trim(_)).map(simplifyAst(_)),
          ).setId(newId)
    }
  }

  // get touched algorithms per ast
  def touchedAlgos = {
    import JsJsonProtocol.given
    import JsonProtocol.given

    // fix interp for collection evaluation info
    def getTouched(testBody: Ast, includes: List[String]) = {
      var nextAstId = testBody.setId(0)
      val (sourceText, ast) = test262.loadTest(testBody, includes)
      val st = initState(sourceText, ast)

      // run interp
      var builtinSet: Set[Int] = Set()
      var algoMap: Map[Int, Set[Int]] = Map()
      var annoMap: Map[Int, Set[Annotation]] = Map()
      var astList: ListBuffer[Ast] = ListBuffer(testBody)
      new Interp(st, Nil) {
        private def contexts =
          this.st.context :: this.st.callStack.map(_.context)
        private def evalAstList =
          contexts.flatMap { c =>
            if (c.name endsWith ".Evaluation") c.astOpt
            else None
          }
        private def astStack = contexts.flatMap(_.astOpt)

        // decide which function not to be tracked
        private var untrackedStack: List[Unit] = List()
        def isUntracked: Boolean =
          this.st.context.name == "GetValue" || this.st.context.func.isBuiltin

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
          // interp node
          super.interp(node)

          node match {
            case _: Call if untrackedStack.isEmpty =>
              // get top-most ast
              val currAstOpt = evalAstList.headOption

              // save algo id of current context
              for {
                ast <- currAstOpt
                astId <- ast.idOpt
                algoIds = algoMap.getOrElse(astId, Set())
              }
                if (this.st.context.func.isBuiltin) builtinSet += astId
                else algoMap += (astId -> (algoIds + this.st.context.func.id))

              // mark untracked call
              if (isUntracked) untrackedStack ::= ()
            case _ => /* do nothing */
          }
        }

        // if current context is evaluation, save type of value
        override def setReturn(value: Value): Unit = {
          super.setReturn(value)

          // save evaluation result
          if (this.st.context.name endsWith ".Evaluation") {
            for {
              currentAst <- this.st.context.astOpt
              astId <- currentAst.idOpt
              annotation <- value.toAnnotation(this.st)
              annotationSet = annoMap.getOrElse(astId, Set())
            } annoMap += (astId -> (annotationSet + annotation))
          }
          // handle GetValue call
          else if (isUntracked && !untrackedStack.isEmpty)
            untrackedStack = untrackedStack.tail
        }
      }.fixpoint

      // check exit and return result
      st(GLOBAL_RESULT) match
        case comp: Comp if comp.ty == CONST_NORMAL =>
          (annoMap, algoMap, astList.toList, builtinSet)
        case v => error(s"not normal exit")
    }

    // progress bar
    val progress =
      ProgressBar("measure algorithms per ast", 0 until tests.size)

    // create global index of programs
    val pIndex = ProgramIndex()

    // runner
    for (idx <- progress) {
      val NormalConfig(name, includes) = tests(idx)
      val testBody = jsParser.fromFile(s"$TEST262_TEST_DIR/$name")
      val (annoMap, algoMap, astList, builtinSet) =
        getTouched(testBody, includes)

      // simplify result and update global index
      val info = new Simplifier(annoMap, algoMap, astList, builtinSet).result
      pIndex.update(idx, info)

      // dump result
      for { dumpDir <- dumpDirOpt } dumpJson(
        info,
        s"$dumpDir/data/$idx.json",
        noSpace = true,
      )
    }

    // dump index result
    for { dumpDir <- dumpDirOpt } dumpJson(
      pIndex,
      s"$dumpDir/data/index.json",
      noSpace = true,
    )
  }
}
