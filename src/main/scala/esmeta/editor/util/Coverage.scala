package esmeta.editor.util

import esmeta.cfg.*
import esmeta.editor.sview.*
import esmeta.interp.*
import esmeta.ir.*
import esmeta.js.{
  Ast,
  Initialize,
  Syntactic => JsSyntactic,
  Lexical => JsLexical,
}
import esmeta.spec.*
import esmeta.test262.*
import esmeta.test262.util.TestFilter
import esmeta.error.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.{TEST262_TEST_DIR, BASE_DIR, LINE_SEP}
import scala.collection.mutable.{ListBuffer, Map => MMap, Set => MSet}

// helper class for eval info recording
trait EvalInfo {
  val id: Int
  val algoSet: MSet[Int]
  def +=(algoId: Int): Unit = algoSet += algoId
  def copied: EvalInfo = this match
    case BuiltinEvalInfo(id, algoSet) => BuiltinEvalInfo(id, MSet.from(algoSet))
    case AstEvalInfo(id, algoSet, assignMap) =>
      AstEvalInfo(id, MSet.from(algoSet), MMap.from(assignMap))
}
case class BuiltinEvalInfo(
  id: Int,
  algoSet: MSet[Int],
) extends EvalInfo
case class AstEvalInfo(
  id: Int,
  algoSet: MSet[Int],
  assignMap: MMap[Int, Option[Int]],
) extends EvalInfo {
  def toAstInfo(evalTypeOpt: Option[Annotation]): AstInfo = AstInfo(
    assignMap.toList.sortBy(_._1),
    evalTypeOpt,
    algoSet.toSet,
  )
}

// helper class for ast info recording
case class AstInfo(
  children: List[(Int, Option[Int])],
  evalTypeOpt: Option[Annotation],
  algoSet: Set[Int],
)

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
    builtinMap: MMap[Int, MSet[Int]],
    astInfoMap: MMap[Int, ListBuffer[AstInfo]],
    astList: ListBuffer[Ast],
  ) {
    // get program info
    lazy val result = {
      val simplified = astList.map(simplifyAst(_))
      ProgramInfo(
        Map.from(for {
          (bid, algoSet) <- builtinMap
        } yield bid -> Set.from(algoSet)),
        Map.from(for {
          (origAstId, origAstInfos) <- astInfoMap
          astInfos = origAstInfos.map {
            case AstInfo(children, evalType, algoSet) =>
              val cs = children.map {
                case (origChildId, idxOpt) => (idMap(origChildId), idxOpt)
              }
              AstInfo(cs, evalType, algoSet)
          }.toList
        } yield idMap(origAstId) -> astInfos),
        simplified.toList,
      )
    }

    // map origin ast id to new ast id
    private var nextId = 0
    private def reassign(origId: Int): Int =
      val newId = nextId;
      nextId += 1
      idMap += origId -> newId
      newId
    private var idMap: Map[Int, Int] = Map()

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
            children.flatten.map(_.trim(cfg)).map(simplifyAst(_)),
          ).setId(newId)
    }
  }

  // get touched algorithms per ast
  def touchedAlgos = {
    import JsonProtocol.given

    // fix interp for collection evaluation info
    def getInfo(testBody: Ast, includes: List[String]): ProgramInfo = {
      var nextAstId = testBody.setId(0)
      val (sourceText, ast) = test262.loadTest(testBody, includes)
      val _st = initState(sourceText, ast)

      // global data structure for collecting evaluation info
      val builtinMap: MMap[Int, MSet[Int]] = MMap()
      val astInfoMap: MMap[Int, ListBuffer[AstInfo]] = MMap()
      val astList: ListBuffer[Ast] = ListBuffer(testBody)

      // run interp
      new Interp(_st, Nil) {
        // shortcuts
        private def contexts = st.context :: st.callStack.map(_.context)
        private def astStack = contexts.flatMap(_.astOpt)
        private def evalAstStack =
          contexts.filter(_.name endsWith ".Evaluation").flatMap(_.astOpt)

        // aux data structure for creating evaluation info
        private var evalInfoStack: List[EvalInfo] = List()
        private def popEvalInfoStack: EvalInfo = evalInfoStack match
          case h :: t => evalInfoStack = t; h
          case _      => error("eval info stack is empty")

        // handle dynamically created ast
        override def interp(expr: Expr): Value = {
          val v = super.interp(expr)
          (expr, v) match
            case (_: EParse, AstValue(ast))
                if astStack.headOption.fold(false)(_.idOpt.isDefined) =>
              nextAstId += ast.setId(nextAstId)
              astList.append(ast)
              AstValue(ast)
            case (_: ECont, cont: Cont) =>
              // record current eval info to continuation
              cont.evalInfoStack = evalInfoStack.map(_.copied)
              cont
            case _ => v
        }

        // override transition for calls
        override def call(
          lhs: Id,
          fexpr: Expr,
          args: List[Expr],
        ): Unit = try {
          interp(fexpr).escaped match {
            case Clo(func, captured) =>
              val vs = args.map(interp)
              val newLocals = getLocals(func.irFunc.params, vs) ++ captured
              st.callStack ::= CallContext(lhs, st.context)
              st.context = Context(func, newLocals)

              // record algo id to algoSetStack
              for { ast <- evalAstStack.headOption if ast.idOpt.isDefined } {
                if (func.isBuiltin)
                  // add new info context
                  evalInfoStack ::= BuiltinEvalInfo(func.id, MSet(func.id))
                else if (func.name == "GetValue")
                  // mark GetValue calling ast
                  evalInfoStack.head.algoSet += func.id
                else
                  contexts.foldLeft(false) {
                    case (false, c) =>
                      if (c.name == "GetValue") true
                      else if (
                        c.name.endsWith(".Evaluation") || c.func.isBuiltin
                      ) { evalInfoStack.head.algoSet += func.id; true }
                      else false
                    case (true, _) => true
                  }
              }

            case cont @ Cont(func, captured, callStack) => {
              // record algo id of continuation
              for { ast <- evalAstStack.headOption if ast.idOpt.isDefined }
                evalInfoStack.head.algoSet += func.id

              // wrap completion for return to resumed context
              val vs = args.map(interp).map(_.wrapCompletion)
              val newLocals =
                getLocals(func.irFunc.params, vs, cont = true) ++ captured
              st.callStack = callStack.map(_.copied)
              st.context = Context(func, newLocals)

              // change algoSetStack
              evalInfoStack = cont.evalInfoStack
            }
            case v => throw NoFunc(fexpr, v)
          }
        } catch {
          case st.SyntacticCalled(ast, sdo) =>
            // same with original interp
            val vs = args.map(interp) match
              case h :: tail => AstValue(ast) :: tail // fix this param
              case _         => error("invalid SDO call")
            st.callStack ::= CallContext(lhs, st.context)
            st.context = Context(sdo, getLocals(sdo.irFunc.params, vs))

            // add new info context
            if (sdo.name.endsWith(".Evaluation") && ast.idOpt.isDefined) {
              val assignMap: MMap[Int, Option[Int]] = ast match
                case _: JsLexical => MMap()
                case syn: JsSyntactic =>
                  MMap.from(for {
                    child <- syn.children.flatten.map(_.trim(cfg))
                    childId = child.idOpt.get
                  } yield childId -> None)
              evalInfoStack ::= AstEvalInfo(
                ast.idOpt.get,
                MSet(sdo.id),
                assignMap,
              )
            }
          case st.LexicalCalled(v) => setCallResult(lhs, v)
        }

        // if current context is evaluation, save type of value
        override def setReturn(value: Value): Unit = {
          super.setReturn(value)

          if (st.context.name endsWith ".Evaluation") {
            for {
              ast <- st.context.astOpt
              astId <- ast.idOpt
              evalTypeOpt = value.toAnnotation(st)
            } {
              val evalInfo = popEvalInfoStack match
                case i: AstEvalInfo => i
                case _              => error("need ast evaluation info")
              val astInfoList = astInfoMap.getOrElseUpdate(astId, ListBuffer())
              val astInfo = evalInfo.toAstInfo(evalTypeOpt)

              // check current evaluation context
              val evalIdx = astInfoList.indexWhere(_.evalTypeOpt == evalTypeOpt)
              val isFirstEval = astInfoList.isEmpty
              val isNewEval =
                evalIdx == -1 ||
                astInfoList.forall(_.children != astInfo.children)

              // save current evaluation type to parent evaluation
              evalInfoStack match {
                case (parentEvalInfo: AstEvalInfo) :: _ =>
                  if (parentEvalInfo.assignMap contains astId) {
                    parentEvalInfo.assignMap.update(
                      astId,
                      if (isFirstEval || isNewEval) Some(astInfoList.size)
                      else Some(evalIdx),
                    )
                  }
                case _ => /* do nothing */
              }

              // if fresh evaluation context, save info
              if (isFirstEval || isNewEval) astInfoList += astInfo
            }
          } else if (st.context.func.isBuiltin) {
            for { ast <- evalAstStack.headOption if ast.idOpt.isDefined } {
              // pop eval info context
              val evalInfo = popEvalInfoStack match
                case i: BuiltinEvalInfo => i
                case _                  => error("need builtin evaluation info")

              // save algoSet
              val algoSet =
                builtinMap.getOrElseUpdate(st.context.func.id, MSet())
              algoSet ++= evalInfo.algoSet
            }
          }
        }
      }.fixpoint

      // check exit and return result
      _st(GLOBAL_RESULT) match
        case comp: Comp if comp.ty == CONST_NORMAL =>
          // simplify result
          new Simplifier(builtinMap, astInfoMap, astList).result
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

      val info = getInfo(testBody, includes)
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
