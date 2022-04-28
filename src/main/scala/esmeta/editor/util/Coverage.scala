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
case class EvalInfo(
  id: Int,
  isBuiltin: Boolean = false,
  algoSet: MSet[Int] = MSet(),
  childEvalTypes: MMap[Int, Annotation] = MMap(),
) {
  def copied: EvalInfo =
    EvalInfo(id, isBuiltin, MSet.from(algoSet), MMap.from(childEvalTypes))

  // bit encoding of children eval type
  def typeBit: Int = {
    val types = childEvalTypes.toList.sortBy(_._1).map(_._2)
    if (types.length > 3) {
      throw error(s"!!! type bit need to be larger ${types.length}")
    }
    types.zipWithIndex.foldLeft(0) {
      case (acc, (t, i)) =>
        def aux(n: Int): Int = acc + (1 << (n + i * 9))
        t match
          case AObj    => aux(0)
          case ASymbol => aux(1)
          case ANum    => aux(2)
          case ABigInt => aux(3)
          case AStr    => aux(4)
          case ABool   => aux(5)
          case AUndef  => aux(6)
          case ANull   => aux(7)
          case AThrow  => aux(8)
          case AAll    => acc
    }
  }
}

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
    annoMap: MMap[Int, MSet[Annotation]],
    astMap: MMap[Int, MMap[Int, MSet[Int]]],
    astList: ListBuffer[Ast],
    builtinCallAstSet: MSet[Int],
    builtinMap: MMap[Int, MSet[Int]],
  ) {
    lazy val result = {
      val simplified = astList.map(simplifyAst(_))
      ProgramInfo(
        Map.from(for {
          (origId, annoSet) <- annoMap
        } yield idMap(origId) -> Set.from(annoSet)),
        Map.from(
          for { (origId, algoSetByType) <- astMap } yield idMap(origId) -> Map
            .from(for {
              (typeBit, algoSet) <- algoSetByType
            } yield typeBit -> Set.from(algoSet)),
        ),
        simplified.toList,
        Set.from(for { origId <- builtinCallAstSet } yield idMap(origId)),
        Map.from(for {
          (bid, algoSet) <- builtinMap
        } yield bid -> Set.from(algoSet)),
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
    import JsonProtocol.given

    // fix interp for collection evaluation info
    def getInfo(testBody: Ast, includes: List[String]): ProgramInfo = {
      var nextAstId = testBody.setId(0)
      val (sourceText, ast) = test262.loadTest(testBody, includes)
      val _st = initState(sourceText, ast)

      // run interp
      val builtinCallAstSet: MSet[Int] = MSet()
      val builtinMap: MMap[Int, MSet[Int]] = MMap()

      // bit encoding of child result type
      val astMap: MMap[Int, MMap[Int, MSet[Int]]] = MMap()
      val annoMap: MMap[Int, MSet[Annotation]] = MMap()

      val astList: ListBuffer[Ast] = ListBuffer(testBody)
      new Interp(_st, Nil) {
        private def contexts = st.context :: st.callStack.map(_.context)
        private def astStack = contexts.flatMap(_.astOpt)
        private def evalAstStack =
          contexts.filter(_.name endsWith ".Evaluation").flatMap(_.astOpt)

        private var evalInfoStack: List[EvalInfo] = List()

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
              for {
                ast <- evalAstStack.headOption
                astId <- ast.idOpt
              } {
                if (func.isBuiltin)
                  // add new info context
                  evalInfoStack ::= EvalInfo(func.id, true, MSet(func.id))
                  builtinCallAstSet += astId
                else if (func.name == "GetValue")
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
              for {
                ast <- evalAstStack.headOption if ast.idOpt.isDefined
              } evalInfoStack.head.algoSet += func.id

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
              evalInfoStack ::= EvalInfo(
                ast.idOpt.get,
                false,
                MSet(sdo.id),
                MMap.from((for {
                  child <- ast match
                    case syn: JsSyntactic => syn.children.flatten
                    case _: JsLexical     => List()
                  childId = child.idOpt.get
                } yield childId -> AAll)),
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
              annotationOpt = value.toAnnotation(st)
            } {

              // pop eval info context
              val currentInfo = evalInfoStack.head
              evalInfoStack = evalInfoStack.tail

              // save algoSet
              val algoSetByType = astMap.getOrElseUpdate(astId, MMap())
              val algoSet =
                algoSetByType.getOrElseUpdate(currentInfo.typeBit, MSet())
              algoSet ++= currentInfo.algoSet

              // save annotation
              for { annotation <- annotationOpt } {
                val annoSet = annoMap.getOrElseUpdate(astId, MSet())
                annoSet += annotation

                // change eval type of current evaluation in parent info context
                evalInfoStack match
                  case parentInfo :: _ =>
                    st.callStack.map(_.context).foldLeft(false) {
                      case (false, c) =>
                        if (c.func.name endsWith ".Evaluation") {
                          var stop = false
                          var current = ast
                          while (
                            !stop && current.parent.isDefined && current.idOpt.isDefined
                          ) {
                            val parent = current.parent.get
                            val currentId = current.idOpt.get
                            for { pid <- parent.idOpt } {
                              if (pid == parentInfo.id) {
                                parentInfo.childEvalTypes += (currentId -> annotation)
                                stop = true
                              }
                            }
                            current = parent
                          }
                          true
                        } else if (c.func.isSDO) true
                        else false
                      case (true, _) => true
                    }
                  case _ => /* do nothing */
              }

            }
          } else if (st.context.func.isBuiltin) {
            for { ast <- evalAstStack.headOption if ast.idOpt.isDefined } {
              // pop eval info context
              val currentInfo = evalInfoStack.head
              evalInfoStack = evalInfoStack.tail

              // save algoSet
              val algoSet =
                builtinMap.getOrElseUpdate(st.context.func.id, MSet())
              algoSet ++= currentInfo.algoSet
            }
          }
        }
      }.fixpoint

      // check exit and return result
      _st(GLOBAL_RESULT) match
        case comp: Comp if comp.ty == CONST_NORMAL =>
          // simplify result
          new Simplifier(
            annoMap,
            astMap,
            astList,
            builtinCallAstSet,
            builtinMap,
          ).result
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

      // TODO
      // val (annoMap, astMap, astList, builtinCallAstSet, builtinMap) =
      //   getInfo(testBody, includes)

    }

    // dump index result
    for { dumpDir <- dumpDirOpt } dumpJson(
      pIndex,
      s"$dumpDir/data/index.json",
      noSpace = true,
    )
  }
}
