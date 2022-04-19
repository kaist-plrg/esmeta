package esmeta.editor.util

import esmeta.*
import esmeta.cfg.CFG
import esmeta.editor.sview.*
import esmeta.editor.models.*
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
    // database
    val tests = readFile(s"$dataDir/test262-list").split(LINE_SEP)
    // var testSet: Set[String] = Set()
    // var algoSet: Set[Int] = Set()

    val conn = SqliteConnection(s"$dataDir/data.db")

    // TODO get concrete parts
    def aux(sview: SyntacticView): Set[Int] = {
      var result: Set[Int] = Set()
      sview match
        case AbsSyntactic(absName, anno, _) => ???
        case Lexical(name, str)             => ???
        case Syntactic(name, _, rhsIdx, children) =>
          conn.selectSynRootStmt.setString(1, name)
          conn.selectSynRootStmt.setInt(2, rhsIdx)
          val resultSet = conn.selectSynRootStmt.executeQuery()
          while (resultSet.next()) {
            val astId = resultSet.getInt(1)
            getChildren(astId, children) match
              case None           =>
              case Some(childSet) => result ++= (childSet + astId)

          }
      result
    }

    def getChildren(
      parentId: Int,
      children: List[Option[SyntacticView]],
    ): Option[Set[Int]] = {
      conn.selectChildrenStmt.setInt(1, parentId)
      val resultSet = conn.selectChildrenStmt.executeQuery()

      var success = true
      var successSet: Set[Int] = Set()
      while (resultSet.next() && success) {
        val astId = resultSet.getInt(1)
        val name = resultSet.getString(2)
        children(resultSet.getInt(3)).get match
          case AbsSyntactic(absName, _, _) =>
            if (name != absName) success = false
          case Lexical(lexName, str) =>
            if (name == lexName && str.trim == resultSet.getString(4))
              successSet += astId
            else success = false
          case Syntactic(synName, _, rhsIdx, synChildren) =>
            if (name == synName && rhsIdx == resultSet.getInt(5))
              getChildren(astId, synChildren) match
                case None     => success = false
                case Some(cs) => successSet ++= (cs + astId)
            else success = false
      }

      if (success) Some(successSet) else None
    }

    def getTestId(astSet: Set[Int]): Set[Int] = {
      for { astId <- astSet } conn.storeTempAstId(astId)
      conn.storeTempAstIdCommit()

      var testSet: Set[Int] = Set()
      val resultSet = conn.executeQuery(
        """SELECT
        |  Ast.test_id
        |FROM Ast
        |INNER JOIN Temp_Ast ON Ast.id = Temp_Ast.id
        |INNER JOIN Ast_Algorithm ON Ast.id = Ast_Algorithm.ast_id
        |GROUP BY Ast.test_id""".stripMargin,
      )
      while (resultSet.next()) testSet += resultSet.getInt(1)
      testSet
    }

    val (t0, concretes) = time(aux(sview))
    println(concretes)
    println(t0)
    val (t1, testSet) = time(getTestId(concretes))
    println(testSet)
    println(t1)

    // conn.executeQuery(
    //   "SELECT * FROM Ast WHERE name = ?"
    // )

    ???

    // file-based
    // val tests = readFile(s"$dataDir/test262-list").split(LINE_SEP)
    // var testSet: Set[String] = Set()
    // var algoSet: Set[Int] = Set()
    // val progress = ProgressBar("filter test262", 0 until 100)

    // var t0 = 0L
    // var t1 = 0L

    // for (idx <- progress) {
    //   val (ltime, (annoMap, algoMap, astList)) =
    //     time(
    //       readJson[(Map[Int, Set[Annotation]], Map[Int, Set[Int]], List[Ast])](
    //         s"$dataDir/data/$idx.json",
    //       ),
    //     )

    //   val (ctime, _) = time(
    //     for {
    //       ast <- astList
    //       conc <- ast.getConcreteParts(sview, annoMap)
    //       astId <- conc.idOpt
    //       aids <- algoMap.get(astId)
    //     } { algoSet ++= aids; testSet += tests(idx) },
    //   )

    //   t0 += ltime
    //   t1 += ctime
    // }

    // println(t0)
    // println(t1)

    // (testSet.toList.sorted, algoSet)
  }
}
