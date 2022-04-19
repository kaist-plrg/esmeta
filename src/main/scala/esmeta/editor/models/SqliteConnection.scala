package esmeta.editor.models

import esmeta.util.BaseUtils.*
import esmeta.js.*
import java.sql.DriverManager
import java.sql.Connection

case class SqliteConnection(path: String) {

  Class.forName("org.sqlite.JDBC") // load JDBC driver
  private val connection = DriverManager.getConnection(s"jdbc:sqlite:$path")
  connection.setAutoCommit(false)

  private def init(): Unit = {
    // create tables

    // ast table
    // TODO encoding nonterminal name to integer?
    executeAndCommit("""CREATE TABLE IF NOT EXISTS Ast (
    |  id INTEGER,
    |  name VARCHAR(64),
    |  test_id INTEGER,
    |  parent_id INTEGER,
    |  child_idx INTEGER,
    |  kind INTEGER,
    |  rhs_idx INTEGER,
    |  str TEXT,
    |  PRIMARY KEY (id)
    |)""".stripMargin)

    // test table
    executeAndCommit("""CREATE TABLE IF NOT EXISTS Test262 (
    |  id INTEGER,
    |  name VARCHAR(256),
    |  PRIMARY KEY (id)
    |)""".stripMargin)

    // algo table
    executeAndCommit("""CREATE TABLE IF NOT EXISTS Algorithm (
    |  id INTEGER,
    |  name VARCHAR(256),
    |  PRIMARY KEY (id)
    |)""".stripMargin)

    // ast-algo table
    executeAndCommit("""CREATE TABLE IF NOT EXISTS Ast_Algorithm (
    |  ast_id INTEGER,
    |  algo_id INTEGER
    |)""".stripMargin)

    // ast-annotation table
    executeAndCommit("""CREATE TABLE IF NOT EXISTS Ast_Annotation (
    |  ast_id INTEGER,
    |  anno_id INTEGER
    |)""".stripMargin)

    // temporty table for get test id
    executeAndCommit("""CREATE TABLE IF NOT EXISTS Temp_Ast (
    |  id INTEGER
    |)""".stripMargin)
    executeAndCommit("DELETE FROM Temp_Ast")
  }

  def executeAndCommit(query: String) = {
    println(query)
    connection.createStatement.execute(query)
    connection.commit()
  }

  // WITH RECURSIVE x (???) AS (
  //   SELECT id FROM Ast WHERE name = ? and rhs_idx = ?
  //   UNION ALL
  //   SELECT kk
  // )

  // abs syntactic
  lazy val selectAbsSynRootStmt = connection.prepareStatement(
    "SELECT * FROM Ast WHERE name = ?",
  )

  // syntactic
  lazy val selectSynRootStmt = connection.prepareStatement(
    "SELECT id FROM Ast WHERE name=? AND rhs_idx=?",
  )
  lazy val selectChildrenStmt = connection.prepareStatement(
    "SELECT id, name, child_idx, str, rhs_idx FROM Ast WHERE parent_id=? ORDER BY child_idx ASC",
  )

  // lexical
  lazy val selectLexRootStmt = connection.prepareStatement(
    "SELECT * FROM Ast WHERE name = ? AND str = ?",
  )

  def executeQuery(query: String): java.sql.ResultSet =
    connection.createStatement.executeQuery(query)

  private var insertAstCnt = 0
  private lazy val astInsertStmt = connection.prepareStatement(
    "INSERT INTO Ast VALUES (?, ?, ?, ?, ?, ?, ?, ?)",
  )
  def storeAst(
    ast: Ast,
    testId: Int,
    parentInfo: Option[(Int, Int)] = None,
  ): Unit = {
    astInsertStmt.setInt(1, ast.idOpt.get)
    astInsertStmt.setString(2, ast.name)
    astInsertStmt.setInt(3, testId)
    parentInfo match
      case None =>
        astInsertStmt.setNull(4, java.sql.Types.INTEGER)
        astInsertStmt.setNull(5, java.sql.Types.INTEGER)
      case Some((pid, childIdx)) =>
        astInsertStmt.setInt(4, pid)
        astInsertStmt.setInt(5, childIdx)
    val children =
      ast match
        case Syntactic(name, _, rhsIdx, children) =>
          astInsertStmt.setInt(6, 0)
          astInsertStmt.setInt(7, rhsIdx)
          astInsertStmt.setNull(8, java.sql.Types.VARCHAR)
          children
        case Lexical(name, str) =>
          astInsertStmt.setInt(6, 1)
          astInsertStmt.setInt(7, 0)
          astInsertStmt.setString(8, str.trim)
          List()
    astInsertStmt.addBatch()
    insertAstCnt += 1
    if (insertAstCnt % 100 == 0) {
      astInsertStmt.executeBatch()
      insertAstCnt = 0
    }

    for {
      (childOpt, childIdx) <- children.zipWithIndex
      child <- childOpt
    } storeAst(child, testId, Some((ast.idOpt.get, childIdx)))
  }

  private lazy val astAlgoInsertStmt = connection.prepareStatement(
    "INSERT INTO Ast_Algorithm VALUES (?, ?)",
  )
  private var astAlgoInsertCnt = 0
  def storeAstAlgo(
    astId: Int,
    algoId: Int,
  ): Unit = {
    astAlgoInsertStmt.setInt(1, astId)
    astAlgoInsertStmt.setInt(2, algoId)
    astAlgoInsertStmt.addBatch()
    astAlgoInsertCnt += 1
    if (astAlgoInsertCnt % 100 == 0) {
      astAlgoInsertStmt.executeBatch()
      astAlgoInsertCnt = 0
    }
  }

  private lazy val astAnnoInsertStmt = connection.prepareStatement(
    "INSERT INTO Ast_Annotation VALUES (?, ?)",
  )
  private var astAnnoInsertCnt = 0
  def storeAstAnno(
    astId: Int,
    annoId: Int,
  ): Unit = {
    astAnnoInsertStmt.setInt(1, astId)
    astAnnoInsertStmt.setInt(2, annoId)
    astAnnoInsertStmt.addBatch()
    astAnnoInsertCnt += 1
    if (astAnnoInsertCnt % 100 == 0) {
      astAnnoInsertStmt.executeBatch()
      astAnnoInsertCnt = 0
    }
  }

  private var tempAstIdInsertCnt = 0
  private lazy val tempAstIdInsertStmt = connection.prepareStatement(
    "INSERT INTO Temp_Ast VALUES (?)",
  )
  def storeTempAstId(astId: Int): Unit = {
    tempAstIdInsertStmt.setInt(1, astId)
    tempAstIdInsertStmt.addBatch()
    tempAstIdInsertCnt += 1
    if (tempAstIdInsertCnt % 100 == 0) {
      tempAstIdInsertStmt.executeBatch()
      tempAstIdInsertCnt = 0
    }
  }
  def storeTempAstIdCommit(): Unit =
    if (tempAstIdInsertCnt > 0) tempAstIdInsertStmt.executeBatch()

  // wrapper for JDBC
  def commit(): Unit = connection.commit()
  def close(): Unit = {
    if (insertAstCnt > 0) astInsertStmt.executeBatch()
    if (astAlgoInsertCnt > 0) astAlgoInsertStmt.executeBatch()
    if (astAnnoInsertCnt > 0) astAnnoInsertStmt.executeBatch()

    commit()
    connection.close()
  }

  // def apply() = {

  //   val names = Array(
  //     "apple",
  //     "banana",
  //     "canada",
  //     "double",
  //     "elephant",
  //     "fruit",
  //     "grape",
  //     "hospital",
  //     "italic",
  //     "jordan",
  //   )

  //   // val (t0, _) = time {
  //   //   val insertStmt = connection.prepareStatement(
  //   //     s"INSERT INTO test_table (id, name) VALUES (?, ?)",
  //   //   )
  //   //   for { i <- 1 to 10000000 } {
  //   //     insertStmt.setInt(1, i)
  //   //     insertStmt.setString(2, names(i % 10))
  //   //     insertStmt.addBatch()

  //   //     if (i % 100 == 0) insertStmt.executeBatch()
  //   //   }
  //   //   connection.commit()
  //   // }

  //   // val (t1, _) = time {
  //   //   connection
  //   //     .createStatement()
  //   //     .execute("CREATE INDEX name_idx ON test_table (name)")
  //   //   connection.commit()
  //   // }

  //   // println(t0)
  //   // println(t1)

  //   // connection.close()

  //   try {
  //     val selectStmt = connection.prepareStatement(
  //       "SELECT * FROM test_table where name = ?",
  //     )

  //     for (i <- 0 until 10) {
  //       selectStmt.setString(1, names(i % 10))
  //       val (t2, _) = time {
  //         val resultSet = selectStmt.executeQuery()
  //         // while (resultSet.next()) {
  //         //   println((names(i % 10), resultSet.getInt(1)))
  //         // }
  //       }
  //       println(t2)
  //     }
  //   } catch {
  //     case e => e.printStackTrace
  //   }

  // }

  init()
}
