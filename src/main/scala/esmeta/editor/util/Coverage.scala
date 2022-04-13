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

  // get touched nodes
  def touchedNodes: Set[Int] = {
    // progress bar
    val progress =
      ProgressBar("measure test262 coverage", 0 until tests.size)

    // total touched node id
    var totalTouched: Set[Int] = Set()

    // run test262 and get touched nodes for each test
    for (idx <- progress) {
      val NormalConfig(name, includes) = tests(idx)
      val touched =
        try {
          val (_, ast) = test262.loadTestFromFile(s"$TEST262_TEST_DIR/$name")
          loadDirOpt match
            case None          => ast.touchedNodes(cfg)
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
    val NormalConfig(name, includes) = tests(0)
    val testBody = jsParser.fromFile(s"$TEST262_TEST_DIR/$name")
    // val coverMap = testBody.coverMap(cfg)
    // println(testBody.coverMap(cfg))
    // println(testBody)
    // println(coverMap.size)
    // val nextAstId = testBody.setId(0) // mark test body code
    // val (_, ast) = test262.loadTest(testBody, includes)
    // println(ast)
    // println(ast.touchedAlgos(cfg, nextAstId))
    ???

    // // progress bar
    // val progress =
    //   ProgressBar("measure algorithms per ast", 0 until tests.size)

    // for (idx <- progress) {
    //   val NormalConfig(name, includes) = tests(idx)
    //   val testBody = jsParser.fromFile(s"$TEST262_TEST_DIR/$name")
    //   testBody.setId(0)
    //   val ast = test262.loadTest(ast, includes)
    //   ast.touchedAlgos(cfg)
    //   ???
    // }
  }
}
