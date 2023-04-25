package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.es.util.injector.Injector
import esmeta.es.Script
import esmeta.es.util.fuzzer.{FSTrie, SelectionEval}
import esmeta.es.util.withCFG
import esmeta.js.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.util.BaseUtils.*
import esmeta.es.util.JsonProtocol

import scala.io.Source

object BugTrie extends Phase[CFG, Unit] {
  val name = "bug-sens"
  val help =
    "Make and save a FSTrie that contains all feature stacks of the online bugs"

  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit = withCFG(cfg) {
    val baseDir =
      cmdConfig.targets.headOption.getOrElse(s"$RESOURCE_DIR/bugs/online")
    val babelPath = s"$baseDir/babel.js"
    val d8Path = s"$baseDir/d8.js"
    val jsPath = s"$baseDir/js.js"
    val newPath = s"$baseDir/new.js"
    val obfuscPath = s"$baseDir/obfuscator.js"
    val smPath = s"$baseDir/sm.js"
    val swcPath = s"$baseDir/swc.js"
    val terserPath = s"$baseDir/terser.js"
    val bugPaths = List(
      babelPath,
      d8Path,
      jsPath,
      newPath,
      obfuscPath,
      smPath,
      swcPath,
      terserPath,
    )

    val bugs = bugPaths.flatMap(path =>
      val file = Source.fromFile(path)
      val b = file.getLines().toList
      file.close()
      b,
    )
    val trie = FSTrie.fromBugs(bugs)
    val jsonProtocol = JsonProtocol(cfg)
    import jsonProtocol.given
    dumpJson(
      name = Some("Dumping bug trie"),
      data = trie,
      filename = s"$baseDir/bugtrie.json",
      space = true,
    )
  }

  def defaultConfig: Config = Config()

  val options: List[PhaseOption[Config]] = List.empty

  case class Config()

}
