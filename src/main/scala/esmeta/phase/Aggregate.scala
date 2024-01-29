package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.js.Target
import esmeta.util.SystemUtils.*

/** `aggregate` phase */
case object Aggregate
  extends Phase[CFG, Map[Target, Map[String, Set[String]]]] {
  val name = "aggregate"
  val help = "Aggregate the failing comform test cases"

  type TargetName = String
  type Test = String

  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Map[Target, Map[String, Set[String]]] =
    val scriptsDir = cmdConfig.targets.applyOrElse(
      0,
      Int =>
        throw Error(
          "The path of the directory containing scripts should be given.",
        ),
    )
    val failsMapFileName = cmdConfig.targets.applyOrElse(
      1,
      Int =>
        throw Error(
          "The path of the JSON file containing failure information of conform test should be given.",
        ),
    )
    val failsMap: Map[TargetName, Set[Test]] = readJson(cmdConfig.targets(1))

    val init: Map[Target, Map[String, Set[String]]] = Map()
    val aggregated = failsMap.foldLeft(init) {
      case (cur, (targetName, fails)) =>
        val target = Target(
          targetName,
          targetName match {
            case s if List("d8", "js", "jsc", "sm") contains s => false
            case s if List("babel", "swc", "terser", "obfuscator") contains s =>
              true
            case _ =>
              throw new Error(
                "Name of an ECMAScript engine or a transpiler is invalid.",
              )
          },
        )
        cur + (target -> fails.foldLeft(Map[String, Set[String]]()) {
          case (count, testName) =>
            // TODO
            val minimized: String = throw new NotImplementedError()
            count + (minimized -> (count.getOrElse(
              minimized,
              Set(),
            ) + testName))
        })
    }

    // TODO: dump result
    throw new NotImplementedError()

    aggregated

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
