package esmeta.phase

import esmeta.{CommandConfig, LINE_SEP}
import esmeta.cfg.CFG
import esmeta.js.Target
import esmeta.util.SystemUtils.{readJson, readFile}
import scala.collection.mutable.{Map as MutMap, ArrayBuffer as MutVec}
import esmeta.es.util.deltadebugger.ESReducer
import scala.util.control.Breaks.{breakable, break}

/** `aggregate` phase */
case object Aggregate
  extends Phase[CFG, Map[Target, Map[String, Set[String]]]] {
  val name = "aggregate"
  val help = "Aggregate the failing comform test cases"

  type TargetName = String
  type TestName = String
  type Script = String

  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Map[Target, Map[String, Set[String]]] =
    val testsDirName = cmdConfig.targets.applyOrElse(
      0,
      Int =>
        throw Error(
          "The path of the directory containing scripts should be given.",
        ),
    )
    val targetNameToFails: Map[TargetName, Set[TestName]] = readJson(
      cmdConfig.targets.applyOrElse(
        1,
        Int =>
          throw Error(
            "The path of the JSON file containing failure information of conform test should be given.",
          ),
      ),
    )
    val targetToFails: Map[Target, Set[TestName]] =
      targetNameToFails.map((targetName, failedTestNames) =>
        val target = Target(
          targetName,
          targetName match
            case s if List("d8", "js", "jsc", "sm") contains s => false
            case s if List("babel", "swc", "terser", "obfuscator") contains s =>
              true
            case _ =>
              throw new Error(
                "Name of an ECMAScript engine or a transpiler is invalid.",
              ),
        )
        target -> failedTestNames,
      )

    val aggregated: Map[Target, Map[String, Set[String]]] =
      targetToFails.map((target, failedTestNames) =>
        val forest = AggregationForest()
        val minimized: MutVec[AggregationNode] = MutVec()
        for (testName <- failedTestNames) {
          val test: Script =
            readFile(s"$testsDirName/$testName").split(LINE_SEP)(1).trim
          forest.tryInsertTest(testName, test) match
            case Some(node) =>
              var curr = node
              val dd = ESReducer(test, target)
              breakable {
                while (true) {
                  dd.tryReduce() match
                    case Some(reduced) =>
                      forest.tryMakeParentOrGraft(curr, reduced) match
                        case Some(parent) => curr = parent
                        case None         => break
                    case None =>
                      minimized += curr
                      break
                }
              }
            case None => ()
        }
        throw NotImplementedError(),
      )

    // TODO: dump result
    throw NotImplementedError()

    aggregated

  private class AggregationNode(val script: Script):
    var parent: Option[AggregationNode] = None
    var leaves: MutVec[LeaveGroup] = null

    def this(script: Script, name: TestName) =
      this(script)
      leaves = MutVec(DirectLeaf(name))

    override def toString(): String =
      val nodeInfo = parent match
        case Some(parent) => s"$script (-> ${parent.script})"
        case None         => s"$script"
      nodeInfo ++ s"\n${leaves}"

  private class LeaveGroup
  private case class DirectLeaf(name: TestName) extends LeaveGroup
  private case class GraftedLeaves(set: MutVec[LeaveGroup]) extends LeaveGroup

  private class AggregationForest:

    val nodeMap: MutMap[Script, AggregationNode] = MutMap()

    def nodeMapToString: String =
      nodeMap.values
        .map(_.toString())
        .mkString("\n\n") ++ "\n=================\n"

    def tryInsertTest(name: TestName, test: Script): Option[AggregationNode] =
      nodeMap.get(test) match
        case None =>
          val node = AggregationNode(test, name)
          nodeMap += test -> node
          Some(node)
        case Some(node) =>
          node.leaves += DirectLeaf(name)
          None

    def tryMakeParentOrGraft(
      node: AggregationNode,
      reduced: Script,
    ): Option[AggregationNode] =
      nodeMap.get(reduced) match
        case Some(parent) =>
          node.parent = Some(parent)
          parent.leaves += GraftedLeaves(node.leaves)
          None
        case None =>
          val parent = AggregationNode(reduced)
          node.parent = Some(parent)
          parent.leaves = node.leaves
          nodeMap += reduced -> parent
          Some(parent)

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List()
  case class Config()
}
