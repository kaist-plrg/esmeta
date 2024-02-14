package esmeta.phase

import esmeta.{CommandConfig, LINE_SEP}
import esmeta.cfg.CFG
import esmeta.js.Target
import esmeta.es.util.USE_STRICT
import esmeta.util.SystemUtils.{readJson, readFile, dumpFile, dumpJson}
import scala.collection.mutable.{Map as MutMap, ArrayBuffer as MutVec}
import esmeta.es.util.deltadebugger.ESReducer
import scala.util.control.Breaks.{breakable, break}
import esmeta.AGGREGATE_LOG_DIR

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

    val aggregated: Map[Target, Map[Script, Set[TestName]]] =
      targetToFails.map((target, failedTestNames) =>
        val forest = AggregationForest()
        val minimized: MutVec[AggregationNode] = MutVec()
        for (testName <- failedTestNames) {
          val test: Script =
            readFile(s"$testsDirName/$testName").split(LINE_SEP)(1).trim
          // val test1 =
          //   "class x extends null { [ [ x , ] = { [ Symbol . iterator ] : async x => 0 } ] ; }"
          println(s"- try reducing    ${test}")
          forest.tryInsertTest(testName, test) match
            case Some(node) =>
              var curr = node
              val dd = ESReducer(test, target, cfg)
              breakable {
                while (true) {
                  dd.tryReduce() match
                    case Some(reduced) =>
                      if curr.script != reduced then
                        forest.tryMakeParentOrGraft(curr, reduced) match
                          case Some(parent) =>
                            println(
                              s"       adopted -> ${reduced}",
                            )
                            curr = parent
                          case None =>
                            println(
                              s"       grafted -> ${reduced}",
                            )
                            println(
                              s"  done.\n",
                            )
                            break
                    case None =>
                      minimized += curr
                      println(s"  minimized. done.\n")
                      break
                }
              }
            case None => ()
        }
        val minimizationMap = minimized
          .map((node) => {
            (
              node.script, {
                val leaves: MutVec[TestName] = MutVec()
                lazy val insertLeaves: MutVec[LeaveGroup] => Unit =
                  (leaveGroups: MutVec[LeaveGroup]) => {
                    for (group <- leaveGroups) {
                      group match {
                        case DirectLeaf(name)   => leaves.addOne(name)
                        case GraftedLeaves(set) => insertLeaves(set)
                      }
                    }
                  }
                insertLeaves(node.leaves)
                leaves.toSet
              },
            )
          })
          .toMap
        (target, minimizationMap),
      )

    println("======================\nAggregation Result")
    println("----------------------")
    aggregated.foreach((target, minimizationMap) => {
      println(s"For ${target}:")
      minimizationMap.foreach((script, nameSet) => {
        println(s"  - Script:    ${script}")
        println(s"      is minimized from:")
        nameSet.foreach((name) => {
          println(s"      + ${name}")
        })
      })
    })
    println("======================")

    val reducedDirPath = s"$AGGREGATE_LOG_DIR/reduced"
    val forJson = aggregated
      .map((target, minimizationMap) => {
        val dirPath = s"$reducedDirPath/$target"
        (
          target.toString,
          minimizationMap
            .zip(0 until minimizationMap.size)
            .map {
              case ((script, nameSet), newName) =>
                val strict = getStrictScript(script)
                dumpFile(strict, s"$dirPath/$newName.js")
                (s"$newName.js", nameSet)
            }
            .toMap,
        )
      })
      .toMap
    dumpJson(
      forJson,
      s"$AGGREGATE_LOG_DIR/aggregation.json",
    )

    aggregated

  private def getStrictScript(candidate: Script): Script =
    USE_STRICT + candidate + LINE_SEP

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
