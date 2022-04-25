package esmeta.cfg

import esmeta.*
import esmeta.cfg.util.*
import esmeta.ir.Program
import esmeta.spec.{Spec, TypeModel, Grammar, Nonterminal}
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import scala.collection.mutable.ListBuffer

/** control-flow graphs (CFGs) */
case class CFG(
  funcs: List[Func] = Nil,
) extends CFGElem {

  /** backward edge to a program */
  var program: ir.Program = ir.Program()

  /** the main function */
  lazy val main: Func = getUnique(funcs, _.irFunc.main, "main function")

  /** JavaScript parser */
  lazy val jsParser: js.util.Parser = program.jsParser

  /** mapping from fid to functions */
  lazy val funcMap: Map[Int, Func] =
    (for (func <- funcs) yield func.id -> func).toMap

  /** mapping from function names to functions */
  lazy val fnameMap: Map[String, Func] =
    (for (func <- funcs) yield func.irFunc.name -> func).toMap

  /** mapping from nid to nodes */
  lazy val nodeMap: Map[Int, Node] = (for {
    func <- funcs
    node <- func.nodes
  } yield node.id -> node).toMap

  /** mapping from nodes to functions */
  lazy val funcOf: Map[Node, Func] = (for {
    func <- funcs
    node <- func.nodes
  } yield node -> func).toMap

  /** get a type model */
  def typeModel: TypeModel = spec.typeModel

  /** get the corresponding specification */
  def spec: Spec = program.spec

  /** get the corresponding grammar */
  def grammar: Grammar = spec.grammar

  /** production name helper */
  lazy val names: Array[String] = grammar.nameMap.keys.toArray.sorted
  def getNameIdx(name: String): Int =
    names.indexOf(
      if (name == "IdentifierName \\ (ReservedWord)") "IdentifierName"
      else name,
    )

  /** get direct simplified forms of nonterminal */
  private lazy val getDirectSimplified =
    cached[Int, (Set[Int], Set[(Int, Int, Int)])] { nameIdx =>
      var chainProds: Set[Int] = Set()
      var simplified: Set[(Int, Int, Int)] = Set()
      val prodName = names(nameIdx)
      val prod = grammar.nameMap(prodName)

      if (prod.isSyntactic) {
        val chainProdNames = prod.rhsList.zipWithIndex.flatMap {
          case (rhs, rhsIdx) =>
            def hasEval(subIdx: Int): Boolean =
              fnameMap contains s"${prodName}[${rhsIdx},${subIdx}].Evaluation"
            def addSimplified(subIdx: Int): Unit =
              simplified += ((nameIdx, rhsIdx, subIdx))

            val (opts, nonOpts) = rhs.nts.partition(_.optional)
            if (opts.size == 0)
              nonOpts match
                case List(nt) if !hasEval(0) => List(nt.name)
                case _                       => { addSimplified(0); List() }
            else {
              val subs =
                opts.foldLeft(List[List[Nonterminal]](List())) {
                  // nt order not important
                  case (acc, optNt) =>
                    acc.flatMap(li => List(li, optNt :: li))
                }
              subs.zipWithIndex.flatMap {
                case (sub, subIdx) =>
                  if (sub.size + nonOpts.size == 1 && !hasEval(subIdx))
                    Some(
                      if (sub.isEmpty) nonOpts.head.name else sub.head.name,
                    )
                  else { addSimplified(subIdx); None }
              }
            }
        }.toSet
        chainProds = chainProdNames.map(names.indexOf(_))
      } else simplified += ((nameIdx, -1, -1))

      (chainProds, simplified)
    }

  /** get all simplified forms of nonterminal */
  lazy val simplifiedMap: Map[Int, Set[(Int, Int, Int)]] = {
    // get direct chains for each nonterminal
    var chains =
      (for {
        i <- 0 until names.size
        name = names(i)
        direct <- getDirectSimplified(i)._1
      } yield i -> direct).toSet

    // get transitive closure of production chain
    for {
      k <- 0 until names.size
      i <- 0 until names.size
      j <- 0 until names.size
    } {
      if (
        !chains.contains((i, j)) &&
        (chains.contains((i, k)) && chains.contains((k, j)))
      ) chains += ((i, j))
    }

    // get simplified production for each nonterminal
    (for {
      (from, toSet) <- chains.groupMap(_._1)(_._2)
    } yield from -> (toSet + from).flatMap(getDirectSimplified(_)._2)).toMap
  }

  /** dump funcs of cfg */
  def dumpTo(baseDir: String): Unit =
    mkdir(baseDir)
    for {
      func <- funcs
      name = func.name
      filename = s"$baseDir/${name.replace("/", "")}.cfg"
    } dumpFile(func.toString(location = false), filename)
}
