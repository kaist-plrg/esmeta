package esmeta.js

import esmeta.{LOG_DIR, LINE_SEP}
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.error.{TimeoutException, NoCommandError}
import esmeta.es.*
import esmeta.es.util.cfg
import esmeta.es.util.{USE_STRICT}
import esmeta.es.util.injector.*
import esmeta.es.util.ValidityChecker
import esmeta.es.util.mutator.Util.AdditiveListWalker
import scala.collection.mutable.{Map => MMap}
import scala.runtime.AbstractFunction4

case class Target(
  val name: String,
  val isTrans: Boolean,
  val _cmd: String = "",
) {
  lazy val cmd = optional {
    if (!isTrans)
      JSEngine.defaultCmd(name)
    else
      JSTrans.defaultCmd(name)
  }.getOrElse(_cmd)

  override def toString = name

  lazy val repl = JSTrans.defaultRepl.get(name).map(REPL(_))
  def cleanup = repl.foreach(_.close)

  type CResult = (ExitTag, ExitTag, String)
  def isPass(result: CResult): Boolean =
    val (expected, actual, stdout) = result
    expected.equivalent(actual) && stdout.isEmpty

  def doConformTest(code: String): Boolean = isPass(runConformTest(code))
  def doConformTest(test: ConformTest): Boolean = isPass(runConformTest(test))

  def runConformTest(code: String): CResult = runConformTest(Injector(code))
  def runConformTest(test: ConformTest): CResult = {

    val assertion = test.core
    val code = test.script
    val exitTag = test.exitTag
    val isNormal = exitTag == NormalTag

    def testMaker(code: String) = {
      if (isNormal)
        List(
          USE_STRICT,
          code,
          libHead,
          Injector.assertionLib,
          delayHead,
          assertion,
          delayTail,
          libTail,
        ).mkString(LINE_SEP)
      else
        List(
          USE_STRICT,
          code,
          assertion,
        ).mkString(LINE_SEP)
    }

    val (concreteExitTag, stdout) =
      if (!isTrans) {
        // engine
        val etest = testMaker(code)
        JSEngine
          .runUsingBinary(cmd, etest)
          .map((NormalTag, _))
          .recover(engineErrorResolver _)
          .get
      } else {
        val transpiledCode = JSTrans.transpileUsingRepl(repl.get, code)
        val ttest = testMaker(transpiledCode)
        JSEngine
          .run(ttest)
          .map((NormalTag, _))
          .recover(engineErrorResolver _)
          .get
      }

    (exitTag, concreteExitTag, stdout)
  }

  // header and footer for tests
  private val libHead = "(()=>{"
  private val libTail = "})();"
  private val delayHead = "$delay(() => {"
  private val delayTail = "});"
  private val errorPattern = "[\\w]*Error(?=: )".r
  private def engineErrorResolver(engineError: Throwable): (ExitTag, String) =
    engineError match {
      case e: TimeoutException     => (TimeoutTag, "")
      case e @ NoCommandError(cmd) => throw e
      case e =>
        val msg = e.getMessage
        val tag = errorPattern.findFirstIn(msg) match {
          case Some(name) =>
            ThrowErrorTag(name, msg.split(LINE_SEP).toList.headOption)
          case _ if msg.contains("TRANSPILE_FAILURE") => TranspileFailTag
          case _ => ThrowValueTag(esmeta.state.Str(msg))
        }
        (tag, "")
    }

  def minimize(code: String): String =
    val origResult = runConformTest(code)

    var ast = Ast(code)
    def tryReduce: Boolean =
      val result = reduce(ast).find(isSameFail(origResult))
      result match {
        case Some(newAst) => ast = newAst; true
        case None         => false
      }

    // Repeat try reducing
    while (tryReduce) {}
    ast.toScript

  def reduce(ast: Ast): List[Ast] =
    val orig = ast.toScript
    (new Reducer).walk(ast).filter(newAst => newAst.toScript != orig)

  private class Reducer extends AdditiveListWalker {

    private var firstChilds: Map[Syntactic, List[Syntactic]] =
      Map().withDefaultValue(List())
    // collect list of parents for each name
    private var parentMap: Map[String, List[Syntactic]] =
      Map().withDefaultValue(List())

    private var substituteMap: Map[Syntactic, Syntactic] = {
      val undefinedMemberExpressions =
        List("0 . x", "[ ] . x", "{ } . x", "'' . x")

      val undefinedFunc = cfg
        .esParser("CoverCallExpressionAndAsyncArrowHead", List(false, false))
        .from("function ( ) { } ( )")
        .asInstanceOf[Syntactic]
      val undefined =
        cfg
          .esParser("LeftHandSideExpression", List(false, false))
          .from("undefined")
          .asInstanceOf[Syntactic]

      undefinedMemberExpressions
        .map(str =>
          (
            cfg
              .esParser("LeftHandSideExpression", List(false, false))
              .from(str)
              .asInstanceOf[Syntactic],
            undefined,
          ),
        )
        .toMap + (undefinedFunc -> undefined)
    }

    override def walk(ast: Lexical): List[Lexical] = List()
    override def walk(ast: Syntactic): List[Syntactic] =
      val Syntactic(name, args, rhsIdx, children) = ast

      // update parentMap
      val origParents = parentMap(name)
      parentMap += (name -> (ast :: origParents))
      val parentStmts = parentMap("Statement")

      // update firstChilds
      // 1. Extract child with same name
      origParents.headOption.map(parent =>
        val origChilds = firstChilds(parent)
        // add (parent -> ast) for closest parent with same name
        firstChilds += (parent -> (ast :: origChilds)),
      )

      // 4. Extract expression into statement
      // ex) switch (e1) { case e2: }
      // ->
      // (e1) ;
      // (e2) ;
      if (name == "Expression")
        parentStmts.headOption.map(parentStmt =>
          val origChilds = firstChilds(parentStmt)
          // add (parentStmt -> ast) for closest statement parent of ast
          val wrappedExprStmt = Syntactic(
            "ExpressionStatement",
            parentStmt.args.take(2),
            0,
            Vector(Option(ast)),
          )
          val wrappedStmt = Syntactic(
            "Statement",
            parentStmt.args,
            3,
            Vector(Option(wrappedExprStmt)),
          )
          firstChilds += (parentStmt -> (wrappedStmt :: origChilds)),
        )

      var mutants = super.walk(ast)

      // restore parentMap (remove current ast from parentMap, "pop")
      parentMap += (name -> origParents)

      // Replace current node with selected children
      mutants = firstChilds(ast) ++ mutants
      // 2. Remove first element from two-elemented list
      val isDoubleton = optional {
        val child = children(0).get.asInstanceOf[Syntactic]
        name == child.name
        && children.length - 1 == child.children.length
        && children.drop(1).map(_.fold("")(_.name)) == child.children.map(
          _.fold("")(_.name),
        )
      }.getOrElse(false)
      if (isDoubleton)
        val singleton = Syntactic(
          name,
          args,
          children(0).get.asInstanceOf[Syntactic].rhsIdx,
          children.drop(1),
        )
        mutants = singleton :: mutants

      // 3. Remove optional children
      // ex) class A extends B { body }
      // ->
      // class A extends B { }
      // class A { body }

      // drop i-th child from the current ast
      def dropChild(i: Int): Unit =
        val newChildren = children.updated(i, None)
        val mutant = Syntactic(name, args, rhsIdx, newChildren)
        mutants = mutant :: mutants

      // get production rules with same name
      val matchingProds = cfg.grammar.prods.filter(_.lhs.name == name)
      // filter optional nonterminals and get matching mutants
      matchingProds.foreach(prod =>
        prod
          .rhsVec(rhsIdx)
          .nts
          .zipWithIndex
          .collect {
            case (nt, idx) if nt.optional => idx
          }
          .foreach(dropChild),
      )

      // 5. Special handling for ??
      // ex) e1 ?? e2 ;
      // ->
      // e1 ;
      // e2 ;
      if (ast.name == "CoalesceExpression")
        val coalesceExprHead = ast.children(0).get.asInstanceOf[Syntactic]
        val e1 = coalesceExprHead.children(0).get.asInstanceOf[Syntactic]

        val e2 = ast.children(0).get.asInstanceOf[Syntactic]
        mutants = e1 :: e2 :: mutants

      // 6. Substitue with undefined / null
      // ex) function () {} ()
      // ->
      // undefined
      // !! this breaks AST structure
      if (substituteMap.contains(ast))
        mutants = substituteMap(ast) :: mutants

      // 7. Replace ?. with .
      // this might also break AST structure
      val ff = List(false, false)
      // helpers for transformation

      /*def optchainToMem(ast: Syntactic, inner: Option[Ast]) = ast match
        case Syntactic("OptionalChain", _, optChainIdx, optChainChildren) =>
          optChainIdx match
            case 0 =>
              Syntactic("CallExpression", ff, 0, Vector(
                Some(Syntactic("CoverCallExpressionAndAsyncArrowHead", ff, 0, Vector(inner, optChainChildren(0))).asInstanceOf[Ast])
              ))
            case 1 | 2 | 3 =>
              Syntactic("MemberExpression", ff, optChainIdx, Vector(inner, optChainChildren(0)))
            case 4 =>
              Syntactic("MemberExpression", ff, 7, Vector(inner, optChainChildren(0))) */

      // mutate a nested OptionalChain to nested CallExpression
      def optChainToCall(optChain: Syntactic, inner: Option[Ast]): Syntactic =
        val Syntactic(name, _, optChainIdx, optChainChildren) = optChain
        assert(name == "OptionalChain")
        optChainIdx match
          case 0 | 1 | 2 | 3 | 4 =>
            Syntactic(
              "CallExpression",
              ff,
              optChainIdx + 3,
              Vector(inner, optChainChildren(0)),
            )
          case _ =>
            val leftChain = optChainChildren(0).get.asInstanceOf[Syntactic]
            Syntactic(
              "CallExpression",
              ff,
              optChainIdx - 2,
              Vector(
                Some(optChainToCall(leftChain, inner)),
                optChainChildren(1),
              ),
            )

      // mutate whole body of OptionalExpression and nested
      def replaceOptionalAccess(ast: Syntactic): Syntactic =
        val Syntactic(_, _, optIdx, children) = ast
        optIdx match
          case 0 | 1 =>
            optChainToCall(children(1).get.asInstanceOf[Syntactic], children(0))
          case 2 =>
            val convertedToCall = replaceOptionalAccess(
              children(0).get.asInstanceOf[Syntactic],
            )
            Syntactic(
              "OptionalExpression",
              ff,
              1,
              Vector(Some(convertedToCall.asInstanceOf[Ast]), children(1)),
            )

      if (name == "OptionalExpression")
        val newAst = replaceOptionalAccess(ast)
        mutants = newAst :: mutants

      // 7. Others?

      mutants.distinctBy(_.toScript)
  }

  val cache: MMap[String, Option[CResult]] = MMap()

  def isSameResult(r1: CResult)(r2: CResult): Boolean =
    val (expected1, actual1, stdout1) = r1
    val (expected2, actual2, stdout2) = r2
    expected1.equivalent(expected2)
    && actual1.equivalent(actual2)
    && (stdout1.isEmpty == stdout2.isEmpty)

  /** check if reduced test fails the test for same reason */
  private def isSameFail(origResult: CResult)(ast: Ast): Boolean =
    isSameFail(origResult)(ast.toScript)
  private def isSameFail(origResult: CResult)(code: String): Boolean =
    val newResultOpt: Option[CResult] = cache.getOrElseUpdate(
      code,
      if !ValidityChecker(code) then None else optional(runConformTest(code)),
    )

    newResultOpt.fold(false)(isSameResult(origResult))
}
