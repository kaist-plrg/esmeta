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
      val undefinedProp = cfg
        .esParser("MemberExpression", List(false, false))
        .from("0 . x")
        .asInstanceOf[Syntactic]
      val undefinedFunc = cfg
        .esParser("CoverCallExpressionAndAsyncArrowHead", List(false, false))
        .from("function ( ) { } ( )")
        .asInstanceOf[Syntactic]
      val undefined =
        cfg
          .esParser("LeftHandSideExpression", List(false, false))
          .from("undefined")
          .asInstanceOf[Syntactic]

      Map(undefinedProp -> undefined, undefinedFunc -> undefined)
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
          val wrappedAst = Syntactic(
            "Statement",
            List(false, false, false),
            3,
            Vector(
              Option(
                Syntactic(
                  "ExpressionStatement",
                  List(false, false),
                  0,
                  Vector(Option(ast)),
                ),
              ),
            ),
          )
          firstChilds += (parentStmt -> (ast :: origChilds)),
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

      // get production rules with same name
      val matchingProds = cfg.grammar.prods.filter(p => p.lhs.name == name)
      // filter optional nonterminals and get matching mutants
      matchingProds.foreach(prod =>
        prod
          .rhsVec(rhsIdx)
          .nts
          .zipWithIndex
          .foreach((nt, i) =>
            if (nt.optional)
              val mutant = Syntactic(
                name,
                args,
                rhsIdx,
                children.zipWithIndex.map((child, idx) =>
                  if (i == idx)
                    None
                  else
                    child,
                ),
              )
              mutants = mutant :: mutants,
          ),
      )

      // 5. Special handling for ??
      // ex) e1 ?? e2 ;
      // ->
      // e1 ;
      // e2 ;

      // 6-1. Substitue with undefined / null
      // ex) function () {} ()
      // ->
      // undefined
      //
      if (ast.name == "CoverCallExpressionAndAsyncArrowHead")
        println(s"cur:${ast}")
      if (substituteMap.contains(ast))
        println(s"match: ${ast.name}")
        mutants = substituteMap(ast) :: mutants

      // 6-2. Replace ?. with .

      // 7. Others?

      mutants.distinctBy(_.toScript)
  }

  val cache: MMap[String, Option[CResult]] = MMap()

  def isSameResult(r1: CResult)(r2: CResult): Boolean =
    val (expected1, actual1, _) = r1
    val (expected2, actual2, _) = r2
    expected1.equivalent(expected2) && actual1.equivalent(actual2)

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
