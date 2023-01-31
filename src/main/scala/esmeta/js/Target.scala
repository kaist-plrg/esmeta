package esmeta.js

import esmeta.{LOG_DIR, LINE_SEP}
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.error.{TimeoutException, NoCommandError}
import esmeta.es.*
import esmeta.es.util.{USE_STRICT}
import esmeta.es.util.injector.*
import esmeta.es.util.ValidityChecker
import esmeta.es.util.mutator.Util.AdditiveListWalker

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

  private val errorPattern = "[\\w]*Error(?=: )".r

  def doConformTest(code: String): Boolean = doConformTest(Injector(code))
  def doConformTest(test: ConformTest): Boolean = {

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
        val etest = testMaker(code)
        JSEngine
          .runUsingBinary(cmd, etest)
          .map((NormalTag, _))
          .recover(engineErrorResolver _)
          .get
      } else {
        var tempFile = s"$LOG_DIR/$name-temp.js"
        var tempOutFile = s"$LOG_DIR/$name-tempOut.js"
        dumpFile(code, tempFile)
        JSTrans
          .transpileFileUsingBinary(
            cmd,
            tempFile,
            tempOutFile,
          )
          .get
        var compiledCode = readFile(tempOutFile)
        val ttest = testMaker(compiledCode)
        JSEngine
          .run(ttest)
          .map((NormalTag, _))
          .recover(engineErrorResolver _)
          .get
      }
    val sameExitTag = exitTag.equivalent(concreteExitTag)
    val pass = sameExitTag && stdout.isEmpty

    pass
  }

  // header and footer for tests
  private val libHead = "(()=>{"
  private val libTail = "})();"
  private val delayHead = "$delay(() => {"
  private val delayTail = "});"
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

  var cache: Map[String, Boolean] = Map()

  def minimize(code: String): String =
    var ast = Ast(code)

    def tryReduce: Boolean =
      val result = reduce(ast).find(isFail)
      result.foreach(newAst => { ast = newAst })
      result.isDefined

    while (tryReduce) {}
    ast.toScript

  def reduce(ast: Ast): List[Ast] =
    val orig = ast.toScript
    (new Reducer).walk(ast).filter(newAst => newAst.toScript != orig)
  private class Reducer extends AdditiveListWalker {
    private var firstChilds: Map[Syntactic, List[Syntactic]] =
      Map().withDefaultValue(List())
    private var parentMap: Map[String, List[Syntactic]] =
      Map().withDefaultValue(List())

    override def walk(ast: Lexical): List[Lexical] = List()
    override def walk(ast: Syntactic): List[Syntactic] =
      val Syntactic(name, args, rhsIdx, children) = ast

      // update parentMap
      val origParents = parentMap(name)
      parentMap += (name -> (ast :: origParents))

      // update firstChilds
      origParents.headOption.map(parent =>
        val origChilds = firstChilds(parent)
        firstChilds += (parent -> (ast :: origChilds)),
      )

      var mutants = super.walk(ast) ++ firstChilds(ast)
      // Remove first element from left-recursive list
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

      // restore parentMap
      parentMap += (name -> origParents)

      mutants.distinctBy(_.toScript)
  }

  /** check if reduced test still fails the test */
  private def isFail(ast: Ast): Boolean = isFail(ast.toScript)
  private def isFail(code: String): Boolean =
    if cache.contains(code) then cache(code)
    else
      val result =
        if !ValidityChecker(code) then false
        else optional(!doConformTest(code)).getOrElse(false)
      cache += (code -> result)
      result
}
