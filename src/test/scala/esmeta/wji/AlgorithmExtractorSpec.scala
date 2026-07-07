package esmeta.wji

import org.scalatest.funsuite.AnyFunSuite
import esmeta.wji.lang.*
import Instr.*
import Expr.*
import Cond.*

class AlgorithmExtractorSpec extends AnyFunSuite:

  private lazy val algorithms =
    AlgorithmExtractor.extractFromFile(SpecFile.jsApiIndex)

  test("extracts every <div algorithm> block in index.bs") {
    val text = scala.io.Source.fromFile(SpecFile.jsApiIndex.toFile).mkString
    val expected = """<div\s+algorithm""".r.findAllMatchIn(text).size
    assert(expected > 0)
    assert(algorithms.size == expected)
  }

  test("parses a flat algorithm into its head and ordered instrs") {
    val algo =
      algorithms.find(_.name.contains("compile a WebAssembly module")).get
    assert(algo.head.contains("perform the following steps"))
    assert(
      algo.body == List(
        Let(Var("module"), AlgoCall("[=module_decode=]", List(Var("bytes")))),
        If(
          Eq(Var("module"), Link("[=error=]", Nil)),
          List(Return(Some(Link("[=error=]", Nil)))),
        ),
        If(
          Eq(
            AlgoCall("[=module_validate=]", List(Var("module"))),
            Link("[=error=]", Nil),
          ),
          List(Return(Some(Link("[=error=]", Nil)))),
        ),
        Return(Some(Var("module"))),
      ),
    )
  }

  test(
    "reconstructs nested instrs (including bullet sub-lists) from indentation",
  ) {
    val algo = algorithms
      .find(_.name.contains("instantiate the core of a WebAssembly module"))
      .get
    val ifInstr = algo.body.find {
      case If(Cond.Eq(Expr.Var("result"), _, _), _) => true
      case _                                        => false
    }.get
    assert(
      ifInstr.body.map {
        case Instr.Unknown(text, _)    => text
        case If(Cond.Unknown(text), _) => text
        case other                     => other.toString
      } == List(
        "A {{LinkError}} exception for most cases which occur during linking",
        "the error came when running the start function",
        "Another error type if appropriate, for example an out-of-memory exception, as documented in <a href=\"#errors\">the WebAssembly error mapping</a>",
      ),
    )
  }

  test(
    "falls back to the algorithm=\"...\" attribute as id when there is no <dfn>",
  ) {
    val algo = algorithms.find(_.id.contains("js-string-cast")).get
    assert(algo.name.isEmpty)
    assert(
      algo.body == List(
        Return(Some(Abrupt("?", JSCall("UnwrapString", List(Var("v")))))),
      ),
    )
  }

  test("supports algorithms with no instrs at all") {
    val algo =
      algorithms.find(a => a.name.contains("exports") && a.params.isEmpty).get
    assert(algo.body.isEmpty)
    assert(algo.head.nonEmpty)
  }

  test(
    "extracts params written inside the <dfn> as 'name(|arg1|, |arg2|)', stripping them from name",
  ) {
    val algo = algorithms.find(_.name.contains("CharCodeAt")).get
    assert(algo.name.contains("CharCodeAt"))
    assert(algo.params == List("|string|", "|index|"))
  }

  test("extracts params written in the prose after '<dfn>name</dfn>'") {
    val algo = algorithms.find(_.name.contains("create an exports object")).get
    assert(algo.params == List("|module|", "|instance|"))
  }

  test(
    "extracts params that appear both right after '</dfn>' and later in the prose",
  ) {
    val algo =
      algorithms.find(_.name.contains("initialize an instance object")).get
    assert(algo.params == List("|instanceObject|", "|module|", "|instance|"))
  }

  test("supports algorithms with no params") {
    val algo =
      algorithms.find(_.name.contains("get the JavaScript exception tag")).get
    assert(algo.params.isEmpty)
  }
