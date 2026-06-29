package esmeta.wji

import org.scalatest.funsuite.AnyFunSuite
import esmeta.wji.lang.*
import Instr.*
import Instr.PerformOutcome.*
import Expr.*
import Cond.*

class InstrParserSpec extends AnyFunSuite:

  private lazy val algorithms = AlgorithmExtractor.extractFromFile(SpecFile.jsApiIndex)

  test("splits a multi-sentence step into separate top-level instrs") {
    val algo = algorithms.find(_.name.contains("compile a WebAssembly module")).get
    val instrs = algo.body
    assert(instrs == List(
      Let(Var("module"), AlgoCall("[=module_decode=]", List(Var("bytes")))),
      If(Eq(Var("module"), AlgoCall("[=error=]", Nil)), List(Return(Some(AlgoCall("[=error=]", Nil))))),
      If(Eq(AlgoCall("[=module_validate=]", List(Var("module"))), AlgoCall("[=error=]", Nil)), List(Return(Some(AlgoCall("[=error=]", Nil))))),
      Return(Some(Var("module"))),
    ))
  }

  test("represents a single-line 'X; otherwise, Y' as sibling If/Else instrs") {
    val algo = algorithms.find(_.name.contains("Memory")).get
    val instrs = algo.body
    assert(instrs.take(2) == List(
      If(MapExists(Index(Var("descriptor"), Str("address"))), List(Let(Var("addrtype"), Index(Var("descriptor"), Str("address"))))),
      Else(List(Let(Var("addrtype"), Str("i32")))),
    ))
  }

  test("parses a flat sequence of Let/If/Return") {
    val algo = algorithms.find(_.id.contains("js-string-compare")).get
    val instrs = algo.body
    assert(instrs == List(
      Let(Var("first"), Abrupt("?", JSCall("UnwrapString", List(Var("first"))))),
      Let(Var("second"), Abrupt("?", JSCall("UnwrapString", List(Var("second"))))),
      If(Eq(Abrupt("!", AlgoCall("[=IsStrictlyEqual=]", List(Var("first"), Var("second")))), Bool(true)), List(Return(Some(Num("0"))))),
      If(Eq(Abrupt("!", AlgoCall("[=IsLessThan=]", List(Var("first"), Var("second"), Bool(true)))), Bool(true)), List(Return(Some(Neg(Num("1")))))),
      Return(Some(Num("1"))),
    ))
  }

  test("treats a ', and ...' clause as part of the condition, not the action separator") {
    val algo = algorithms.find(_.id.contains("read-the-imports")).get
    val instrs = algo.body
    assert(instrs.head == If(
      And(
        Cond.Unknown("|module|.[=imports=] [=list/is empty|is not empty=]"),
        Eq(Var("importObject"), SpecConst("undefined")),
      ),
      List(Throw("a {{TypeError}} exception")),
    ))
  }

  test("parses 'For each ELEM of/in COLLECTION' with a nested body") {
    val algo = algorithms.find(_.id.contains("create the WebAssembly namespace object")).get
    val instrs = algo.body
    assert(instrs(1) == ForEach(
      "|error|",
      """« "CompileError", "LinkError", "RuntimeError" »""",
      List(
        Let(Var("constructor"), Expr.UnknownNew("a new object, implementing the [=NativeError Object Structure=], with <var ignore>NativeError</var> set to |error|")),
        Perform("[=!=]", List(JSCall("DefineMethodProperty", List(Var("namespaceObject"), Var("error"), Var("constructor"), Bool(false))))),
      ),
    ))
  }

  test("attaches nested sub-steps as the body of their introducing instr, even for Let") {
    val algo = algorithms.find(_.name.contains("create a host function")).get
    val instrs = algo.body
    val hostfunc = instrs.collectFirst { case l @ Let(Var("hostfunc"), _, _) => l }.get
    assert(hostfunc.body.head == Let(Var("realm"), Expr.Unknown("|func|'s [=associated Realm=]")))
  }

  test("parses 'Set X as specified in Y' as Set(ref, expr)") {
    val algo = algorithms.find(_.name.contains("a new Exported GC Object")).get
    val instrs = algo.body
    assert(instrs.contains(
      Set(Slot(Var("object"), "GetPrototypeOf"), Expr.Unknown("[=[[GetPrototypeOf]] internal method of an Exported GC Object=]"))
    ))
  }

  test("parses 'Perform OP, and return the result' as Perform with ReturnResult") {
    val algo = algorithms.find(a => a.name.contains("instantiate") && a.params.contains("|bytes|")).get
    val instrs = algo.body
    assert(instrs.contains(
      Perform("[=Instantiate a promise of a module=]", List(Var("promiseOfModule"), Var("importObject")), ReturnResult)
    ))
  }

  test("parses 'Perform OP, and let |x| be the result' as Perform with BindResult") {
    val algo = algorithms.find(_.id.contains("read-the-imports")).get
    val instrs = algo.body
    assert(containsDeep(instrs, Perform("[=Create a host function=]", List(Var("v"), Var("functype")), BindResult("|funcaddr|"))))
  }

  test("parses 'N<sup>M</sup>' as Pow, '-N<sup>M</sup>' as Neg(Pow), and Pow combined with &minus; as BinOp") {
    assert(ExprParser.parse("2<sup>30</sup>")        == Pow(Num("2"), Num("30")))
    assert(ExprParser.parse("-2<sup>30</sup>")       == Neg(Pow(Num("2"), Num("30"))))
    assert(ExprParser.parse("2<sup>64</sup> &minus; 1") == BinOp(Pow(Num("2"), Num("64")), "&minus;", Num("1")))
  }

  test("parses '[=is [not] a[n] TYPE=]' as IsType") {
    val unwrap = algorithms.find(_.name.contains("UnwrapString")).get
    assert(unwrap.body.head == If(IsType(Var("v"), "String", negated = true), List(
      Throw("a {{RuntimeError}} exception as if a [=trap=] was executed")
    )))
    val imports = algorithms.find(_.id.contains("read-the-imports")).get
    assert(containsDeep(imports.body, If(
      And(Eq(Var("valtype"), AlgoCall("[=i64=]", Nil)), IsType(Var("v"), "BigInt", negated = true)),
      List(Throw("a {{LinkError}} exception"))
    )))
  }

  test("parses 'the length of X' and 'the number of elements in X' as Length") {
    val memAlgo = algorithms.find(_.name.contains("create a fixed length memory buffer")).get
    assert(containsDeep(memAlgo.body, Set(Slot(Var("buffer"), "ArrayBufferByteLength"), Length(Var("block")))))
    val strAlgo = algorithms.find(_.id.contains("js-string-fromCharCodeArray")).get
    assert(containsDeep(strAlgo.body, Let(Var("length"), Length(Var("array")))))
  }

  test("parses 'the value of the element stored at index I in A' as Index(A, I)") {
    val algo = algorithms.find(_.id.contains("js-string-fromCharCodeArray")).get
    assert(containsDeep(algo.body, Let(Var("charCode"), Index(Var("array"), Var("i")))))
  }

  test("parses '(|x|, |y|)' tuple as Let LHS") {
    val algo = algorithms.find(_.name.contains("instantiate the core of a WebAssembly module")).get
    assert(containsDeep(algo.body,
      Let(Tuple(List(Var("store"), Var("instance"))), Var("result"))))
  }

  test("parses '|x| + 1' as BinOp inside a Set") {
    val algo = algorithms.find(_.name.contains("call an Exported Function")).get
    assert(containsDeep(algo.body, Set(Var("i"), BinOp(Var("i"), "+", Num("1")))))
  }

  test("parses '|x| interpreted as a [=mathematical value=]' as AsMath") {
    val algo = algorithms.find(_.name.contains("ToJSValue")).get
    assert(containsDeep(algo.body,
      Return(Some(AlgoCall("[=ℤ=]", List(AsMath(Var("i64"))))))))
  }

  private def containsDeep(instrs: List[Instr], target: Instr): Boolean =
    instrs.exists(i => i == target || containsDeep(i.body, target))
