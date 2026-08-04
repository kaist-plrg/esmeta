package esmeta.wji

import org.scalatest.funsuite.AnyFunSuite
import esmeta.wji.lang.*
import esmeta.wji.lang.parser.InstrParser
import Instr.*
import Instr.PerformOutcome.*
import Expr.*
import Cond.*

class InstrParserSpec extends AnyFunSuite:

  private def parse(text: String, sub: List[Instr] = Nil): List[Instr] =
    InstrParser.parseStepText(text, sub)

  // ---- representative cases, one (or a few related) per pattern ----

  test("let / set") {
    assert(parse("Let |x| be |y|.") == List(Let(Var("x"), Var("y"))))
    assert(parse("Set |x| to |y|.") == List(Set(Var("x"), Var("y"))))
  }

  test("assert / note") {
    assert(
      parse("Assert: |x| is |y|.") == List(Assert(Eq(Var("x"), Var("y")))),
    )
    assert(parse("Note: this is a note.") == List(Note("this is a note")))
  }

  test("return, with and without a value") {
    assert(parse("Return |x|.") == List(Return(Some(Var("x")))))
    assert(parse("Return.") == List(Return(None)))
  }

  test("throw") {
    assert(
      parse("Throw a {{TypeError}} exception.") ==
      List(Throw(New("TypeError"))),
    )
    assert(
      parse("Throw |exception|.") ==
      List(Throw(Var("exception"))),
    )
  }

  test("if, unambiguous (no else branch nearby)") {
    assert(
      parse("If |x| is |y|, return |x|.") ==
      List(If(Eq(Var("x"), Var("y")), List(Return(Some(Var("x")))))),
    )
  }

  test("for each, taking its sub-steps as its body") {
    // unlike If/For/While below, ForEach has no inline-trailing-action
    // support (no `deriveBody` call) — its body is always exactly whatever
    // sub-list items are passed in, never text after a trailing comma.
    assert(
      parse("For each |x| of |list|.", List(Return(Some(Var("x"))))) ==
      List(ForEach(Var("x"), Var("list"), List(Return(Some(Var("x")))))),
    )
  }

  test("for-in-range, with an inline trailing action") {
    assert(
      parse("For |i| in [=the range=] |a| to |b|, inclusive, return |i|.") ==
      List(
        For(Var("i"), Range(Var("a"), Var("b")), List(Return(Some(Var("i"))))),
      ),
    )
  }

  test("while, taking its sub-steps as its body") {
    // like ForEach, While has no inline-trailing-action support either —
    // everything after the ":" is folded into `rest` and handed to
    // CondParser as part of the condition text unless sub-steps are given.
    assert(
      parse("While |x| is |y|:", List(Return(Some(Var("x"))))) ==
      List(While(Eq(Var("x"), Var("y")), List(Return(Some(Var("x")))))),
    )
  }

  test("append / iteration continue") {
    assert(
      parse("[=list/Append=] |x| to |list|.") ==
      List(Append(Var("x"), Var("list"))),
    )
    assert(parse("[=iteration/continue=].") == List(Continue()))
  }

  test("run the following steps in parallel, taking sub-steps as its body") {
    assert(
      parse("Run the following steps in parallel.", List(Return(None))) ==
      List(RunInParallel(List(Return(None)))),
    )
  }

  test("perform, discarding the result") {
    assert(
      parse("Perform [$Foo$](|x|).") ==
      List(Perform("Foo", List(Var("x")), Discard)),
    )
  }

  test("perform ... and return the result") {
    assert(
      parse("Perform [$Foo$](|x|), and return the result.") ==
      List(Perform("Foo", List(Var("x")), ReturnResult)),
    )
  }

  test(
    "perform ... and let VAR be the result / ... and store the result as VAR",
  ) {
    assert(
      parse("Perform [$Foo$](|x|), and let |y| be the result.") ==
      List(Perform("Foo", List(Var("x")), BindResult("|y|"))),
    )
    assert(
      parse("Perform [$Foo$](|x|), and store the result as |y|.") ==
      List(Perform("Foo", List(Var("x")), BindResult("|y|"))),
    )
  }

  test("perform ... and return (bare, unrelated to the call's own result)") {
    assert(
      parse("Perform [$Foo$](|x|), and return.") ==
      List(Perform("Foo", List(Var("x")), Discard, List(Return(None)))),
    )
  }

  // ---- order-sensitive cases: two patterns whose surface syntax genuinely
  // overlaps, where one must be tried before the other. Each assertion here
  // only holds if the relevant case order in InstrParser.scala stays as-is. ----

  test(
    "order: AbruptCallPrefix is tried before LeadingAlgoLink, inside parseCall",
  ) {
    // "[=!=]" also matches LeadingAlgoLink's "[=...=]" shape (any non-']'
    // content is accepted as a link name) — if LeadingAlgoLink won this
    // race, "!" would be parsed as the call target itself, and the real
    // JS-call target ([$SetIntegrityLevel$](...)) would end up misparsed as
    // a bare argument instead.
    assert(
      parse("Perform [=!=] [$SetIntegrityLevel$](|obj|).") ==
      List(Perform("SetIntegrityLevel", List(Var("obj")), Discard)),
    )
  }

  test("order: ElseIfPrefix is tried before ElsePrefix") {
    // "Else if ..."/"Otherwise, if ..." also match ElsePrefix's bare
    // "(Else|Otherwise)[,] ...$" shape — if ElsePrefix won this race, the
    // whole "if COND, ACTION" tail would be swallowed as a plain Else's
    // body (itself then re-parsed as a *nested* If) instead of a sibling
    // ElseIf.
    assert(
      parse("Else if |x| is |y|, return |x|.") ==
      List(ElseIf(Eq(Var("x"), Var("y")), List(Return(Some(Var("x")))))),
    )
    assert(
      parse("Otherwise, if |x| is |y|, return |x|.") ==
      List(ElseIf(Eq(Var("x"), Var("y")), List(Return(Some(Var("x")))))),
    )
  }
