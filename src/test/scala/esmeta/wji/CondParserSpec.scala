package esmeta.wji

import org.scalatest.funsuite.AnyFunSuite
import esmeta.wji.lang.*
import esmeta.wji.lang.parser.CondParser
import Expr.*
import Cond.*

class CondParserSpec extends AnyFunSuite:

  // ---- representative cases, one (or a few related) per pattern ----

  test("is-a-type, positive and negative") {
    assert(
      CondParser.parse("|x| [=is a Number=]") == IsType(Var("x"), "Number"),
    )
    assert(
      CondParser.parse("|x| [=is not a Number=]") ==
      IsType(Var("x"), "Number", negated = true),
    )
  }

  test("matches, positive and negative") {
    // real spec occurrences always write the dfn link with a "|matches"
    // display-text alias (e.g. "[=matches/valtype|matches=]") — see
    // ir.expected's "match_valtype" call sites.
    assert(
      CondParser.parse("|a| [=matches/valtype|matches=] |b|") ==
      Matches(Var("a"), "valtype", Var("b")),
    )
    assert(
      CondParser.parse("|a| does not [=matches/valtype|matches=] |b|") ==
      Matches(Var("a"), "valtype", Var("b"), negated = true),
    )
  }

  test("map existence, positive and negative") {
    assert(
      CondParser.parse("|map|[|key|] [=map/exists=]") ==
      HasField(Index(Var("map"), Var("key"))),
    )
    assert(
      CondParser.parse("|map|[|key|] [=map/doesn't exist=]") ==
      HasField(Index(Var("map"), Var("key")), negated = true),
    )
  }

  test("list emptiness, plain and the 'is not empty' alias") {
    assert(
      CondParser.parse("|list| [=list/is empty=]") ==
      Eq(Length(Var("list")), Num("0")),
    )
    assert(
      CondParser.parse("|list| [=list/is empty|is not empty=]") ==
      Eq(Length(Var("list")), Num("0"), negated = true),
    )
  }

  test("implements, positive and negative") {
    assert(
      CondParser.parse("|x| [=implements=] {{Iterable}}") ==
      Implements(Var("x"), "Iterable"),
    )
    assert(
      CondParser.parse("|x| does not [=implement=] {{Iterable}}") ==
      Implements(Var("x"), "Iterable", negated = true),
    )
  }

  test("has internal slot, positive and negative") {
    assert(
      CondParser.parse("|x| has a [[Foo]] internal slot") ==
      HasSlot(Var("x"), "Foo"),
    )
    assert(
      CondParser.parse("|x| does not have a [[Foo]] internal slot") ==
      HasSlot(Var("x"), "Foo", negated = true),
    )
  }

  test("has duplicates, positive and negative phrasings") {
    assert(
      CondParser.parse("|list| contains any duplicates") ==
      HasDuplicates(Var("list")),
    )
    assert(
      CondParser.parse("|list| contains no duplicates") ==
      HasDuplicates(Var("list"), negated = true),
    )
    assert(
      CondParser.parse("|list| does not contain any duplicates") ==
      HasDuplicates(Var("list"), negated = true),
    )
  }

  test("call-with-English-connectors compared against a boolean") {
    assert(
      CondParser.parse("[=foo=] for |a| is true") ==
      Eq(Link("[=foo=]", List(Var("a"))), Bool(true)),
    )
  }

  test("existential quantifier over one or more collections") {
    // the binder after "any" is written bare (e.g. "any type in ..."), not
    // pipe-delimited — see metalang.expected's "any type in |parameters| or
    // |results| |type| [=matches/valtype=] ..." occurrences.
    assert(
      CondParser.parse("any t in |xs| [=matches/valtype|matches=] |v|") ==
      Exists(
        "t",
        List(Var("xs")),
        Matches(Var("t"), "valtype", Var("v")),
      ),
    )
  }

  test("or / and composition") {
    assert(
      CondParser.parse("|a| is |b| or |c| is |d|") ==
      Or(Eq(Var("a"), Var("b")), Eq(Var("c"), Var("d"))),
    )
    assert(
      CondParser.parse("|a| is |b| and |c| is |d|") ==
      And(Eq(Var("a"), Var("b")), Eq(Var("c"), Var("d"))),
    )
  }

  test("an unparseable 'or' right-hand side salvages to Abbreviated") {
    assert(
      CondParser.parse("|x| is [=A=] or [=B=]") ==
      Or(
        Eq(Var("x"), Link("[=A=]", Nil)),
        Abbreviated(Link("[=B=]", Nil)),
      ),
    )
  }

  test("is of the form, with a nested Link payload") {
    assert(
      CondParser.parse("|x| is of the form [=external-type/func=] |ft|") ==
      IsOfForm(
        Var("x"),
        Link("[=external-type/func=]", List(Var("ft"))),
      ),
    )
  }

  test("missing / given") {
    assert(CondParser.parse("|x| is missing") == IsMissing(Var("x")))
    assert(
      CondParser.parse("|x| is given") == IsMissing(Var("x"), negated = true),
    )
  }

  test("contained in") {
    assert(
      CondParser.parse("|x| is contained in |list|") ==
      Contains(Var("x"), Var("list")),
    )
  }

  test("relational comparison") {
    assert(
      CondParser.parse("|a| < |b|") == Compare(Var("a"), CompareOp.Lt, Var("b")),
    )
    assert(
      CondParser
        .parse("|a| >= |b|") == Compare(Var("a"), CompareOp.Ge, Var("b")),
    )
  }

  test("plain equality") {
    assert(CondParser.parse("|a| is |b|") == Eq(Var("a"), Var("b")))
  }

  test("unreachable-step marker") {
    assert(CondParser.parse("this step is not reached") == Unreachable)
  }

  test("throws, untyped and typed") {
    assert(
      CondParser.parse("this operation throws an exception") == Throws(None),
    )
    assert(
      CondParser.parse("this throws a {{TypeError}}") ==
      Throws(Some("TypeError")),
    )
  }

  // ---- order-sensitive cases: within parseEqOrCompare's `.orElse` chain,
  // the longer/more-specific separator must be tried before the shorter one
  // it's a superstring of — see personal/parser_refactor_ideas.md (D-1). ----

  test("order: ' is not equal to ' is tried before ' is not '") {
    // " is not equal to " *contains* " is not " as a literal substring, so if
    // " is not " won this race, it would split the sentence right after
    // "not" and hand parseRhs the leftover "equal to |b|" as if it were the
    // whole right-hand side, instead of just |b|.
    assert(
      CondParser.parse("|a| is not equal to |b|") ==
      Eq(Var("a"), Var("b"), negated = true),
    )
  }

  test("order: ' is equal to ' is tried before ' is '") {
    // same shape of hazard as above, one level down: " is equal to "
    // contains " is " as a substring.
    assert(
      CondParser.parse("|a| is equal to |b|") == Eq(Var("a"), Var("b")),
    )
  }
