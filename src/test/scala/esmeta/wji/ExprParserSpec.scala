package esmeta.wji

import org.scalatest.funsuite.AnyFunSuite
import esmeta.wji.lang.*
import esmeta.wji.lang.parser.{CondParser, ExprParser}
import Expr.*

class ExprParserSpec extends AnyFunSuite:

  // ---- representative cases, one (or a few related) per pattern ----

  test("abrupt-completion prefix") {
    assert(ExprParser.parse("[=?=] |x|") == Abrupt("?", Var("x")))
    assert(ExprParser.parse("[=!=] |x|") == Abrupt("!", Var("x")))
  }

  test("'the result of'/'either' are transparent wrappers") {
    assert(ExprParser.parse("the result of |x|") == Var("x"))
    assert(ExprParser.parse("either |x|") == Var("x"))
  }

  test("type-annotated prefix keeps the annotating term") {
    assert(
      ExprParser.parse("the [=external value=] [=func=] |x|") ==
      TypeAnnotated("external value", Link("[=func=]", List(Var("x")))),
    )
  }

  test("braced-term value, with and without a payload") {
    assert(ExprParser.parse("the {{Foo}} value |x|") == Var("x"))
    assert(ExprParser.parse("the {{undefined}} value") == SpecTerm("undefined"))
  }

  test("'the following steps ...:' closure idioms") {
    assert(
      ExprParser.parse("the following steps given argument |x|:") ==
      FollowingSteps(List("x")),
    )
    assert(
      ExprParser.parse("to perform the following steps:") == FollowingSteps(Nil),
    )
    assert(
      ExprParser.parse(
        "the following steps given the list of arguments |argValues|:",
      ) == FollowingSteps(List("argValues"), variadicLast = true),
    )
  }

  test("'performing CLOSURE given ARGS' invokes a closure value") {
    assert(
      ExprParser.parse("performing |f| given |x|") ==
      ClosureCall(Var("f"), List(Var("x"))),
    )
  }

  test(
    "backtick-wrapped quoted string is a SpecTerm, plain backticks are transparent",
  ) {
    assert(ExprParser.parse("`\"frozen\"`") == SpecTerm("frozen"))
    assert(ExprParser.parse("`code`") == Unknown("code"))
  }

  test("'[=the range=] LOW to HIGH'") {
    assert(
      ExprParser.parse("[=the range=] |a| to |b|") == Range(Var("a"), Var("b")),
    )
  }

  test(
    "call syntaxes: JSCall / explicit-paren AlgoCall / prose Link / bare Link",
  ) {
    assert(
      ExprParser.parse("[$Foo$](|a|, |b|)") ==
      JSCall("Foo", List(Var("a"), Var("b"))),
    )
    assert(
      ExprParser.parse("[=algo=](|a|)") == AlgoCall("[=algo=]", List(Var("a"))),
    )
    assert(
      ExprParser.parse("[=algo=] |a|") == Link("[=algo=]", List(Var("a"))),
    )
    assert(ExprParser.parse("the [=algo=]") == Link("[=algo=]", Nil))
  }

  test("'the result of passing ARGS to the [=link=]' is a direct AlgoCall") {
    assert(
      ExprParser.parse(
        "the result of passing |S| and |args| to the [=overload resolution algorithm=]",
      ) ==
      AlgoCall(
        "[=overload resolution algorithm=]",
        List(Var("S"), Var("args")),
      ),
    )
  }

  test("bare var / var-ignore / this") {
    assert(ExprParser.parse("|x|") == Var("x"))
    assert(ExprParser.parse("<var ignore>foo</var>") == Var("foo"))
    assert(ExprParser.parse("**this**") == This)
  }

  test("field/slot access: dotted slot, bare slot name, possessive slot") {
    assert(
      ExprParser.parse("|x|.[[Foo]]") == Field(Var("x"), "Foo"),
    )
    assert(ExprParser.parse("[[Foo]]") == Str("Foo"))
    assert(
      ExprParser.parse("the value of |x|'s [[Foo]] internal slot") ==
      Field(Var("x"), "Foo"),
    )
  }

  test("field access via dfn-link dot / plain dot") {
    assert(ExprParser.parse("|x|.[=name=]") == Field(Var("x"), "name"))
    assert(ExprParser.parse("|x|.foo") == Field(Var("x"), "foo"))
  }

  test("trailing-comma link call: 'VALUE, [=link=]'") {
    assert(
      ExprParser.parse("|x|, [=converted to a JavaScript value=]") ==
      Link("[=converted to a JavaScript value=]", List(Var("x"))),
    )
  }

  test("length-of phrasings all normalize to Length") {
    assert(ExprParser.parse("the length of |list|") == Length(Var("list")))
    assert(
      ExprParser.parse("the number of elements in |list|") ==
      Length(Var("list")),
    )
    assert(ExprParser.parse("|list|'s [=list/size=]") == Length(Var("list")))
  }

  test("element access / index-of") {
    assert(
      ExprParser.parse(
        "the value of the element stored at index |i| in |arr|",
      ) == Index(Var("arr"), Var("i")),
    )
    assert(
      ExprParser.parse("the index of |list| where |elem| is found") ==
      IndexOf(Var("list"), Var("elem")),
    )
  }

  test("shortest argument list of the entries in a list") {
    assert(
      ExprParser.parse("the shortest argument list in the entries in |S|") ==
      ShortestArgumentList(Var("S")),
    )
    assert(
      ExprParser.parse(
        "the length of the shortest argument list in the entries in |S|",
      ) == Length(ShortestArgumentList(Var("S"))),
    )
    assert(
      ExprParser.parse("the shortest type list of the entries of |S|") ==
      ShortestArgumentList(Var("S")),
    )
  }

  test("associated realm / possessive association") {
    assert(
      ExprParser.parse("|func|'s [=associated Realm=]") == Field(
        Var("func"),
        "Realm",
      ),
    )
    assert(
      ExprParser.parse("the |map|'s [=map/entries=]") ==
      Field(Var("map"), "map/entries"),
    )
  }

  test("index syntax: string / var / number / general-expr key") {
    assert(
      ExprParser.parse("""|obj|["key"]""") == Index(Var("obj"), Str("key")),
    )
    assert(ExprParser.parse("|obj|[|key|]") == Index(Var("obj"), Var("key")))
    assert(ExprParser.parse("|obj|[3]") == Index(Var("obj"), Num("3")))
  }

  test("new-object expressions") {
    assert(ExprParser.parse("a [=/new=] {{Promise}}") == New("Promise"))
    assert(ExprParser.parse("a [=/new=] {{Promise}} object") == New("Promise"))
    assert(ExprParser.parse("a {{TypeError}} exception") == New("TypeError"))
  }

  test("empty list / new byte sequence of length") {
    assert(ExprParser.parse("a new, empty list") == List_(Nil))
    assert(
      ExprParser.parse(
        "a new [=byte sequence=] of [=byte sequence/length=] equal to |n|",
      ) == NewByteSequence(Var("n")),
    )
  }

  test("relative-clause description, kept as raw unparsed text") {
    assert(
      ExprParser.parse("a [=Data Block=] which is identified with |x|") ==
      Described("[=Data Block=]", "is identified with |x|"),
    )
  }

  test("generic type-parameter instantiation") {
    assert(
      ExprParser.parse("of type <code>Promise&lt;T&gt;</code>") ==
      SpecTerm("T"),
    )
  }

  test("'such that' description: desc kept as raw text, cond parsed as Cond") {
    assert(
      ExprParser.parse("the unsigned integer such that |i64| is foo") ==
      SuchThat("unsigned integer", CondParser.parse("|i64| is foo")),
    )
  }

  test("indefinite noun-phrase single-arg call") {
    assert(
      ExprParser.parse("a [=foo=] |x|") == Link("[=foo=]", List(Var("x"))),
    )
  }

  test("math-value casts") {
    assert(
      ExprParser.parse("|x| interpreted as a [=mathematical value=]") ==
      AsMath(Var("x")),
    )
  }

  test("exponentiation") {
    assert(ExprParser.parse("2<sup>32</sup>") == Pow(Num("2"), Num("32")))
  }

  test(
    "generic 'a new ...' with no more specific pattern falls to UnknownNew",
  ) {
    assert(ExprParser.parse("a new Foo Bar") == UnknownNew("a new Foo Bar"))
  }

  test("map / list / tuple literals") {
    assert(
      ExprParser.parse("«[ |a| → |b| ]»") ==
      Map_(List(Var("a") -> Var("b"))),
    )
    assert(ExprParser.parse("the ordered map «[ ]»") == Map_(Nil))
    assert(
      ExprParser.parse("« |a|, |b| »") == List_(List(Var("a"), Var("b"))),
    )
    assert(
      ExprParser.parse("(|a|, |b|)") == Tuple(List(Var("a"), Var("b"))),
    )
  }

  test("record/struct literal ('the PropertyDescriptor{...}')") {
    assert(
      ExprParser.parse(
        "the PropertyDescriptor{[[Writable]]: <emu-val>true</emu-val>, [[Value]]: |constructor|}",
      ) ==
      RecordLit(
        "PropertyDescriptor",
        List(
          "Writable" -> SpecTerm("true"),
          "Value" -> Var("constructor"),
        ),
      ),
    )
    // bare (no "the " prefix), e.g. nested inside a call's argument list
    assert(
      ExprParser.parse("PropertyDescriptor{[[Value]]: |proto|}") ==
      RecordLit("PropertyDescriptor", List("Value" -> Var("proto"))),
    )
    // backslash-escaped brackets, as extracted straight from .bs source
    assert(
      ExprParser.parse("PropertyDescriptor{\\[[Value]]: |proto|}") ==
      RecordLit("PropertyDescriptor", List("Value" -> Var("proto"))),
    )
  }

  test("negation, applied to both a variable and a number literal") {
    assert(ExprParser.parse("-|x|") == Neg(Var("x")))
    assert(ExprParser.parse("-5") == Neg(Num("5")))
  }

  test("number / hex / string / boolean literals") {
    assert(ExprParser.parse("42") == Num("42"))
    assert(ExprParser.parse("3.14") == Num("3.14"))
    assert(ExprParser.parse("0x1F") == Num("0x1F"))
    assert(ExprParser.parse("\"foo\"") == Str("foo"))
    assert(ExprParser.parse("the empty string") == Str(""))
    assert(ExprParser.parse("true") == Bool(true))
    assert(ExprParser.parse("false") == Bool(false))
  }

  test("'the exception' aliases the Cond.Throws binding") {
    assert(ExprParser.parse("the exception") == Var("exception"))
  }

  test("bold constant keeps its markers verbatim") {
    assert(ExprParser.parse("**foo**") == SpecTerm("**foo**"))
  }

  test("glossary SpecTerm keywords") {
    assert(ExprParser.parse("undefined") == SpecTerm("undefined"))
    assert(ExprParser.parse("null") == SpecTerm("null"))
    assert(ExprParser.parse("empty") == SpecTerm("empty"))
    assert(ExprParser.parse("absent") == SpecTerm("absent"))
  }

  test("emu-const / emu-val / braced autolink all unify to SpecTerm") {
    assert(
      ExprParser.parse("<emu-const>throw</emu-const>") == SpecTerm("throw"),
    )
    assert(
      ExprParser.parse("<emu-val>undefined</emu-val>") == SpecTerm("undefined"),
    )
    assert(ExprParser.parse("{{uint8}}") == SpecTerm("uint8"))
  }

  test("cross-spec autolink / possessive settings-object") {
    assert(
      ExprParser.parse("the <a spec=HTML>incumbent settings object</a>") ==
      SpecTerm("incumbent settings object"),
    )
    assert(
      ExprParser.parse("|realm|'s [=realm/settings object=]") ==
      SpecTerm("realm/settings object"),
    )
  }

  // ---- order-sensitive cases: two patterns whose surface syntax genuinely
  // overlaps, where one must be tried before the other. Each assertion here
  // only holds if `parse`'s case order in ExprParser.scala stays as-is. ----

  test(
    "order: a top-level BinOp splits before SlotAccess's greedy left-hand base",
  ) {
    // documented directly in ExprParser.scala's BinOpSeps comment: must parse
    // as |newLength| - (|buffer|.[[ArrayBufferByteLength]]), not
    // (|newLength| - |buffer|).[[ArrayBufferByteLength]] — i.e. BinOp has to
    // be tried before SlotAccess, whose `(.+)` base would otherwise swallow
    // the operator.
    assert(
      ExprParser.parse("|newLength| - |buffer|.[[ArrayBufferByteLength]]") ==
      BinOp(
        Var("newLength"),
        BOp.Sub,
        Field(Var("buffer"), "ArrayBufferByteLength"),
      ),
    )
  }

  test(
    "order: WhichPerformsStepsClosure is tried before RelativeClauseDesc",
  ) {
    // both patterns match "a/an [=...=] which ...$" — if RelativeClauseDesc
    // won this race, the whole tail would be swallowed as an unparsed
    // Described(...) instead of being recognized as a closure definition.
    assert(
      ExprParser.parse(
        "a [=Job Abstract Closure=] which performs the following steps " +
        "when called with argument |V|:",
      ) == FollowingSteps(List("V")),
    )
  }

  test("comp-type/func arrow notation") {
    // The required "[=comp-type/func=] " prefix (see docs/spec_errors.md #18)
    // means this never collides with IndexByVar's own "base[|var|]" shape the
    // old bracket-decorated form used to risk (no trailing "]" here at all).
    // Each side parses generically -- a bare |var|, or a literal «...» list
    // (e.g. an empty results side, "« »") -- and the tag stays the raw link
    // text; NormalizeSpecTecCaseShapePass.RenamedTag resolves it to "->".
    assert(
      ExprParser.parse("[=comp-type/func=] |types| → |results|") ==
      Case("[=comp-type/func=]", List(Var("types"), Var("results"))),
    )
    assert(
      ExprParser.parse("[=comp-type/func=] |types| → « »") ==
      Case("[=comp-type/func=]", List(Var("types"), List_(Nil))),
    )
  }

  test("order: RangePrefix is tried before LinkProse") {
    // "[=the range=] |a| to |b|" also matches LinkProse's "[=link=] PROSE"
    // shape (link="[=the range=]", prose="|a| to |b|"), which would produce
    // a Link/AlgoCall instead of a Range.
    assert(
      ExprParser.parse("[=the range=] |a| to |b|") == Range(Var("a"), Var("b")),
    )
  }

  // found while regrouping ExprParser.scala into role-based sections (C-1) —
  // undocumented in the source itself before that, unlike the four above.

  test("order: PossessiveSize is tried before PossessiveAssociation") {
    // "the X's [=list/size=]" matches both: PossessiveSize doesn't require a
    // leading "the" (its base is a bare `(.+)`, which happily absorbs one),
    // while PossessiveAssociation requires one. If PossessiveAssociation won
    // this race, the result would be a plain field read (Field(_, "list/size"))
    // instead of a Length.
    assert(
      ExprParser.parse("the [=current object=]'s [=list/size=]") ==
      Length(Link("[=current object=]", Nil)),
    )
  }

  test("order: MapLiteral is tried before ListLiteral") {
    // "«[ ... ]»" also matches ListLiteral's more general "«...»" shape (its
    // non-greedy capture still stretches to include the "[ ... ]" as one
    // opaque element), which would produce a one-element List_ instead of
    // recognizing the map-literal brackets.
    assert(
      ExprParser.parse("«[ |a| → |b| ]»") ==
      Map_(List(Var("a") -> Var("b"))),
    )
  }

  test(
    "order: EmptyList/NewByteSeqOfLength are tried before the generic PlainNewExpr fallback",
  ) {
    // both start with the literal words "a new ...", which PlainNewExpr's
    // catch-all `^a\s+new\s+.+$` would otherwise also match, producing an
    // opaque UnknownNew instead of the specific node each really means.
    assert(ExprParser.parse("a new, empty list") == List_(Nil))
    assert(
      ExprParser.parse(
        "a new [=byte sequence=] of [=byte sequence/length=] equal to |n|",
      ) == NewByteSequence(Var("n")),
    )
  }
