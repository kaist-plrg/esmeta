package esmeta.es.util

import esmeta.LINE_SEP
import esmeta.spec.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*
import esmeta.es.*
import esmeta.es.util.injector.Injector
import esmeta.state.*

/** stringifier for ECMAScript */
class Stringifier(
  detail: Boolean,
  location: Boolean,
  grammar: Option[Grammar],
) {
  // elements
  given elemRule: Rule[ESElem] = (app, elem) =>
    elem match
      case elem: Script      => scriptRelu(app, elem)
      case elem: Ast         => astRule(app, elem)
      case elem: ConformTest => testRule(app, elem)
      case elem: Assertion   => assertRule(app, elem)

  /** ECMAScript script program */
  given scriptRelu: Rule[Script] = (app, script) => app >> script.ast

  // abstract syntax tree (AST) values
  given astRule: Rule[Ast] = (app, ast) =>
    grammar match
      case Some(grammar) => grammarAstRule(app, (grammar, ast))
      case None          => basicAstRule(app, ast)

  // span information
  given locRule: Rule[Loc] = (app, loc) =>
    app >> "{" >> loc.start.toString >> "-" >> loc.end.toString >> "}"

  lazy val grammarAstRule: Rule[(Grammar, Ast)] = (app, pair) =>
    val (grammar, origAst) = pair
    val nameMap = grammar.nameMap

    def aux(ast: Ast): Unit = ast match
      case Lexical(name, str) => app >> str >> " "
      case Syntactic(name, args, rhsIdx, children) =>
        var cs = children
        for (symbol <- nameMap(name).rhsList(rhsIdx).symbols) symbol match
          case Terminal(term)                          => app >> term >> " "
          case Empty | NoLineTerminator | _: Lookahead =>
          case _ =>
            cs match
              case hd :: tl => hd.map(aux); cs = tl
              case _        => error(s"invalid AST: $origAst")

    aux(origAst)
    app

  lazy val basicAstRule: Rule[Ast] = (app, ast) =>
    ast match
      case Syntactic(name, args, rhsIdx, children) =>
        given Rule[Boolean] = (app, bool) => app >> (if (bool) "T" else "F")

        given Rule[List[Boolean]] = iterableRule()

        app >> "|" >> name >> "|"
        if (args.nonEmpty) app >> "[" >> args >> "]"
        app >> "<" >> rhsIdx >> ">"
        if (detail && ast.loc.isDefined) app >> ast.loc.get

        given Rule[Option[Ast]] = optionRule("<none>")

        if (detail) app.wrap("(", ")")(children.map(app :> _ >> ",")) else app
      case Lexical(name, str) =>
        app >> "|" >> name >> "|(" >> str >> ")"
        if (detail && ast.loc.isDefined) app >> ast.loc.get else app

  // conformance tests
  given testRule: Rule[ConformTest] = (app, test) =>
    val ConformTest(id, script, exitTag, defs, isAsync, assertions) = test
    app >> "// [EXIT] " >> exitTag.toString
    if (defs) app >> Injector.assertions >> LINE_SEP
    if (isAsync) app :> "$delay(() => {"
    app :> script
    if (isAsync) app :> "});"
    assertions.foreach(app :> _)
    app

  // assertions
  given assertRule: Rule[Assertion] = (app, assert) =>
    given Rule[SimpleValue] = (app, value) =>
      app >> (
        value match
          case Number(n) => n.toString
          case v         => v.toString
      )

    assert match
      case HasValue(x, v) => app :> s"$$assert.sameValue($x, " >> v >> ");"
      case IsExtensible(addr, path, callable) =>
        app >> s"$$assert.sameValue(Object.isExtensible($path), $callable);"
      case IsCallable(addr, path, callable) =>
        app >> (if callable then s"$$assert.callable($path);"
                else s"$$assert.notCallable($path);")
      case IsConstructable(addr, path, constructable) =>
        app >> (if constructable then s"$$assert.constructable($path);"
                else s"$$assert.notConstructable($path);")
      case CompareArray(addr, path, array) =>
        app >> s"$$assert.compareArray(Reflect.ownKeys($path), ${array
          .mkString("[", ", ", "]")}, $path);"
      case SameObject(addr, path, origPath) =>
        app >> s"$$assert.sameValue($path, $origPath);"
      case VerifyProperty(addr, path, prop, desc) =>
        prop match
          case sv: SimpleValue =>
            app >> s"$$verifyProperty($path, " >> sv >> s", $desc);"
          case addr: Addr =>
            val PREFIX_INTRINSIC = "INTRINSICS."
            addr match
              case _ @NamedAddr(name) if name.startsWith(PREFIX_INTRINSIC) =>
                app >> s"$$verifyProperty($path, ${name.substring(PREFIX_INTRINSIC.length)}, $desc);"
              case _ => app
          case _ => app
}
