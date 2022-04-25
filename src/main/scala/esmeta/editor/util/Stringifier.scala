package esmeta.editor.util

import esmeta.LINE_SEP
import esmeta.spec.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*
import esmeta.editor.*
import esmeta.editor.sview.*

/** stringifier for editor */
class Stringifier(
  detail: Boolean,
  location: Boolean,
  grammar: Option[Grammar],
) {
  // Editor elements
  given elemRule: Rule[EditorElem] = (app, elem) =>
    elem match {
      case view: SyntacticView => syntacticViewRule(app, view)
      case ast: SimpleAst      => simpleAstRule(app, ast)
    }

  // TODO syntactic views
  given syntacticViewRule: Rule[SyntacticView] = (app, view) => {
    grammar match
      case Some(grammar) => grammarSyntacticViewRule(app, (grammar, view))
      case None          => basicSyntacticViewRule(app, view)
  }

  lazy val grammarSyntacticViewRule: Rule[(Grammar, SyntacticView)] =
    (app, pair) =>
      val (grammar, origView) = pair
      val nameMap = grammar.nameMap
      def aux(view: SyntacticView): Unit = view match
        case Lexical(name, str) => app >> str >> " "
        case AbsSyntactic(name, identifier, annotation, isFold) =>
          app >> (if (isFold) "#e" else (s"#$name")) >> (identifier match
            case None        => ""
            case Some(ident) => s"/$ident"
          ) >> (annotation match
            case AAll    => ""
            case ABool   => "/:Bool"
            case AStr    => "/:String"
            case ANum    => "/:Number"
            case ABigInt => "/:BigInt"
            case AObj    => "/:Object"
            case ASymbol => "/:Symbol"
            case AThrow  => "/:Throw"
            case AUndef  => "/:Undef"
            case ANull   => "/:Null"
          ) >> "# "
        case Syntactic(name, args, rhsIdx, children) =>
          var cs = children
          for (symbol <- nameMap(name).rhsList(rhsIdx).symbols) symbol match
            case Terminal(term)                          => app >> term >> " "
            case Empty | NoLineTerminator | _: Lookahead =>
            case _ =>
              cs match
                case hd :: tl => hd.map(aux); cs = tl
                case _        => error(s"invalid SyntacticView: $origView")
      aux(origView)
      app

  lazy val basicSyntacticViewRule: Rule[SyntacticView] = (app, view) =>
    view match
      case Syntactic(name, args, rhsIdx, children) =>
        given Rule[Boolean] = (app, bool) => app >> (if (bool) "T" else "F")
        given Rule[List[Boolean]] = iterableRule()
        app >> "|" >> name >> "|"
        if (!args.isEmpty) app >> "[" >> args >> "]"
        app >> "<" >> rhsIdx >> ">"
        given Rule[Option[SyntacticView]] = optionRule("<none>")
        if (detail) app.wrap("(", ")")(children.map(app :> _ >> ",")) else app
      case Lexical(name, str) =>
        app >> "|" >> name >> "|(" >> str >> ")"
      case AbsSyntactic(name, _, annotation, _) =>
        app >> "#" >> name

  // abstract syntax tree (AST) values
  given simpleAstRule: Rule[SimpleAst] = (app, ast) =>
    (grammar, detail) match
      case (_, true) | (None, false) => basicSimpleAstRule(app, (grammar, ast))
      case (Some(grammar), false) => grammarSimpleAstRule(app, (grammar, ast))

  lazy val grammarSimpleAstRule: Rule[(Grammar, SimpleAst)] = (app, pair) =>
    val (grammar, origAst) = pair
    val nameMap = grammar.nameMap
    def aux(ast: SimpleAst): Unit = ast match
      case SimpleLexical(_, str) => app >> str >> " "
      case SimpleSyntactic(nameIdx, rhsIdx, subIdx, children) =>
        val name = grammar.names(nameIdx)
        var cs = children
        for (symbol <- nameMap(name).rhsList(rhsIdx).symbols) symbol match
          case Terminal(term)                          => app >> term >> " "
          case Empty | NoLineTerminator | _: Lookahead =>
          case _ =>
            cs match
              case hd :: tl => aux(hd); cs = tl
              case _        => error(s"invalid AST: $origAst")
      case SimpleAbsSyntactic(nameIdx, _, _) =>
        app >> "#" >> grammar.names(nameIdx) >> "# "
    aux(origAst)
    app

  lazy val basicSimpleAstRule: Rule[(Option[Grammar], SimpleAst)] =
    (app, pair) =>
      val (grammar, ast) = pair
      grammar match
        case Some(grammar) =>
          app >> "[#" >> ast.id >> "]"
          val name = grammar.names(ast.nameIdx)
          ast match
            case SimpleSyntactic(nameIdx, rhsIdx, subIdx, children) =>
              app >> "|" >> name >> "|"
              app >> "<" >> rhsIdx >> "," >> subIdx >> ">"
              if (detail) app.wrap("(", ")")(children.map(app :> _ >> ","))
              else app
            case SimpleLexical(nameIdx, str) =>
              app >> "|" >> name >> "|(" >> str >> ")"
            case SimpleAbsSyntactic(nameIdx, _, _) =>
              app >> "|#" >> name >> "#|"
        case None =>
          app >> "[#" >> ast.id >> "]"
          ast match
            case SimpleSyntactic(nameIdx, rhsIdx, subIdx, children) =>
              app >> "|" >> nameIdx >> "|"
              app >> "<" >> rhsIdx >> "," >> subIdx >> ">"
              if (detail) app.wrap("(", ")")(children.map(app :> _ >> ","))
              else app
            case SimpleLexical(nameIdx, str) =>
              app >> "|" >> nameIdx >> "|(" >> str >> ")"
            case SimpleAbsSyntactic(nameIdx, _, _) =>
              app >> "|#" >> nameIdx >> "#|"

}
