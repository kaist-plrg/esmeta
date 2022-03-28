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
        case AbsSyntactic(name) => app >> "#" >> name >> " "
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
      case AbsSyntactic(name) =>
        app >> "#" >> name
}
