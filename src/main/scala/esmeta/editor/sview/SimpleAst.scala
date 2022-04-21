package esmeta.editor.sview

/** simplified ast */
sealed trait SimpleAst {

  /** id */
  val id: Int

  /** index of production name */
  val nameIdx: Int

  /** index */
  val idx: Int

  /** sub-index */
  val subIdx: Int
}

/** simplified syntactic ast */
case class SimpleSyntactic(
  id: Int,
  nameIdx: Int,
  idx: Int,
  subIdx: Int,
  children: List[SimpleAst],
) extends SimpleAst

/** simplified lexical ast */
case class SimpleLexical(id: Int, nameIdx: Int, str: String) extends SimpleAst {
  val idx = 0
  val subIdx = 0
}

/** unit walker for simplified ast */
def simpleAstUnitWalker(ast: SimpleAst, pre: SimpleAst => Unit): Unit = {
  pre(ast)
  ast match
    case syn: SimpleSyntactic =>
      for { child <- syn.children } simpleAstUnitWalker(ast, pre)
    case _ =>
}
