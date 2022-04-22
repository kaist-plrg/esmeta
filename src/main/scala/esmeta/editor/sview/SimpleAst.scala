package esmeta.editor.sview

/** simplified ast */
sealed trait SimpleAst {

  /** id */
  var id: Int = -1
  def setId(i: Int): this.type = { id = i; this }

  /** index of production name */
  val nameIdx: Int

  /** index */
  val idx: Int

  /** sub-index */
  val subIdx: Int
}

/** simplified syntactic ast */
case class SimpleSyntactic(
  nameIdx: Int,
  idx: Int,
  subIdx: Int,
  children: List[SimpleAst],
) extends SimpleAst

/** simplified lexical ast */
case class SimpleLexical(nameIdx: Int, str: String) extends SimpleAst {
  val idx = 0
  val subIdx = 0
}

/** simplified abstract ast */
case class SimpleAbsSyntactic(
  nameIdx: Int,
  annotation: Annotation = AAll,
  fold: Boolean = false,
) extends SimpleAst {
  val idx = -1
  val subIdx = -1
}

/** unit walker for simplified ast */
def simpleAstUnitWalker(ast: SimpleAst, pre: SimpleAst => Unit): Unit = {
  pre(ast)
  ast match
    case syn: SimpleSyntactic =>
      for { child <- syn.children } simpleAstUnitWalker(ast, pre)
    case _ =>
}
