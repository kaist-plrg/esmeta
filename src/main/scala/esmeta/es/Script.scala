package esmeta.es

/** ECMAScript script program */
case class Script(
  code: String,
  ast: Ast,
  name: String,
  path: Option[String],
) extends ESElem
