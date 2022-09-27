package esmeta.es

import esmeta.cfg.CFG

/** ECMAScript script program */
case class Script private (
  code: String,
  ast: Ast,
  name: String,
  path: Option[String],
) extends ESElem
object Script:
  def apply(
    cfg: CFG,
    code: String,
    name: String,
    path: Option[String] = None,
  ): Script =
    val (ast, semiInjected) = cfg.scriptParser.fromWithCode(code)
    Script(semiInjected, ast, name, path)
