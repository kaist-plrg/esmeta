package esmeta.state

import esmeta.cfg.*
import esmeta.es.*
import esmeta.ir.Name
import esmeta.spec.*
import esmeta.ty.AstSingleTy

/** syntax directed operation information */
case class SdoInfo(
  ast: Syntactic,
  func: Func,
  methodName: String,
) extends StateElem {

  /** get type information */
  def ty: AstSingleTy = AstSingleTy(ast.name, ast.idx, ast.subIdx)
}
