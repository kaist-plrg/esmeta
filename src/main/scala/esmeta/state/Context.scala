package esmeta.state

import esmeta.cfg.{Func, Block, Call}
import esmeta.ir.{Func => IRFunc, *}
import esmeta.es.Ast
import esmeta.util.BaseUtils.error
import scala.collection.mutable.{Map => MMap}

/** IR contexts */
case class Context(
  val func: Func,
  val locals: MMap[Local, Value] = MMap(),
  val sdoList: List[SdoInfo] = Nil,
) extends StateElem {

  /** current cursor in this context */
  var cursor: Cursor = NodeCursor(func, func.entry)

  /** move one instruction */
  def step: Unit = cursor match
    case cursor: NodeCursor => cursor.idx += 1
    case _                  =>

  /** move cursor to next */
  def moveNext: Unit = cursor match
    case NodeCursor(_, block: Block, _) => cursor = Cursor(block.next, func)
    case NodeCursor(_, call: Call, _)   => cursor = Cursor(call.next, func)
    case _                              => error("cursor can't move to next")

  /** return variable */
  var retVal: Option[Value] = None

  /** copy contexts */
  def copied: Context = {
    val newContext = copy(locals = MMap.from(locals))
    newContext.cursor = cursor
    newContext
  }

  /** name */
  def name: String = func.irFunc.name

  /** ast of current context */
  def astOpt: Option[Ast] =
    if (func.isSDO) Some(locals(NAME_THIS).asAst)
    else None

  def provenance: Provenance = Provenance(cursor, sdoList.headOption)
}
