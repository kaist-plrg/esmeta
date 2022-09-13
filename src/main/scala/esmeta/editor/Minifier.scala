package esmeta.editor

import esmeta.cfg.*
import esmeta.ir.{Func => IRFunc, _}
import esmeta.editor.sview.SyntacticView
import esmeta.editor.util.*
import scala.collection.mutable.{Map => MMap, Set => MSet, ListBuffer}
import esmeta.ir.util.UnitWalker
import esmeta.ir.util.Walker
import esmeta.util.BasicWalker

//import esmeta.util.BaseUtils.cached

class Minifier(func: Func) {

  sealed trait Singleton {
    def join(that: Singleton): Singleton = (this, that) match {
      case (_, Top)                   => Top
      case (Top, _)                   => Top
      case (x, Bot)                   => x
      case (Bot, x)                   => x
      case (ExprElem(a), ExprElem(b)) => if a == b then ExprElem(a) else Top
      case (CallElem(a), CallElem(b)) => if a.eq(b) then CallElem(a) else Top
      case _                          => Top
    }
  }
  case object Bot extends Singleton
  case class ExprElem(e: Expr) extends Singleton
  case class CallElem(c: Call) extends Singleton
  case object Top extends Singleton

  case class IdInfo(
    val mustPointTo: Singleton = Bot,
    var usedAt: List[Node] = List(),
  ) {
    def join(that: IdInfo) = {
      IdInfo(
        this.mustPointTo.join(that.mustPointTo),
        this.usedAt ++ that.usedAt,
      )
    }
  }

  val idMap: MMap[Id, IdInfo] = MMap()
  def isPure(e: Expr): Boolean = e match {
    case _: LiteralExpr => true
    case _: AstExpr     => true
    case _: ERef        => true
    case _              => false
  }
  def isValue(e: Expr): Boolean = e match {
    case _: LiteralExpr => true
    case _: AstExpr     => true
    case _              => false
  }

  class Collector extends UnitWalker {
    // idMap modifiers
    def assign(lhs: Id, expr: Expr) = {
      idMap.updateWith(lhs)(infoOpt =>
        Some(
          infoOpt
            .getOrElse(IdInfo())
            .join(IdInfo(ExprElem(expr))),
        ),
      )
    }
    def assign(lhs: Id, call: Call) = {
      idMap.updateWith(lhs)(infoOpt =>
        Some(
          infoOpt
            .getOrElse(IdInfo())
            .join(IdInfo(CallElem(call))),
        ),
      )
    }
    def assignTop(lhs: Ref): Unit = lhs match {
      case id: Id           => assignTop(id)
      case Prop(base, prop) => assignTop(base)
    }
    def assignTop(lhs: Id) = {
      idMap.updateWith(lhs)(infoOpt =>
        Some(
          infoOpt
            .getOrElse(IdInfo())
            .join(IdInfo(Top)),
        ),
      )
    }

    var curNode: Option[Node] = None;

    def use(ref: Ref): Unit = ref match {
      case id: Id        => use(id)
      case Prop(base, _) => use(base)
    }
    def use(ref: Id) = {
      idMap.updateWith(ref)(infoOpt =>
        Some(
          infoOpt
            .getOrElse(IdInfo())
            .join(IdInfo(Bot, curNode.toList)),
        ),
      )
    }

    override def walk(inst: Inst): Unit = inst match {
      case ILet(lhs, expr)          => assign(lhs, expr); walk(expr)
      case IAssign(ref: Id, expr)   => assign(ref, expr); walk(expr)
      case IAssign(ref: Prop, expr) => assignTop(ref); walk(ref); walk(expr)
      case _                        => super.walk(inst)
    }

    override def walk(expr: Expr): Unit = expr match {
      case ERef(ref) => use(ref); walk(ref)
      case _         => super.walk(expr)
    }

    val visited: MSet[Node] = MSet()
    def walk(node: Node): Unit = {
      if (visited contains node)
        return
      visited += node

      curNode = Some(node)
      node match {
        case b @ Block(id, insts, next) => insts.map(walk); walkOpt(next, walk)
        case c @ Call(id, lhs, fexpr, args, next) =>
          assign(lhs, c)
          walk(fexpr); walkList(args, walk); walkOpt(next, walk)
        case b @ Branch(id, kind, cond, thenNode, elseNode) =>
          walk(cond); walkOpt(thenNode, walk); walkOpt(elseNode, walk)
      }
    }

  }

  class Substituter extends Walker {
    var updated = false

    def walk(inst: NormalInst): NormalInst =
      walk(inst.asInstanceOf[Inst]).asInstanceOf[NormalInst]

    override def walk(expr: Expr): Expr = expr match {
      case ERef(x: Id) =>
        val info = idMap(x)
        info.mustPointTo match {
          case ExprElem(e) if isPure(e)               => e
          case ExprElem(e) if info.usedAt.length == 1 => e // TODO: ORDER?
          case _                                      => super.walk(expr)
        }
      case ERef(Prop(id: Id, EMathVal(i))) =>
        idMap(id).mustPointTo match {
          case ExprElem(ESyntactic(_, _, _, children)) =>
            children(i.toInt).getOrElse(EAbsent)
          case _ => super.walk(expr)
        }
      case ETypeCheck(e, ty) =>
        (walk(e), walk(ty)) match {
          case (ast: AstExpr, EStr(t)) =>
            EBool(
              SyntacticView(ast).chains.exists(_.name == t),
            )
          case (newE, newTy) => ETypeCheck(newE, newTy)
        }
      case EUnary(UOp.Not, e) =>
        walk(e) match {
          case EBool(b) => EBool(!b)
          case e        => EUnary(UOp.Not, e)
        }
      case EBinary(BOp.And, e1, e2) =>
        walk(e1) match {
          case EBool(false) => EBool(false)
          case EBool(true)  => walk(e2)
          case e            => EBinary(BOp.And, e, walk(e2))
        }
      case EBinary(BOp.Or, e1, e2) =>
        walk(e1) match {
          case EBool(true)  => EBool(true)
          case EBool(false) => walk(e2)
          case e            => EBinary(BOp.Or, e, walk(e2))
        }
      case EBinary(BOp.Eq, e1, e2) =>
        (walk(e1), walk(e2)) match {
          case (e1: LiteralExpr, e2: LiteralExpr) => EBool(e1 == e2)
          case (e1, e2)                           => EBinary(BOp.Eq, e1, e2)
        }
      case EReturnIfAbrupt(e: Expr, check) =>
        walk(e) match {
          case EReturnIfAbrupt(e, _) => e // idempotency
          case _ if isValue(e)       => e
          case _                     => super.walk(expr)
        }
      case _ => super.walk(expr)
    }

    val GET_VALUE = EClo("GetValue", List())

    def walk(block: Block): Node =
      val Block(id, insts, next) = block
      Block(id, insts.map(walk), walkOpt(next, walk))

    def walk(call: Call): Node =
      val Call(id, lhs, fexpr, args, next) = call
      ((fexpr, args) match {
        case (GET_VALUE, List(arg)) =>
          arg match {
            // GetValue first calls reutrn if abrupt of the argument
            case EReturnIfAbrupt(e, _) => Some(("call", e))
            case e if isValue(e)       => Some(("block", e))
            case e @ ERef(x: Id) =>
              val info = idMap(x)
              info.mustPointTo match {
                case CallElem(c) if c.fexpr == GET_VALUE =>
                  Some(("block", c.args(0)))
                case _ => None
              }
            case _ => None
          }
        case _ => None
      })
        .map((kind, e) =>
          if (kind == "block") then
            Block(id, ListBuffer(IAssign(walk(lhs), e)), walkOpt(next, walk))
          else
            Call(
              id,
              walk(lhs),
              GET_VALUE,
              List(walk(e)),
              walkOpt(next, walk),
            ),
        )
        .getOrElse(
          Call(
            id,
            walk(lhs),
            walk(fexpr),
            walkList(args, walk),
            walkOpt(next, walk),
          ),
        )

    def walk(branch: Branch): Node =
      val Branch(id, kind, cond, thenNode, elseNode) = branch
      Branch(
        id,
        kind,
        walk(cond),
        walkOpt(thenNode, walk),
        walkOpt(elseNode, walk),
      )

    val visited: MMap[Node, Node] = MMap()
    def walk(node: Node): Node = {
      val ret = visited.getOrElseUpdate(
        node,
        node match {
          case b: Block  => walk(b)
          case c: Call   => walk(c)
          case b: Branch => walk(b)
        },
      )
      if (ret.toString != node.toString)
        updated = true
      ret
    }
  }

  class Remover extends BasicWalker {
    var updated = false

    def getId(ref: Ref): Id = ref match {
      case id: Id          => id
      case Prop(ref, prop) => getId(ref)
    }

    def isDead(inst: Inst): Boolean = inst match {
      case ILet(lhs, expr)         => idMap(lhs).usedAt.isEmpty
      case IAssign(ref: Ref, expr) => idMap(getId(ref)).usedAt.isEmpty
      case IAssert(EBool(true))    => true
      case _: INop                 => true
      case _                       => false
    }

    val visited: MMap[Node, Option[Node]] = MMap()
    def walk(node: Node): Option[Node] = {
      val ret = visited.getOrElseUpdate(
        node,
        node match {
          case Block(id, insts, next) =>
            val newInsts = insts.filterNot(isDead)
            if (newInsts.isEmpty) then next.flatMap(walk)
            else Some(Block(id, newInsts, next.flatMap(walk)))
          case Call(id, lhs, fexpr, args, next) =>
            if idMap(lhs).usedAt.isEmpty then next.flatMap(walk)
            else Some(Call(id, lhs, fexpr, args, next.flatMap(walk)))
          case Branch(id, kind, cond, thenNode, elseNode) =>
            cond match {
              case EBool(true)  => thenNode.flatMap(walk)
              case EBool(false) => elseNode.flatMap(walk)
              case _ =>
                Some(
                  Branch(
                    id,
                    kind,
                    cond,
                    thenNode.flatMap(walk),
                    elseNode.flatMap(walk),
                  ),
                )
            }
        },
      )
      ret match {
        case None      => updated = true
        case Some(ret) => if (ret.toString != node.toString) updated = true
      }
      ret
    }
  }

  def resolve(): Unit = {

    // TODO: O(N^2) time complexity)
    def resolve(id: Id, single: Singleton): (Id, Singleton) = single match {
      case ExprElem(e) =>
        e match {
          case ERef(id: Id) =>
            val (tmpId, tmpSingle) = resolve(id, idMap(id).mustPointTo)
            (
              tmpId,
              tmpSingle match {
                case ExprElem(e) if isPure(e) => tmpSingle
                case _                        => ExprElem(ERef(tmpId))
              },
            )
          case _ if isPure(e) => (id, ExprElem(e))
          case _              => (id, single)
        }
      case _ => (id, single)
    }

    idMap.mapValuesInPlace((id, info) =>
      IdInfo(resolve(id, info.mustPointTo)._2, info.usedAt),
    )
  }

  def doMinify(n: Node): Node = {
    var curN = n
    var updated = true

    while (updated) {
      updated = false

      val collector = new Collector()
      idMap.clear()
      collector.walk(curN)

      resolve()

      val substituter = new Substituter()
      val subN = substituter.walk(curN)
      updated ||= substituter.updated

      val collector2 = new Collector()
      idMap.clear()
      collector2.walk(subN)

      val remover = new Remover()
      val newN = remover.walk(subN).getOrElse(Block(0, ListBuffer(), None))

      updated ||= remover.updated

      curN = newN
    }

    curN
  }

  def apply(): Func =
    val Func(id, irFunc, node) = func
    Func(id, irFunc, node.map(doMinify))
}
