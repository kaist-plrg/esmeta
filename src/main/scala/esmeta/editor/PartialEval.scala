package esmeta.editor

import esmeta.spec.Algorithm
import esmeta.cfg.CFG
import esmeta.editor.analyzer.AbsSemantics
import esmeta.editor.sview.*
import esmeta.ir.{Func => IRFunc}
import esmeta.editor.util.CFGHelper
import esmeta.editor.analyzer.*
import esmeta.editor.analyzer.AbsStateDomain
import esmeta.editor.analyzer.AbsValueDomain
import esmeta.cfg.*
import esmeta.ir.*
import esmeta.error.ESMetaError
import esmeta.interp.Bool
import esmeta.interp.LiteralValue
import esmeta.interp

extension (node: Node)
  def asBranch = node match {
    case node: Branch => node
    case _            => assert(false)
  }
  def asCall = node match {
    case node: Call => node
    case _          => assert(false)
  }
extension (kind: Branch.Kind)
  def isLoop = kind match { case Branch.Kind.Loop(_) => true; case _ => false }

trait IRCFGWalker[ASD <: AbsStateDomain[_] with Singleton, T <: AbsSemantics[
  ASD,
] with Singleton](val sem: T)(
  cfg: CFG,
  npMap: Map[NodePoint[Node], sem.AbsState],
) {

  def walk(name: String): IRFunc = {
    val func = cfg.fnameMap(name)
    val irFunc = func.irFunc
    walkModified(name, irFunc)
  }

  def walkModified(name: String, irFunc: IRFunc): IRFunc = {

    val func = cfg.fnameMap(name)

    // node id counter
    var nidCount: Int = 0
    def nextNId: Int = { val nid = nidCount; nidCount += 1; nid }

    // previous edges
    // var prev: List[(Node, Boolean)] = Nil
    var prevIsSingleBlock = false

    // connect previous edges
    // def connect(to: Node): Unit = {
    //  prev foreach {
    //    case (block: Block, _)       => block.next = Some(to)
    //    case (call: Call, _)         => call.next = Some(to)
    //    case (branch: Branch, true)  => branch.thenNode = Some(to)
    //    case (branch: Branch, false) => branch.elseNode = Some(to)
    //  }
    // }

    // aux
    def aux(
      inst: Inst,
      prevnp: NodePoint[Node],
      prevst: sem.AbsState.Elem,
    ): (Inst, NodePoint[Node], sem.AbsState.Elem) = { // println(s"$inst <-> $prevnp");
      inst match {
        case normal: NormalInst =>
          // val (block, currnp, prevst2) = prev match
          //  case List((b: Block, _)) => (b, prevnp, prevst)
          //  case _ =>
          //    val nid = nextNId;
          //    val cnp = NodePoint(func, cfg.nodeMap(nid), View(Nil, Nil, 0));
          //    val b = Block(nid); connect(b); (b, cnp, npMap.getOrElse(cnp, sem.AbsState.Bot))
          // block.insts += normal
          // prev = List((block, true))
          val (currnp, prevst2) = prevIsSingleBlock match
            case true => (prevnp, prevst)
            case false =>
              val nid = nextNId;
              val cnp = NodePoint(func, cfg.nodeMap(nid), View(Nil, Nil, 0));
              (cnp, npMap.getOrElse(cnp, sem.AbsState.Bot))
          prevIsSingleBlock = true
          (
            walk(inst, currnp, prevst2),
            currnp,
            sem.transfer.Helper(currnp).transfer(normal)(prevst2),
          )
        case ISeq(insts) =>
          val ninsts = List[Inst]()
          val (ninsts2, currnp, currst) =
            insts.foldLeft((ninsts, prevnp, prevst)) {
              case ((ninsts, prevnp, prevst), inst) =>
                val (i, c, s) = aux(inst, prevnp, prevst)
                (ninsts :+ i, c, s)
            }
          (walk(ISeq(ninsts2), currnp, prevst), currnp, currst)
        case inst @ IIf(cond, thenInst, elseInst) =>
          val nid = nextNId
          // val branch = Branch(nid, Branch.Kind.If, cond)
          val currnp = NodePoint(func, cfg.nodeMap(nid), View(Nil, Nil, 0))
          val bnode = currnp.node.asBranch
          assert(bnode.kind == Branch.Kind.If)

          val (tnp, fnp) = (
            NodePoint(
              func,
              cfg.nodeMap(nid) match {
                case n: Branch => n.thenNode.get
                case _         => throw ESMetaError("NP")
              },
              View(Nil, Nil, 0),
            ),
            NodePoint(
              func,
              cfg.nodeMap(nid) match {
                case n: Branch => n.elseNode.get
                case _         => throw ESMetaError("NP")
              },
              View(Nil, Nil, 0),
            ),
          )
          // connect(branch.setInst(inst))
          val (thenInst2, thenPrevisSingleBlock) = {
            // prev = List((branch, true));
            prevIsSingleBlock = false
            val (i, _, _) =
              aux(thenInst, currnp, npMap.getOrElse(tnp, sem.AbsState.Bot));
            (i, prevIsSingleBlock)
          }
          val (elseInst2, elsenp, elsest, elsePrevisSingleBlock) = {
            // prev = List((branch, false));
            prevIsSingleBlock = false
            val (i, pnp, pst) =
              aux(elseInst, currnp, npMap.getOrElse(fnp, sem.AbsState.Bot));
            (i, pnp, pst, prevIsSingleBlock)
          }
          // prev = thenPrev ++ elsePrev
          prevIsSingleBlock = false
          (
            walk(IIf(cond, thenInst2, elseInst2), currnp, prevst),
            elsenp,
            elsest,
          )
        case inst @ ILoop(kind, cond, body) =>
          val nid = nextNId
          // val branch = Branch(nid, Branch.Kind.Loop(kind), cond)
          val currnp = NodePoint(func, cfg.nodeMap(nid), View(Nil, Nil, 0))
          val bnode = currnp.node.asBranch
          assert(bnode.kind.isLoop)
          val (tnp, fnp) = (
            bnode.thenNode.map(tnode =>
              NodePoint(
                func,
                tnode,
                View(Nil, Nil, 0),
              ),
            ),
            bnode.elseNode.map(enode =>
              NodePoint(
                func,
                enode,
                View(Nil, Nil, 0),
              ),
            ),
          )
          // connect(branch.setInst(inst))
          val nbody = {
            // prev = List((branch, true));
            prevIsSingleBlock = false
            val (i, np, st) =
              aux(
                body,
                currnp,
                tnp
                  .map((tnp) => npMap.getOrElse(tnp, sem.AbsState.Bot))
                  .getOrElse(sem.AbsState.Bot),
              )
            i // connect(branch); i
          }
          // prev = List((branch, false))
          prevIsSingleBlock = false
          (
            walk(ILoop(kind, cond, nbody), currnp, prevst),
            currnp,
            fnp
              .map((fnp) => npMap.getOrElse(fnp, sem.AbsState.Bot))
              .getOrElse(sem.AbsState.Bot),
          )
        case inst @ ICall(lhs, fexpr, args) =>
          val nid = nextNId
          // val call = Call(nid, lhs, fexpr, args)
          val currnp = NodePoint(func, cfg.nodeMap(nid), View(Nil, Nil, 0))
          val bnode = currnp.node.asCall

          // connect(call.setInst(inst))
          // prev = List((call, true))
          prevIsSingleBlock = false
          val nextnp = bnode.next.map(bnode =>
            NodePoint(
              func,
              bnode,
              View(Nil, Nil, 0),
            ),
          )
          val currst = nextnp
            .map(nextnp => npMap.getOrElse(nextnp, sem.AbsState.Bot))
            .getOrElse(sem.AbsState.Bot)
          (walk(ICall(lhs, fexpr, args), currnp, prevst), currnp, currst)

      }
    }
    // body
    val entryNode = func.entry.get
    nidCount = entryNode.id
    val entryNodePoint = NodePoint(func, entryNode, View(Nil, Nil, 0))
    val (nbody, _, _) = aux(
      irFunc.body,
      entryNodePoint,
      npMap.getOrElse(entryNodePoint, sem.AbsState.Bot),
    )
    irFunc.copy(body = nbody)
  }

  def walk(
    inst: Inst,
    currnp: NodePoint[Node],
    prevst: sem.AbsState.Elem,
  ): Inst

}

class AnnotationWalker[ASD <: AbsStateDomain[
  _,
] with Singleton, T <: AbsSemantics[
  ASD,
] with Singleton](sem_ : T)(
  cfg: CFG,
  npMap: Map[NodePoint[Node], sem_.AbsState],
) extends IRCFGWalker[ASD, T](sem_)(cfg, npMap) {
  def walk(
    inst: Inst,
    currnp: NodePoint[Node],
    prevst: sem.AbsState.Elem,
  ): Inst = inst match {
    case inst: NormalInst =>
      IExpr(EBinary(BOp.Xor, EStr(inst.toString), EStr(prevst.toString)))
    case ISeq(insts) => ISeq(insts)
    case IIf(cond, thenInst, elseInst) =>
      IIf(EBinary(BOp.Xor, cond, EStr(prevst.toString)), thenInst, elseInst)
    case ILoop(kind, cond, body) =>
      ILoop(kind, EBinary(BOp.Xor, cond, EStr(prevst.toString)), body)
    case ICall(lhs, fexpr, args) =>
      ICall(lhs, EBinary(BOp.Xor, fexpr, EStr(prevst.toString)), args)
  }
}

class ReplaceExprWalker[BSD <: BasicStateDomain[
  _,
] with Singleton, T <: AbsSemantics[
  BSD,
] with Singleton](sem_ : T)(
  cfg: CFG,
  npMap: Map[NodePoint[Node], sem_.AbsState],
) extends IRCFGWalker[BSD, T](sem_)(cfg, npMap) {

  def literalToExpr(v: LiteralValue): Expr = v match
    case interp.Math(n)     => EMathVal(n)
    case interp.Number(n)   => ENumber(n)
    case interp.BigInt(n)   => EBigInt(n)
    case interp.Str(str)    => EStr(str)
    case interp.Bool(b)     => EBool(b)
    case interp.Undef       => EUndef
    case interp.Null        => ENull
    case interp.Absent      => EAbsent
    case interp.Const(name) => EConst(name)
    case interp.CodeUnit(c) => ECodeUnit(c)

  def replaceExpr(
    expr: Expr,
    currnp: NodePoint[Node],
    prevst: sem.AbsState.Elem,
  ): Expr = {
    val helper = sem.transfer.Helper(currnp)
    try {
      helper
        .transfer(expr)(prevst)
        ._1
        .getSingle(sem.AbsValue.LiteralKind) match {
        case FlatElem(sem.AbsValue.ALiteral(v)) => literalToExpr(v)
        case _                                  => expr
      }
    } catch {
      case prevst.SyntacticCalled(_, _) => expr
      case prevst.LexicalCalled(_)      => expr
    }
  }

  def markUnreachable(inst: Inst) = inst match {
    case inst: NormalInst => IExpr(EStr("unreachable"))
    case _: ISeq          => inst
    case IIf(_, thenInst, elseInst) =>
      IIf(EStr("unreachable"), thenInst, elseInst)
    case ILoop(kind, _, body) => ILoop(kind, EStr("unreachable"), body)
    case ICall(lhs, _, args)  => ICall(lhs, EStr("unreachable"), args)
  }
  override def walk(
    inst: Inst,
    currnp: NodePoint[Node],
    prevst: sem.AbsState.Elem,
  ): Inst = {
    if (!prevst.reachable) markUnreachable(inst)
    else
      inst match
        case IExpr(expr)     => IExpr(replaceExpr(expr, currnp, prevst))
        case ILet(lhs, expr) => ILet(lhs, replaceExpr(expr, currnp, prevst))
        case IAssign(ref, expr) =>
          IAssign(ref, replaceExpr(expr, currnp, prevst))
        case _: IDelete => inst
        case IPush(from, to, front) =>
          IPush(
            replaceExpr(from, currnp, prevst),
            replaceExpr(to, currnp, prevst),
            front,
          )
        case IRemoveElem(l, e) =>
          IRemoveElem(
            replaceExpr(l, currnp, prevst),
            replaceExpr(e, currnp, prevst),
          )
        case IReturn(expr) => IReturn(replaceExpr(expr, currnp, prevst))
        case IAssert(expr) => IAssert(replaceExpr(expr, currnp, prevst))
        case IPrint(expr)  => IPrint(replaceExpr(expr, currnp, prevst))
        case _: INop       => inst
        case _: ISeq       => inst
        case IIf(cond, trueInst, falseInst) =>
          IIf(replaceExpr(cond, currnp, prevst), trueInst, falseInst)
        case ILoop(kind, cond, body) =>
          ILoop(kind, replaceExpr(cond, currnp, prevst), body)
        case ICall(lhs, fexpr, args) =>
          ICall(
            lhs,
            replaceExpr(fexpr, currnp, prevst),
            args.map(replaceExpr(_, currnp, prevst)),
          )
  }
}

class InsensitiveUseTracker extends esmeta.ir.util.UnitWalker {
  val m: collection.mutable.Set[Id] = collection.mutable.Set[Id]()
  override def walk(inst: Inst): Unit = inst match {
    case IAssign(_: Id, expr) => walk(expr)
    case _                    => super.walk(inst)
  }

  override def walk(ref: Ref): Unit = ref match {
    case l: Local => m.add(l)
    case _        => super.walk(ref)
  }

  override def walk(expr: Expr): Unit = expr match {
    case EClo(_, captured) => m ++ captured
    case _                 => super.walk(expr)
  }

}

// Change CFG, so only should use at final stage
class UnreachableRemoveWalker[ESD <: EmptyStateDomain[
  _,
] with Singleton, T <: AbsSemantics[
  ESD,
] with Singleton](val absfin: T, liveVariables: Map[String, Set[Id]])(
  cfg: CFG,
  npMap: Map[NodePoint[Node], absfin.AbsState],
) extends IRCFGWalker(absfin)(cfg, npMap) {
  override def walk(
    inst: Inst,
    currnp: NodePoint[Node],
    prevst: absfin.AbsState.Elem,
  ): Inst = {
    inst match
      case ILet(id, _) if (!liveVariables(currnp.func.name)(id)) => INop()
      case IAssign(l: Local, _) if (!liveVariables(currnp.func.name)(l)) =>
        INop()
      case IIf(EBool(true), trueInst, falseInst)  => trueInst
      case IIf(EBool(false), trueInst, falseInst) => falseInst
      case IIf(EStr("unreachable"), _, _)         => INop()
      case ILoop(_, EStr("unreachable"), _)       => INop()
      case ICall(_, EStr("unreachable"), _)       => INop()
      case IExpr(EStr("unreachable"))             => INop()
      case ILoop(kind, EBool(false), body)        => body
      case ISeq(insts) =>
        ISeq(insts.filterNot {
          case INop() => true; case ISeq(Nil) => true; case _ => false
        })
      case _ => inst
  }
}

// partial evaluator for IR functions with a given syntactic view
class PartialEval(cfgHelper: CFGHelper, verbose: Boolean = false) {

  def setOfUsedVar(irFunc: IRFunc): Set[Id] = {
    def aux(inst: Inst): Set[Id] = {
      val v = InsensitiveUseTracker(); v.walk(inst); v.m.toSet
    }
    aux(irFunc.body) ++ irFunc.params.map(_.lhs)
  }

  def getReachables(view: SyntacticView): List[IRFunc] = {
    val avd: AbsValueDomain = BasicValueDomain()
    val aod = BasicObjDomain(avd)
    val asd = BasicStateDomain(aod, cfgHelper)
    val ard = RetDomain(asd)

    val absinit =
      new AbsSemantics[asd.type](ard)(
        cfgHelper,
        ignoreCond = true,
      )

    absinit.initialize(view)

    val absfin = absinit.fixpoint

    val fList = absfin.npMap.keySet
      .map(_.func.name)
      .map(cfgHelper.cfg.fnameMap(_).irFunc)
      .toList
    fList
  }

  def cg(view: SyntacticView): CallGraph = {
    val avd: AbsValueDomain = BasicValueDomain()
    val aod = BasicObjDomain(avd)
    val asd = BasicStateDomain(aod, cfgHelper)
    val ard = RetDomain(asd)

    val absinit =
      new AbsSemantics[asd.type](ard)(
        cfgHelper,
      )

    absinit.initialize(view)

    val absfin = absinit.fixpoint
    absfin.getCG
  }

  def apply(view: SyntacticView): List[IRFunc] = {
    //
    val avd: AbsValueDomain = BasicValueDomain()
    val aod = BasicObjDomain(avd)
    val asd = BasicStateDomain(aod, cfgHelper)
    val ard = RetDomain(asd)

    val absinit =
      new AbsSemantics[asd.type](ard)(
        cfgHelper,
      )

    absinit.initialize(view)

    val absfin = absinit.fixpoint

    val fList = absfin.npMap.keySet.map(_.func.name).toList
    fList.map(cfgHelper.cfg.fnameMap(_).irFunc)
  }
}
