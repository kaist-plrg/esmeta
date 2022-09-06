package esmeta.editor

import esmeta.cfg.*
import esmeta.editor.sview.*
import esmeta.editor.util.*
import esmeta.ir.{Func => IRFunc, _}
import esmeta.ir.util.Walker
import scala.collection.mutable.{ListBuffer, Map => MMap}
import esmeta.util.BaseUtils.cached

class Inliner(cfg: CFG, view: SyntacticView) {
  var _nid: Int = 10000
  def nid: Int =
    _nid = _nid + 1
    _nid - 1

  var _fid: Int = 0
  def fid: Int =
    _fid = _fid + 1
    _fid - 1

  var _tid: Int = 0
  def tid: Int =
    _tid = _tid + 1
    _tid - 1

  // TODO: make this into whitelist instead?
  val blackList = List(
    "GetValue",
    "PutValue",
    "EvaluatePropertyAccessWithIdentifierKey",
    "ResolveBinding",
    "AbstractEqualityComparison",
    "OrdinaryObjectCreate",
    "ToPropertyKey",
    "ToBoolean",
    "CreateDataPropertyOrThrow",
  )

  val init: Node = Call(
    nid,
    Name("result"),
    ERef(Prop(Name("expr"), EStr("Evaluation"))),
    List(ERef(Name("expr"))),
    None,
  )

  val cfgHelper = CFGHelper(cfg)

  type AstMap = Map[Id, ESyntactic]

  class Renamer(suffix: Int, lhs: Id) extends Walker {
    val tempMap: MMap[Int, Int] = MMap()

    def walk(inst: NormalInst): NormalInst =
      walk(inst.asInstanceOf[Inst]).asInstanceOf[NormalInst]

    def walk(block: Block): Block = block match
      case Block(id, insts, next) => Block(id, insts.map(walk), next)

    def walk(call: Call): Call = call match
      case Call(id, lhs, fexpr, args, next) =>
        Call(id, walk(lhs), walk(fexpr), walkList(args, walk), next)

    def walk(branch: Branch): Branch = branch match
      case Branch(id, kind, cond, thenNode, elseNode) =>
        Branch(id, kind, walk(cond), thenNode, elseNode)

    override def walk(x: Id): Id = x match {
      case Name("_lhs") => lhs
      case Global(x)    => Global(walk(x))
      case x: Name      => walk(x)
      case Temp(k)      => Temp(tempMap.getOrElseUpdate(k, tid))
    }

    override def walk(x: Name): Name = Name(x.name + suffix)
  }

  val doInline = cached[(Node, AstMap), Node] {
    case (n, astMap) =>
      n match {
        case b: Block  => doInlineBlock(b, astMap)
        case c: Call   => doInlineCall(c, astMap)
        case b: Branch => doInlineBranch(b, astMap)
      }
  }

  /*
  def doInline(n: Node, astMap: AstMap): Node = n match {
    case b: Block => doInline(b, astMap)
    case c: Call => doInline(c, astMap)
    case b: Branch => doInline(b, astMap)
  }
   */

  def doInlineBlock(b: Block, astMap: AstMap): Node =
    val Block(_, insts, next) = b
    val newMap = insts.foldLeft(astMap)((m, i) =>
      i match {
        case IAssign(id: Id, e: Expr) =>
          resolveView(e, m).map(e => m + (id -> e)).getOrElse(m)
        case ILet(n: Name, e: Expr) =>
          resolveView(e, m).map(e => m + (n -> e)).getOrElse(m)
        case _ => m
      },
    )
    Block(nid, insts, next.map(doInline(_, newMap)))

  def resolveView(expr: Expr, astMap: AstMap): Option[ESyntactic] = expr match {
    case e: ESyntactic => Some(e)
    case ERef(ref)     => resolveView(ref, astMap)
    case EParse(
          code,
          EGrammar("ParenthesizedExpression", _),
        ) => // TODO: More generally?
      resolveView(code, astMap).flatMap(t =>
        t match {
          case ESyntactic(
                "CoverParenthesizedExpressionAndArrowParameterList",
                args,
                0,
                children,
              ) =>
            Some(ESyntactic("ParenthesizedExpression", args, 0, children))
          case ESyntactic("ParenthesizedExpression", _, _, _) => Some(t)
          case _                                              => None
        },
      )
    case _ => None
  }

  def resolveView(ref: Ref, astMap: AstMap): Option[ESyntactic] = ref match {
    case x: Id => astMap.get(x)
    case Prop(Name(x), EMathVal(i)) =>
      astMap
        .get(Name(x))
        .flatMap(tree =>
          val child = tree.children(i.toInt).get
          child match {
            case child: ESyntactic => Some(child)
            case _                 => None
          },
        )
    case _ => None
  }

  trait ResolvedFunc
  case class Sdo(func: Func, ast: AstExpr) extends ResolvedFunc
  case class AbsSdo(ast: AstExpr) extends ResolvedFunc
  case class Normal(func: Func) extends ResolvedFunc
  case object NoFunc extends ResolvedFunc

  def resolveFunc(f: Expr, astMap: AstMap): ResolvedFunc = f match {
    // SDO
    case ERef(Prop(ref, EStr(name))) =>
      resolveView(ref, astMap)
        .flatMap(ast => {
          if ast.rhsIdx < 0 then Some(AbsSdo(ast))
          else
            cfgHelper
              .getSDOView(SyntacticView(ast), name)
              .map((v, f) => Sdo(f, v.toAstExpr))
        })
        .getOrElse(NoFunc)
    // Normal Function
    case EClo(name, captured) => // TODO: handle captured
      if blackList.contains(name) then NoFunc
      else cfg.fnameMap.get(name).map(Normal(_)).getOrElse(NoFunc)
    case _ => NoFunc
  }

  def doInlineCall(c: Call, astMap: AstMap): Node =
    val Call(_, lhs, f, args, next) = c
    lazy val inlinedNext = next.map(n => doInline(n, astMap))

    resolveFunc(f, astMap) match {
      case NoFunc => Call(nid, lhs, f, args, inlinedNext)
      case Sdo(func, newView) =>
        inlineFunc(lhs, func, List(newView), next, astMap)
      case AbsSdo(newView) =>
        Call(
          nid,
          lhs,
          EStr(s"Evaluation of ${newView.name}"),
          List(),
          inlinedNext,
        )
      case Normal(func) =>
        inlineFunc(lhs, func, args, next, astMap)
    }

  // Convert a function to inlined instructions
  def inlineFunc(
    lhs: Id,
    func: Func,
    args: List[Expr],
    origNext: Option[Node],
    astMap: AstMap,
  ): Node =
    val suffix = fid
    val paramInitInsts: ListBuffer[NormalInst] =
      ListBuffer() ++ func.irFunc.params
        .zip(args)
        .map({
          case (p, a) => IAssign(Name(p.lhs.name + suffix), a)
        })

    val entry = func.entry
    val nodeMap = MMap[Int, Node]()

    val renamer = Renamer(suffix, lhs)

    // TODO: Infinite recursion for loop?
    def rewrite(node: Node): Node = node match {
      case Block(id, insts, next) => {
        nodeMap.getOrElseUpdate(
          id,
          renamer.walk(insts.last match {
            case IReturn(e) =>
              val newInsts =
                insts.clone.dropRightInPlace(1).append(IAssign(Name("_lhs"), e))
              Block(id, newInsts, origNext)
            case _ =>
              Block(id, insts, next.map(rewrite))
          }),
        )
      }
      case Call(id, lhs, fexpr, args, next) => {
        nodeMap.getOrElse(
          id,
          renamer.walk(Call(id, lhs, fexpr, args, next.map(rewrite))),
        )
      }
      case Branch(id, kind, cond, thenNode, elseNode) => {
        nodeMap.getOrElse(
          id,
          renamer.walk(
            Branch(id, kind, cond, thenNode.map(rewrite), elseNode.map(rewrite)),
          ),
        )
      }
    }

    doInline(Block(nid, paramInitInsts, entry.map(rewrite)), astMap)

  def doInlineBranch(b: Branch, astMap: AstMap): Node =
    val Branch(_, kind, cond, thenNode, elseNode) = b
    lazy val empty = Block(nid, ListBuffer(), None)
    cond match {
      case EBool(true)  => thenNode.map(doInline(_, astMap)).getOrElse(empty)
      case EBool(false) => elseNode.map(doInline(_, astMap)).getOrElse(empty)
      case _ =>
        Branch(
          nid,
          kind,
          cond,
          thenNode.map(doInline(_, astMap)),
          elseNode.map(doInline(_, astMap)),
        )
    }

  def pprint(
    obj: Any,
    depth: Int = 0,
    paramName: Option[String] = None,
  ): Unit = {

    val indent = "  " * depth
    val prettyName = paramName.fold("")(x => s"$x: ")
    val ptype = obj match {
      case _: Iterable[Any] => ""
      case obj: Product     => obj.productPrefix
      case _                => obj.toString
    }

    println(s"$indent$prettyName$ptype")

    obj match {
      case seq: Iterable[Any] =>
        seq.foreach(pprint(_, depth + 1))
      case obj: Product =>
        (obj.productIterator zip obj.productElementNames)
          .foreach {
            case (subObj, paramName) =>
              pprint(subObj, depth + 1, Some(paramName))
          }
      case _ =>
    }
  }

  def apply(): Func =
    val expr = view.toAstExpr.asInstanceOf[ESyntactic]
    val dummy = IRFunc(
      false,
      IRFunc.Kind.AbsOp,
      "inlinedEvaluation",
      List(),
      AnyType,
      INop(),
      None,
    )

    Func(0, dummy, Some(doInline(init, Map(Name("expr") -> expr))))
}
