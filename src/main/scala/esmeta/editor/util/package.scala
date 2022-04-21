package esmeta.editor.util

import esmeta.editor.sview.*
import esmeta.js.{Ast, Syntactic => JsSyntactic, Lexical => JsLexical}
import scala.collection.mutable.{ListBuffer, Map => MMap}
import esmeta.interp.*
import esmeta.ir.*
import esmeta.cfg.CFG
import scala.annotation.tailrec

/** extension for ast */
extension (ast: Ast) {

  /** get sub-index */
  def subIdx(cfg: CFG): Int = ast match
    case _: JsLexical => 0
    case JsSyntactic(name, _, rhsIdx, children) =>
      val rhs = cfg.grammar.nameMap(name).rhsList(rhsIdx)
      val optionals = (for {
        (opt, child) <- rhs.nts.map(_.optional) zip children if opt
      } yield !child.isEmpty)
      optionals.reverse.zipWithIndex.foldLeft(0) {
        case (acc, (true, idx)) => acc + scala.math.pow(2, idx).toInt
        case (acc, _)           => acc
      }

  /** set id */
  def setId(id: Int): Int = {
    ast.idOpt = Some(id)
    var inc = 1
    ast match
      case syn: JsSyntactic =>
        for {
          childOpt <- syn.children
          child <- childOpt
        } inc += child.setId(id + inc)
      case lex: JsLexical => /* do nothing */
    inc
  }

  /** get size */
  def size: Int = ast match
    case syn: JsSyntactic =>
      1 + syn.children.foldLeft(0) {
        case (acc, Some(child)) => acc + child.size
        case (acc, None)        => acc + 1
      }
    case lex: JsLexical => 1

  /** get ast that matches concrete part of syntactic view */
  def getConcreteParts(
    sview: SyntacticView,
    annoMap: Map[Int, Set[Annotation]],
  ): List[Ast] =
    val matched: ListBuffer[Ast] = ListBuffer()
    val concretes: ListBuffer[Ast] = ListBuffer()

    @tailrec
    def aux(astList: List[Ast]): Unit = if (!astList.isEmpty) {
      var next: List[Ast] = List()
      for { ast0 <- astList } {
        if (ast0.matches(sview, annoMap)) matched += ast0
        ast0 match
          case jsSyn: JsSyntactic =>
            for {
              childOpt <- jsSyn.children
              child <- childOpt
            } next ::= child
          case _ =>
      }
      aux(next)
    }

    @tailrec
    def aux2(queue: List[(Ast, SyntacticView)]): Unit = if (!queue.isEmpty) {
      var next: List[(Ast, SyntacticView)] = List()
      for { (ast0, sview0) <- queue } (ast0, sview0) match
        case (jsLex: JsLexical, absLex: Lexical) =>
          concretes += jsLex
        case (jsSyn: JsSyntactic, absSyn: Syntactic) =>
          concretes += jsSyn
          (jsSyn.children.flatten zip absSyn.children.flatten).foreach {
            case (jsChild, absChild) => next ::= (jsChild, absChild)
          }
        case _ =>
      aux2(next)
    }

    aux(List(ast))
    aux2(matched.toList.map((_, sview)))
    concretes.toList

  /** check whether given JS ast matches syntactic view */
  def matches(
    sview: SyntacticView,
    annoMap: Map[Int, Set[Annotation]],
  ): Boolean = (ast, sview) match
    case (_, AbsSyntactic(absName, anno, _)) =>
      ast.name == absName &&
      ast.idOpt
        .map(id => annoMap.getOrElse(id, Set()))
        .fold(true)(annoSet =>
          annoSet.isEmpty || annoSet.exists(_.subType(anno)),
        )
    case (jsLex: JsLexical, absLex: Lexical) =>
      jsLex.name == absLex.name &&
      jsLex.str.trim == absLex.str.trim
    case (jsSyn: JsSyntactic, absSyn: Syntactic) =>
      jsSyn.name == absSyn.name &&
      jsSyn.rhsIdx == absSyn.rhsIdx &&
      (jsSyn.children zip absSyn.children).forall {
        case (None, None) => true
        case (Some(jsChild), Some(absChild)) =>
          jsChild.matches(absChild, annoMap)
        case _ => false
      }
    case _ => false

  /** check whether given JS ast contains sytactic view */
  def contains(
    sview: SyntacticView,
    annoMap: Map[Int, Set[Annotation]] = Map(),
  ): Boolean =
    if (ast.matches(sview, annoMap)) true
    else
      ast match
        case jsSyn: JsSyntactic =>
          jsSyn.children.foldLeft(false) {
            case (true, _)            => true
            case (false, None)        => false
            case (false, Some(child)) => child contains sview
          }
        case _ => false
}

/** extension for annotation */
extension (anno: Annotation) {
  def subType(anno0: Annotation): Boolean = (anno, anno0) match
    case (_, AAll) => true
    case _         => anno == anno0
}

/** extension for values */
extension (value: Value) {
  def toAnnotation(st: State): Option[Annotation] = value match
    case NormalComp(v0)                       => v0.toAnnotation(st)
    case comp: Comp if comp.ty == CONST_THROW => Some(AThrow)
    case _: Comp                              => None // ignore return, break
    case _: Number                            => Some(ANum)
    case _: BigInt                            => Some(ABigInt)
    case _: Str                               => Some(AStr)
    case _: Bool                              => Some(ABool)
    case Undef                                => Some(AUndef)
    case Null                                 => Some(ANull)
    case addr: Addr =>
      st(addr) match
        case m: MapObj if st.cfg.typeModel.subType(m.ty, "Object") => Some(AObj)
        case m: MapObj if m.ty == "ReferenceRecord"                =>
          // GetValue result
          val newSt = st.copied
          newSt.context =
            Context(st.cfg.fnameMap("GetValue"), MMap(Name("V") -> value))
          newSt.callStack = Nil
          Interp(newSt)
          newSt(GLOBAL_RESULT).toAnnotation(newSt)
        case s: SymbolObj => Some(ASymbol)
        case _            => ???
    case CONST_EMPTY => None
    case _: Const    => ???
    case _: Clo      => ???
    case _: Cont     => ???
    case _: AstValue => ???
    case _: Grammar  => ???
    case _: Math     => ???
    case Absent      => ???
    case _: CodeUnit => ???
}
