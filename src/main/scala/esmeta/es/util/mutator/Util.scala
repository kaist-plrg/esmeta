package esmeta.es.util.mutator

import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.Coverage.*
import esmeta.spec.Grammar
import esmeta.util.*

object Util {
  class AstCounter(pred: Syntactic => Boolean) extends UnitWalker {
    def apply(ast: Ast): Int = {
      _cnt = 0
      walk(ast)
      _cnt
    }
    private var _cnt = 0

    override def walk(ast: Syntactic): Unit = {
      if pred(ast) then _cnt += 1
      super.walk(ast)
    }
  }

  trait ListWalker {
    def walkOpt(opt: Option[Ast]): List[Option[Ast]] = opt match {
      case None      => List(None)
      case Some(ast) => walk(ast).map(ast => Some(ast))
    }
    def walk(ast: Ast): List[Ast] = ast match
      case ast: Lexical   => walk(ast)
      case ast: Syntactic => walk(ast)
    def walk(ast: Lexical): List[Lexical] = List(ast)
    def walk(ast: Syntactic): List[Syntactic]
  }

  private type Childrens = List[Vector[Option[Ast]]]

  trait MultiplicativeListWalker extends ListWalker {
    def walk(ast: Syntactic): List[Syntactic] =
      val Syntactic(name, args, rhsIdx, children) = ast
      val newChildrens = children
        .map(walkOpt)
        .foldLeft[Childrens](List(Vector()))((childrens, childs) => {
          for {
            childrens <- childrens
            child <- childs
          } yield (childrens :+ child)
        })
      newChildrens.map(newChildren =>
        Syntactic(name, args, rhsIdx, newChildren),
      )
  }

  trait AdditiveListWalker extends ListWalker {
    def walk(ast: Syntactic): List[Syntactic] =
      val Syntactic(name, args, rhsIdx, children) = ast
      // pair of processed childrens and unprocessed childrens
      val initStat: (Childrens, Childrens) = (List(), List(Vector()))
      val newStat = children
        .foldLeft[(Childrens, Childrens)](initStat)((stat, child) => {
          val (done, yet) = stat
          val done1 = done.map(_ :+ child)
          val done2: Childrens = for {
            child <- walkOpt(child)
            children <- yet
          } yield (children :+ child)
          (done1 ++ done2, yet.map(_ :+ child))
        })
      newStat._1.map(newChildren => Syntactic(name, args, rhsIdx, newChildren))
  }

}
