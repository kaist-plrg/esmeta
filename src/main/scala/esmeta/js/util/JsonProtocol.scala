package esmeta.js.util

import io.circe.*, io.circe.syntax.*, io.circe.parser.*,
io.circe.generic.semiauto._
import esmeta.js.*
import scala.collection.mutable.ListBuffer

/** json encoder/decoder */
object JsonProtocol {
  def decodeFail[T](msg: String, c: HCursor): Decoder.Result[T] =
    Left(DecodingFailure(msg, c.history))
  def assignId[T <: Ast](ast: T, idOpt: Option[JsonNumber]): T = {
    ast.idOpt = idOpt.map(_.toInt.get); ast
  }

  // given Encoder[List[Boolean]] = new Encoder[List[Boolean]] {
  //   final def apply(l: List[Boolean]): Json =
  //     if (l.isEmpty) None.asJson
  //     else
  //       Some(
  //         l.reverse.zipWithIndex
  //           .foldLeft(0) {
  //             case (acc, (b, idx)) => acc + (if (b) 1 << idx else 0)
  //           },
  //       ).asJson
  // }
  // given Decoder[List[Boolean]] = new Decoder[List[Boolean]] {
  //   final def apply(c: HCursor): Decoder.Result[List[Boolean]] = {
  //     Right(c.value.asNumber match {
  //       case Some(n) =>
  //         var n0 = n.toInt.get
  //         if (n0 == 0) List(false)
  //         else {
  //           val buf = ListBuffer[Boolean]()
  //           while (n0 > 0) {
  //             buf.prepend(n0 % 2 == 1)
  //             n0 = n0 >>> 1
  //           }
  //           buf.toList
  //         }
  //       case None => List()
  //     })
  //   }
  // }

  given Encoder[Ast] = Encoder.instance {
    case syn: Syntactic => syn.asJson
    case lex: Lexical   => lex.asJson
  }
  given Decoder[Ast] = new Decoder[Ast] {
    final def apply(c: HCursor): Decoder.Result[Ast] = {
      val arr = c.value.asArray.get
      if (arr.length == 5) synDecoder(c)
      else if (arr.length == 3) lexDecoder(c)
      else decodeFail(s"unknown Ast: ${c.value}", c)
    }
  }

  given Encoder[Syntactic] = new Encoder[Syntactic] {
    final def apply(syn: Syntactic): Json = Json.arr(
      syn.idOpt.asJson,
      syn.name.asJson,
      syn.args.asJson,
      syn.rhsIdx.asJson,
      syn.children.asJson,
    )
  }
  given synDecoder: Decoder[Syntactic] = new Decoder[Syntactic] {
    final def apply(c: HCursor): Decoder.Result[Syntactic] =
      (for {
        data <- c.value.asArray
        (idOpt, name, args, rhsIdx, children) <- data match {
          case Vector(i, n, a, r, cs) =>
            Some(
              i.asNumber,
              n.asString.get,
              a.as[List[Boolean]].toOption.get,
              r.as[Int].toOption.get,
              cs.as[List[Option[Ast]]].toOption.get,
            )
          case _ => None
        }
        syn = Syntactic(name, args, rhsIdx, children)
      } yield {
        for {
          childOpt <- children
          child <- childOpt
        } child.parent = Some(syn)
        Right(assignId(syn, idOpt))
      }).getOrElse {
        decodeFail(s"unknown Lexical: ${c.value}", c)
      }

  }

  given Encoder[Lexical] = new Encoder[Lexical] {
    final def apply(lex: Lexical): Json = Json.arr(
      lex.idOpt.asJson,
      lex.name.asJson,
      lex.str.asJson,
    )
  }
  given lexDecoder: Decoder[Lexical] = new Decoder[Lexical] {
    final def apply(c: HCursor): Decoder.Result[Lexical] =
      (for {
        data <- c.value.asArray
        (idOpt, name, str) <- data match {
          case Vector(i, n, s) =>
            Some((i.asNumber, n.asString.get, s.asString.get))
          case _ => None
        }
        lex = Lexical(name, str)
      } yield Right(assignId(lex, idOpt))).getOrElse {
        decodeFail(s"unknown Lexical: ${c.value}", c)
      }
  }
}
