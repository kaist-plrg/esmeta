package esmeta.editor.util

import io.circe.*, io.circe.syntax.*, io.circe.parser.*,
io.circe.generic.semiauto._
import esmeta.editor.sview.*
import scala.collection.mutable.ListBuffer

/** json encoder/decoder */
object JsonProtocol {
  def decodeFail[T](msg: String, c: HCursor): Decoder.Result[T] =
    Left(DecodingFailure(msg, c.history))

  // conversion helpers
  given Conversion[Json, Int] with
    def apply(json: Json): Int = json.as[Int].toOption.get
  given Conversion[Json, String] with
    def apply(json: Json): String = json.as[String].toOption.get
  given Conversion[Int, Json] with
    def apply(i: Int): Json = i.asJson
  given Conversion[String, Json] with
    def apply(s: String): Json = s.asJson

  // filter result
  given Encoder[FilterResult] = deriveEncoder
  given Decoder[FilterResult] = deriveDecoder

  // program index
  given Encoder[ProgramIndex] = deriveEncoder
  given Decoder[ProgramIndex] = deriveDecoder

  // program info
  given Encoder[ProgramInfo] = deriveEncoder
  given Decoder[ProgramInfo] = deriveDecoder

  // simplified ast
  given Encoder[SimpleAst] = Encoder.instance {
    case syn: SimpleSyntactic    => syn.asJson
    case lex: SimpleLexical      => lex.asJson
    case abs: SimpleAbsSyntactic => ???
  }
  given Decoder[SimpleAst] = new Decoder[SimpleAst] {
    final def apply(c: HCursor): Decoder.Result[SimpleAst] = {
      val arr = c.value.asArray.get
      if (arr.length == 5) synDecoder(c)
      else if (arr.length == 3) lexDecoder(c)
      else decodeFail(s"unknown simple Ast: ${c.value}", c)
    }
  }

  // simplified syntactic
  given Encoder[SimpleSyntactic] = new Encoder[SimpleSyntactic] {
    final def apply(syn: SimpleSyntactic): Json =
      Json.arr(syn.id, syn.nameIdx, syn.idx, syn.subIdx, syn.children.asJson)
  }
  given synDecoder: Decoder[SimpleSyntactic] = new Decoder[SimpleSyntactic] {
    final def apply(c: HCursor): Decoder.Result[SimpleSyntactic] =
      (for {
        data <- c.value.asArray
        syn <- data match
          case Vector(id, n, idx, subIdx, cs) =>
            val children = cs.as[List[SimpleAst]].toOption.get
            Some(SimpleSyntactic(n, idx, subIdx, children).setId(id))
          case _ => None
      } yield Right(syn)).getOrElse {
        decodeFail(s"unknown Lexical: ${c.value}", c)
      }
  }

  // simplified lexical
  given Encoder[SimpleLexical] = new Encoder[SimpleLexical] {
    final def apply(lex: SimpleLexical): Json =
      Json.arr(lex.id, lex.nameIdx, lex.str)
  }
  given lexDecoder: Decoder[SimpleLexical] = new Decoder[SimpleLexical] {
    final def apply(c: HCursor): Decoder.Result[SimpleLexical] =
      (for {
        data <- c.value.asArray
        lex <- data match
          case Vector(id, n, s) => Some(SimpleLexical(n, s).setId(id))
          case _                => None
      } yield Right(lex)).getOrElse {
        decodeFail(s"unknown SimpleLexical: ${c.value}", c)
      }
  }

  // annotation
  given Encoder[Annotation] = Encoder.instance {
    case AObj    => 0.asJson
    case ASymbol => 1.asJson
    case ANum    => 2.asJson
    case ABigInt => 3.asJson
    case AStr    => 4.asJson
    case ABool   => 5.asJson
    case AUndef  => 6.asJson
    case ANull   => 7.asJson
    case AThrow  => 8.asJson
    case AAll    => 9.asJson
  }
  given Decoder[Annotation] = new Decoder[Annotation] {
    final def apply(c: HCursor): Decoder.Result[Annotation] = {
      c.value.asNumber.get.toInt.get match {
        case 0 => Right(AObj)
        case 1 => Right(ASymbol)
        case 2 => Right(ANum)
        case 3 => Right(ABigInt)
        case 4 => Right(AStr)
        case 5 => Right(ABool)
        case 6 => Right(AUndef)
        case 7 => Right(ANull)
        case 8 => Right(AThrow)
        case 9 => Right(AAll)
        case _ => decodeFail(s"unknown annotation: ${c.value}", c)
      }
    }
  }

}
