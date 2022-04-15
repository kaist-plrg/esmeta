package esmeta.editor.util

import io.circe.*, io.circe.syntax.*, io.circe.parser.*,
io.circe.generic.semiauto._
import esmeta.editor.sview.*
import scala.collection.mutable.ListBuffer

/** json encoder/decoder */
object JsonProtocol {
  def decodeFail[T](msg: String, c: HCursor): Decoder.Result[T] =
    Left(DecodingFailure(msg, c.history))

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
