package esmeta.state.util

import esmeta.state.{ALNum, ALValue}
import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax.*

/** JSON wire encoding for [[ALNum]] / [[ALValue]], used on the JSON-RPC
  * connection between wjmeta-bridge and the spectec-server process (see
  * `esmeta.wji.bridge.SpecTecWasmHost`).
  *
  * Kept separate from the `ALNum`/`ALValue` ADTs themselves (in
  * `state/Value.scala`) since this wire format is specific to that one
  * boundary, not part of the core value representation.
  */
object ALValueJson:

  /** Wire format:
    *   - `Nat`/`Int`: `{"kind":"nat","value":"42"}` (decimal string, arbitrary
    *     precision)
    *   - `Rat`: `{"kind":"rat","num":"1","den":"2"}`
    *   - `Real`: `{"kind":"real","value":3.14}`
    */
  given Encoder[ALNum] = Encoder.instance {
    case ALNum.Nat(v) =>
      Json.obj("kind" -> "nat".asJson, "value" -> v.toString.asJson)
    case ALNum.Int(v) =>
      Json.obj("kind" -> "int".asJson, "value" -> v.toString.asJson)
    case ALNum.Rat(n, d) =>
      Json.obj(
        "kind" -> "rat".asJson,
        "num" -> n.toString.asJson,
        "den" -> d.toString.asJson,
      )
    case ALNum.Real(v) =>
      Json.obj("kind" -> "real".asJson, "value" -> v.asJson)
  }

  given Decoder[ALNum] = Decoder.instance { c =>
    c.get[String]("kind").flatMap {
      case "nat" =>
        c.get[String]("value").map(s => ALNum.Nat(scala.math.BigInt(s)))
      case "int" =>
        c.get[String]("value").map(s => ALNum.Int(scala.math.BigInt(s)))
      case "rat" =>
        for n <- c.get[String]("num"); d <- c.get[String]("den")
        yield ALNum.Rat(scala.math.BigInt(n), scala.math.BigInt(d))
      case "real" => c.get[Double]("value").map(ALNum.Real.apply)
      case other =>
        Left(io.circe.DecodingFailure(s"unknown ALNum kind: $other", c.history))
    }
  }

  /** Wire format: `{"type": "<tag>", ...fields}`. */
  given Encoder[ALValue] = Encoder.instance {
    case ALValue.NumV(n) =>
      Json.obj("type" -> "num".asJson).deepMerge(n.asJson)
    case ALValue.BoolV(v) =>
      Json.obj("type" -> "bool".asJson, "value" -> v.asJson)
    case ALValue.TextV(v) =>
      Json.obj("type" -> "text".asJson, "value" -> v.asJson)
    case ALValue.ListV(vs) =>
      Json.obj("type" -> "list".asJson, "values" -> vs.asJson)
    case ALValue.StrV(fs) =>
      Json.obj(
        "type" -> "str".asJson,
        "fields" -> fs.map { (k, v) =>
          Json.obj("key" -> k.asJson, "value" -> v.asJson)
        }.asJson,
      )
    case ALValue.CaseV(id, as) =>
      Json.obj("type" -> "case".asJson, "id" -> id.asJson, "args" -> as.asJson)
    case ALValue.OptV(v) =>
      Json.obj("type" -> "opt".asJson, "value" -> v.asJson)
    case ALValue.TupV(vs) =>
      Json.obj("type" -> "tup".asJson, "values" -> vs.asJson)
    case ALValue.FnameV(id) =>
      Json.obj("type" -> "fname".asJson, "id" -> id.asJson)
  }

  given Decoder[ALValue] = Decoder.instance { c =>
    c.get[String]("type").flatMap {
      case "num"  => c.as[ALNum].map(ALValue.NumV.apply)
      case "bool" => c.get[Boolean]("value").map(ALValue.BoolV.apply)
      case "text" => c.get[String]("value").map(ALValue.TextV.apply)
      case "list" => c.get[List[ALValue]]("values").map(ALValue.ListV.apply)
      case "str" =>
        c.get[List[Json]]("fields")
          .flatMap { fs =>
            fs.traverse { f =>
              val fc = f.hcursor
              for k <- fc.get[String]("key"); v <- fc.get[ALValue]("value")
              yield (k, v)
            }
          }
          .map(ALValue.StrV.apply)
      case "case" =>
        for id <- c.get[String]("id"); args <- c.get[List[ALValue]]("args")
        yield ALValue.CaseV(id, args)
      case "opt"   => c.get[Option[ALValue]]("value").map(ALValue.OptV.apply)
      case "tup"   => c.get[List[ALValue]]("values").map(ALValue.TupV.apply)
      case "fname" => c.get[String]("id").map(ALValue.FnameV.apply)
      case other =>
        Left(
          io.circe.DecodingFailure(s"unknown ALValue type: $other", c.history),
        )
    }
  }

  extension [A](as: List[A])
    private def traverse[B](
      f: A => Decoder.Result[B],
    ): Decoder.Result[List[B]] =
      as.foldRight[Decoder.Result[List[B]]](Right(Nil)) { (a, acc) =>
        for x <- f(a); xs <- acc yield x :: xs
      }
