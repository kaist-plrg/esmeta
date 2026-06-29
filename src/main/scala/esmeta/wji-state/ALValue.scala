package esmeta.wji.state

import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax.*

/** Numeric AL values, mirroring `xl/num.ml`'s `num` variant
  * (`spectec/spectec/src/xl/num.ml`).
  *
  * Wire format:
  *   - `Nat`/`Int`: `{"kind":"nat","value":"42"}` (decimal string, arbitrary precision)
  *   - `Rat`: `{"kind":"rat","num":"1","den":"2"}`
  *   - `Real`: `{"kind":"real","value":3.14}`
  */
enum ALNum:
  case Nat(value: BigInt)
  case Int(value: BigInt)
  case Rat(num: BigInt, den: BigInt)
  case Real(value: Double)

object ALNum:
  given Encoder[ALNum] = Encoder.instance {
    case Nat(v)      => Json.obj("kind" -> "nat".asJson, "value" -> v.toString.asJson)
    case Int(v)      => Json.obj("kind" -> "int".asJson, "value" -> v.toString.asJson)
    case Rat(n, d)   => Json.obj("kind" -> "rat".asJson, "num" -> n.toString.asJson, "den" -> d.toString.asJson)
    case Real(v)     => Json.obj("kind" -> "real".asJson, "value" -> v.asJson)
  }

  given Decoder[ALNum] = Decoder.instance { c =>
    c.get[String]("kind").flatMap {
      case "nat"  => c.get[String]("value").map(s => Nat(BigInt(s)))
      case "int"  => c.get[String]("value").map(s => Int(BigInt(s)))
      case "rat"  => for n <- c.get[String]("num"); d <- c.get[String]("den") yield Rat(BigInt(n), BigInt(d))
      case "real" => c.get[Double]("value").map(Real.apply)
      case other  => Left(io.circe.DecodingFailure(s"unknown ALNum kind: $other", c.history))
    }
  }

/** AL (Algorithmic Language) runtime values, mirroring the `value` type of
  * SpecTec's AL AST (`spectec/spectec/src/al/ast.ml`):
  *
  * {{{
  * and value =
  *   | NumV of Num.num                    (* number *)
  *   | BoolV of bool                      (* boolean *)
  *   | TextV of string                    (* string *)
  *   | ListV of value growable_array      (* list of values *)
  *   | StrV of (id, value) record         (* key-value mapping *)
  *   | CaseV of id * value list           (* constructor *)
  *   | OptV of value option               (* optional value *)
  *   | TupV of value list                 (* tuple of values *)
  *   | FnameV of id                       (* name of the first order function *)
  * }}}
  *
  * This is the common value representation used in the params/results of the
  * Wasm Embedding interface (`esmeta.wji.bridge.host.WasmHost`): stores, modules,
  * module instances, function/extern addresses, `val*`, types, etc. are all
  * encoded as [[ALValue]].
  *
  * Wire format: `{"type": "<tag>", ...fields}`.
  */
enum ALValue:
  case NumV(num: ALNum)
  case BoolV(value: Boolean)
  case TextV(value: String)
  case ListV(values: List[ALValue])
  case StrV(fields: List[(String, ALValue)])
  case CaseV(id: String, args: List[ALValue])
  case OptV(value: Option[ALValue])
  case TupV(values: List[ALValue])
  case FnameV(id: String)

object ALValue:
  given Encoder[ALValue] = Encoder.instance {
    case NumV(n)        => Json.obj("type" -> "num".asJson).deepMerge(n.asJson)
    case BoolV(v)       => Json.obj("type" -> "bool".asJson, "value" -> v.asJson)
    case TextV(v)       => Json.obj("type" -> "text".asJson, "value" -> v.asJson)
    case ListV(vs)      => Json.obj("type" -> "list".asJson, "values" -> vs.asJson)
    case StrV(fs)       => Json.obj("type" -> "str".asJson, "fields" -> fs.map { (k, v) => Json.obj("key" -> k.asJson, "value" -> v.asJson) }.asJson)
    case CaseV(id, as)  => Json.obj("type" -> "case".asJson, "id" -> id.asJson, "args" -> as.asJson)
    case OptV(v)        => Json.obj("type" -> "opt".asJson, "value" -> v.asJson)
    case TupV(vs)       => Json.obj("type" -> "tup".asJson, "values" -> vs.asJson)
    case FnameV(id)     => Json.obj("type" -> "fname".asJson, "id" -> id.asJson)
  }

  given Decoder[ALValue] = Decoder.instance { c =>
    c.get[String]("type").flatMap {
      case "num"  => c.as[ALNum].map(NumV.apply)
      case "bool" => c.get[Boolean]("value").map(BoolV.apply)
      case "text" => c.get[String]("value").map(TextV.apply)
      case "list" => c.get[List[ALValue]]("values").map(ListV.apply)
      case "str" =>
        c.get[List[Json]]("fields").flatMap { fs =>
          fs.traverse { f =>
            val fc = f.hcursor
            for k <- fc.get[String]("key"); v <- fc.get[ALValue]("value") yield (k, v)
          }
        }.map(StrV.apply)
      case "case" =>
        for id <- c.get[String]("id"); args <- c.get[List[ALValue]]("args") yield CaseV(id, args)
      case "opt" => c.get[Option[ALValue]]("value").map(OptV.apply)
      case "tup" => c.get[List[ALValue]]("values").map(TupV.apply)
      case "fname" => c.get[String]("id").map(FnameV.apply)
      case other => Left(io.circe.DecodingFailure(s"unknown ALValue type: $other", c.history))
    }
  }

  extension [A](as: List[A])
    private def traverse[B](f: A => Decoder.Result[B]): Decoder.Result[List[B]] =
      as.foldRight[Decoder.Result[List[B]]](Right(Nil)) { (a, acc) =>
        for x <- f(a); xs <- acc yield x :: xs
      }
