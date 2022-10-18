package esmeta.state.util

import esmeta.cfg.*
import esmeta.cfg.util.{JsonProtocol => CFGJsonProtocol}
import esmeta.ir.EReturnIfAbrupt
import esmeta.spec.{SyntaxDirectedOperationHead, BuiltinHead}
import esmeta.state.*
import esmeta.util.*
import io.circe.*, io.circe.generic.semiauto.*
import io.circe.syntax.*

/** JSON protocols for states */
class JsonProtocol(cfg: CFG) extends CFGJsonProtocol(cfg) {
  // abstraction of call stack as graph
  given callGraphDecoder: Decoder[CallGraph] = deriveDecoder
  given callGraphEncoder: Encoder[CallGraph] = deriveEncoder

  // ECMAScript features
  given featureDecoder: Decoder[Feature] = new Decoder {
    final def apply(c: HCursor): Decoder.Result[Feature] = for {
      func <- funcDecoder(c)
      feature <- func.head match
        case Some(head: SyntaxDirectedOperationHead) =>
          Right(SyntacticFeature(func, head))
        case Some(head: BuiltinHead) =>
          Right(BuiltinFeature(func, head))
        case _ =>
          unknownFail("feature", c)
    } yield feature
  }
  given featureEncoder: Encoder[Feature] = new Encoder {
    final def apply(feature: Feature): Json = funcEncoder(feature.func)
  }
}