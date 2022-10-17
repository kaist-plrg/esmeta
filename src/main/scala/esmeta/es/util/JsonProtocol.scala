package esmeta.es.util

import esmeta.ir.EReturnIfAbrupt
import esmeta.cfg.*
import esmeta.state.util.{JsonProtocol => StateJsonProtocol}
import esmeta.util.*
import io.circe.*, io.circe.generic.semiauto.*
import io.circe.syntax.*

/** JSON protocols for CFG */
class JsonProtocol(cfg: CFG) extends StateJsonProtocol(cfg) {
  import Coverage.*

  // branch or reference to EReturnIfAbrupt with boolean values
  given condDecoder: Decoder[Cond] = deriveDecoder
  given condEncoder: Encoder[Cond] = deriveEncoder
  given condElemDecoder: Decoder[Branch | WeakUIdRef[EReturnIfAbrupt]] =
    val branchDecoder = idDecoder[Branch | WeakUIdRef[EReturnIfAbrupt]](
      "branch",
      cfg.nodeMap.get(_).collect { case branch: Branch => branch },
    )
    val abruptDecoder = idDecoder[Branch | WeakUIdRef[EReturnIfAbrupt]](
      "abrupt",
      cfg.riaExmprMap.get(_).map(_.idRef),
    )
    branchDecoder.handleErrorWith(_ => abruptDecoder)
  given condElemEncoder: Encoder[Branch | WeakUIdRef[EReturnIfAbrupt]] =
    new Encoder {
      final def apply(x: Branch | WeakUIdRef[EReturnIfAbrupt]): Json = x match
        case branch: Branch                      => idEncoder("branch")(branch)
        case abrupt: WeakUIdRef[EReturnIfAbrupt] => idEncoder("branch")(abrupt)
    }

  // syntax-sensitive views
  given viewDecoder: Decoder[View] = deriveDecoder
  given viewEncoder: Encoder[View] = deriveEncoder
  given nodeViewDecoder: Decoder[NodeView] = deriveDecoder
  given nodeViewEncoder: Encoder[NodeView] = deriveEncoder
  given condViewDecoder: Decoder[CondView] = deriveDecoder
  given condViewEncoder: Encoder[CondView] = deriveEncoder
}
