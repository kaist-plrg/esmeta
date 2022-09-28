package esmeta.state

import esmeta.cfg.*

/** provenance of addresses */
case class Provenance(
  cursor: Cursor,
  sdo: Option[SdoInfo],
) extends StateElem
