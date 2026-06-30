package esmeta.wji.state

/** Minimal mutable store for the heap-allocated [[WjValue]]s — records and
  * lists. Each allocation returns a fresh integer id used as the payload of
  * [[WjValue.Record]] / [[WjValue.Lst]].
  *
  * This is intentionally simple: no garbage collection, no aliasing analysis,
  * just enough state for the interpreter to model mutable spec objects.
  */
class Heap:
  private var records: Map[Int, Map[String, WjValue]] = Map.empty
  private var lists: Map[Int, Vector[WjValue]] = Map.empty
  private var nextId = 0

  private def fresh(): Int =
    val id = nextId
    nextId += 1
    id

  // -- Records ----------------------------------------------------------------

  def allocRecord(fields: Map[String, WjValue]): Int =
    val id = fresh()
    records += (id -> fields)
    id

  def getField(id: Int, key: String): Option[WjValue] =
    records.getOrElse(id, sys.error(s"Heap: no record #$id")).get(key)

  def setField(id: Int, key: String, value: WjValue): Unit =
    val rec = records.getOrElse(id, sys.error(s"Heap: no record #$id"))
    records += (id -> (rec + (key -> value)))

  def deleteField(id: Int, key: String): Unit =
    val rec = records.getOrElse(id, sys.error(s"Heap: no record #$id"))
    records += (id -> (rec - key))

  // -- Lists ------------------------------------------------------------------

  def allocList(elems: Vector[WjValue]): Int =
    val id = fresh()
    lists += (id -> elems)
    id

  def listGet(id: Int, idx: Int): WjValue =
    list(id).applyOrElse(
      idx,
      _ => sys.error(s"Heap: list #$id index $idx out of bounds"),
    )

  def listSet(id: Int, idx: Int, value: WjValue): Unit =
    lists += (id -> list(id).updated(idx, value))

  def listPush(id: Int, v: WjValue, front: Boolean): Unit =
    val cur = list(id)
    lists += (id -> (if front then v +: cur else cur :+ v))

  def listPop(id: Int, front: Boolean): WjValue =
    val cur = list(id)
    if cur.isEmpty then sys.error(s"Heap: pop from empty list #$id")
    if front then
      lists += (id -> cur.tail)
      cur.head
    else
      lists += (id -> cur.init)
      cur.last

  def listSize(id: Int): Int = list(id).size

  def listAll(id: Int): Vector[WjValue] = list(id)

  private def list(id: Int): Vector[WjValue] =
    lists.getOrElse(id, sys.error(s"Heap: no list #$id"))
