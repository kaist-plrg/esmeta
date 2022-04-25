package esmeta.util

/** measure performance */
object PerformanceRecorder {
  private var map: Map[String, (Int, Long)] = Map()

  def apply[T](key: String)(f: => T): T =
    val (accCnt, accTime) = map.getOrElse(key, (0, 0L))
    val (t, res) = BaseUtils.time(f)
    map += (key -> (accCnt + 1, accTime + t))
    res

  def foreach(f: (String, (Int, Long)) => Unit): Unit =
    for { (k, v) <- map } f(k, v)

  def printStat(): Unit = for { (k, (c, t)) <- map }
    println(s"- $k: $c called in $t (ms) ")
}
