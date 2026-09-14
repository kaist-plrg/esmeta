package esmeta.wji.lang.parser

/** Bracket/quote-aware substring search and splitting, used to find separators
  * at the "top level" of spec prose — i.e. outside any `(...)`, `[...]`,
  * `{...}`, `«...»`, or `"..."`.
  */
private[wji] object TextSplit:
  private val Open = "([{«".toSet
  private val Close = ")]}»".toSet

  /** the index of the first top-level occurrence of `sep` in `text`, if any */
  def findTopLevel(text: String, sep: String): Option[Int] =
    findTopLevelAny(text, Seq(sep)).map(_._1)

  /** the index (and matched separator) of the first top-level occurrence of any
    * of `seps` in `text` at or after `from`, if any. `from` must itself be a
    * top-level position (bracket depth 0, outside any string) in `text` —
    * [[findLastTopLevelAny]] relies on this to resume scanning without
    * re-deriving depth/string state for the part of `text` before `from`.
    */
  def findTopLevelAny(
    text: String,
    seps: Seq[String],
    from: Int = 0,
  ): Option[(Int, String)] =
    var depth = 0
    var inString = false
    var i = from
    while i < text.length do
      if !inString && depth == 0 then
        seps.find(text.startsWith(_, i)) match
          case found @ Some(_) => return found.map(i -> _)
          case None            =>
      val c = text(i)
      if c == '"' then inString = !inString
      else if !inString then
        if Open(c) then depth += 1
        else if Close(c) then depth -= 1
      i += 1
    None

  /** splits `text` at the first top-level occurrence of `sep`, if any */
  def splitTopLevel(text: String, sep: String): Option[(String, String)] =
    findTopLevel(text, sep).map(i =>
      (text.substring(0, i), text.substring(i + sep.length)),
    )

  /** the index (and matched separator) of the *last* top-level occurrence of
    * any of `seps` in `text`, if any — e.g. for splitting a chain of binary
    * operators (`a + b - c`) at its rightmost top-level operator, so the
    * left-hand side recurses out left-associatively.
    */
  def findLastTopLevelAny(
    text: String,
    seps: Seq[String],
  ): Option[(Int, String)] =
    var last: Option[(Int, String)] = None
    var from = 0
    var continue = true
    while continue do
      findTopLevelAny(text, seps, from) match
        case Some((i, sep)) =>
          last = Some((i, sep))
          from = i + sep.length
        case None => continue = false
    last

  /** splits `text` at every top-level occurrence of `sep` */
  def splitTopLevelAll(text: String, sep: String): List[String] =
    val parts = scala.collection.mutable.ListBuffer[String]()
    var remaining = text
    var continue = true
    while continue do
      findTopLevel(remaining, sep) match
        case Some(i) =>
          parts += remaining.substring(0, i)
          remaining = remaining.substring(i + sep.length)
        case None =>
          parts += remaining
          continue = false
    parts.toList

  /** splits `raw` at every top-level comma, trimming and dropping empty parts
    */
  def splitComma(raw: String): List[String] =
    if raw.trim.isEmpty then Nil
    else splitTopLevelAll(raw, ",").map(_.trim).filter(_.nonEmpty)

  /** a single `[=dfn link=]` token, and an English list of them ("A, B or C",
    * "A, B, or C", or a bare "A or B") — shared between
    * [[esmeta.wji.lang.parser.CondParser]]'s `IsOneOfPos`/`IsOneOfNeg` (which
    * builds the actual `Cond` from a full "X is [not] one of ..." match) and
    * [[isOneOfSpans]] below (which only needs the list's own character range,
    * to keep some *other* top-level split from cutting through it).
    */
  val EnumItem = """\[=[^\]]+=\]"""
  val EnumList =
    s"""$EnumItem(?:\\s*,\\s*$EnumItem)*(?:\\s*,?\\s*or\\s+$EnumItem)?"""
  private val IsOneOfSpan =
    s"""(?si)is (?:not )?one of\\s+($EnumList)""".r

  /** the `[start, end)` character ranges of every "is [not] one of A, B or C"
    * enumeration in `text` (index.bs:521's "|valtype| is one of [=i32=],
    * [=f32=] or [=f64=]" being the corpus's one occurrence so far). A list like
    * this can itself contain a ", "/" or " that reads exactly like the
    * separator a caller is scanning `text` for at a higher level (e.g.
    * `InstrParser.splitCondAndRest`'s cond/rest-splitting comma, or
    * `CondParser.parse`'s own top-level `" or "`/`" and "` search) — such a
    * caller should skip any candidate position that falls inside one of these
    * ranges, the same way it already skips a comma immediately followed by "and
    * "/"or " (a *different* list shape, joined at every separator rather than
    * only before the last item).
    */
  def isOneOfSpans(text: String): List[(Int, Int)] =
    IsOneOfSpan.findAllMatchIn(text).map(m => (m.start(1), m.end(1))).toList
