package esmeta.js

import esmeta.*
import esmeta.util.SystemUtils.*

object Bug {
  trait Tag(val _id: String) {
    val kind: String
    override def toString = s"$kind-${_id}"
  }
  case class KnownTag(val id: String) extends Tag(id) {
    val kind = "KNOWN"
  }
  case class TodoTag(val id: String, val path: String) extends Tag(id) {
    val kind = "TODO"
  }
  case class NewTag(val id: String) extends Tag(id) {
    val kind = "NEW"
  }

  def tagFinder(
    _script: String,
    db: Option[Map[String, Set[String]]] = None,
    todos: Option[Map[String, String]] = None,
    test: Option[String] = None,
    msg: Option[String] = None,
  ): Tag =
    val script = _script.trim

    var tag: Tag = NewTag("YET")

    // search if tag of this bug is already known
    for (db <- db)
      db.foreach((id, knowns) =>
        if tag == NewTag("YET") && knowns.contains(script) then
          tag = KnownTag(id),
      )

    // search if this bug is already in TODO
    for (todos <- todos)
      todos.foreach((path, todo) =>
        if tag == NewTag("YET") && todo.contains(script) then
          tag = TodoTag("YET", path),
      )

    // apply heuristic to guess tag of this bug
    for {
      test <- test
      msg <- msg
    } {
      def matchPattern(orig: String, pattern: String) =
        orig.contains(pattern) ||
        pattern.endsWith("$") && orig.endsWith(pattern.dropRight(1))

      def matched(rule: Array[String]): Option[String] =
        val Array(id, scriptPattern, testPattern, msgPattern) = rule

        if matchPattern(script, scriptPattern)
          && matchPattern(test, testPattern)
          && matchPattern(msg, msgPattern)
        then Some(id)
        else None

      manualRule.foreach(rule =>
        tag match
          case NewTag("YET") =>
            matched(rule).foreach(id => tag = NewTag(id))
          case TodoTag("YET", filename) =>
            matched(rule).map(id => TodoTag(id, filename))
          case _ => (),
      )
    }

    tag

  def loadBugDB[T](targets: Iterable[T]) =
    val knownMaps = targets
      .map(target =>
        val db = s"$RESOURCE_DIR/bugs/$target"
        val buggies = listFiles(db).filter(jsFilter)
        val tagMap = buggies
          .map(buggy =>
            (
              buggy.getName.dropRight(3),
              readFile(buggy.getPath).split(LINE_SEP).map(_.trim).toSet,
            ),
          )
          .toMap
        (target, tagMap),
      )
      .toMap

    val todos = targets
      .map(target =>
        val todosDir = s"$RESOURCE_DIR/bugs/$target/TODO"
        val todos = listFiles(todosDir)
          .filter(jsFilter)
          .map(todo =>
            (
              todo.getPath,
              readFile(todo.getPath).split(LINE_SEP)(1).trim,
            ),
          )
          .toMap
        (target, todos),
      )
      .toMap

    (knownMaps, todos)

  /** Manually written rule to categorize bugs kind */
  lazy val manualRule =
    readFile(f"$RESOURCE_DIR/bugs/manual-categorize.rule")
      .split(LINE_SEP)
      .drop(1) // drop header
      .map(l => l.split("\\|", -1))
}
