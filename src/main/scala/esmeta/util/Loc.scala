package esmeta.util

import esmeta.LINE_SEP
import scala.annotation.alpha

/** A trait for objects that have a location in spec.html */
trait Locational {
  var loc: Option[Loc] = None
  def setLoc(start: Pos, end: Pos, steps: List[Int]): this.type =
    setLoc(Some(Loc(start, end, steps)))
  def setLoc(locOpt: Option[Loc]): this.type = {
    if (loc.isEmpty) loc = locOpt
    this
  }
}

/** source locations in algorithms
  *
  * @example
  *   3:2-4:7 (1.2.2) for `Loc(Pos(3,2), Pos(4,7), List(1,2,2,))`
  */
case class Loc(
  var start: Pos,
  var end: Pos,
  var steps: List[Int],
) {

  /** get substring from a string */
  def getString(str: String): String = str.substring(start.offset, end.offset)

  // TODO short string for the same line (e.g. 3:2-4)
  /** conversion to string */
  override def toString: String =
    s"${start.simpleString}-${end.simpleString}$stepString"

  /** get step string */
  def stepString: String =
    if (steps.isEmpty) ""
    else
      (for ((step, idx) <- steps.zipWithIndex) yield idx % 3 match
        case 0 => step.toString
        case 1 => AlphabetNumeral(step + 1)
        case 2 => RomanNumeral(step, lower = true)
      ).mkString(" (step ", ".", ")")
}

/** positions in algorithms
  *
  * @example
  *   3:2 for `Pos(3,2)`
  */
case class Pos(
  var line: Int,
  var column: Int,
  var offset: Int,
) {

  /** get simple string */
  def simpleString: String = s"$line:$column"

  /** append another position */
  def +(that: Pos): Pos =
    val newLine = this.line + that.line - 1
    val newColumn = (if (that.line == 1) this.column else 0) + that.column
    val newOffset = this.offset + that.offset
    Pos(newLine, newColumn, newOffset)

  /** append a source location */
  def +(loc: Loc): Loc = Loc(this + loc.start, this + loc.end, loc.steps)

  /** conversion to string */
  override def toString: String = s"$simpleString($offset)"
}
