package esmeta.ir

import esmeta.util.BasicParser

/** IR parser */
trait Parser[T] extends BasicParser[T] with Parsers
object Parser extends Parsers