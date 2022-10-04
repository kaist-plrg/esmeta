package esmeta.es.util.synthesizer

import esmeta.cfg.*
import esmeta.error.*
import esmeta.es.*
import esmeta.spec.*
import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.BaseUtils.*

/** An ECMAScript AST synthesizer for built-in libraries */
class BuiltinSynthesizer(
  val cfg: CFG,
) extends Synthesizer {
  import grammar.*
  import BuiltinSynthesizer.*
  import BuiltinPath.*

  /** get script */
  def script: String = "String ( 0 ) ; "

  /** get initial pool */
  lazy val initPool: Vector[String] = (for {
    BuiltinHead(path, _, _) <- cfg.spec.algorithms.map(_.head)
    code <- path match
      case YetPath(_) => Nil
      case Getter(base) =>
        getString(base) :: (base match
          case Prototype(proto, prop) =>
            List(
              s"var x = { } ; Object . setPrototypeOf ( x , $proto ) ; x $prop ; ",
            )
          case _ => Nil
        )
      case Setter(base) =>
        getString(base) :: (base match
          case Prototype(proto, prop) =>
            List(
              s"var x = { } ; Object . setPrototypeOf ( x , $proto ) ; x $prop = 0 ; ",
            )
          case _ => Nil
        )
      case path =>
        val pathStr = getString(path)
          .replace(".", " . ")
          .replace("[", " [ ")
          .replace("]", " ]")
        // calls
        val calls = for {
          argsLen <- Range(1, 6).toList
          argsStr = List.fill(argsLen)("0 ").mkString("( ", ", ", ")")
        } yield s"$pathStr . call $argsStr ; "
        // construct without arguments
        val construct = s"new $pathStr ; "
        // constructs with arguments
        val constructs = for {
          argsLen <- Range(0, 5).toList
          argsStr = List.fill(argsLen)("0 ").mkString("( ", ", ", ")")
        } yield s"new $pathStr $argsStr ; "
        calls ++ (construct :: constructs)

  } yield code).toVector

  // get prototype paths and properties
  object Prototype:
    def unapply(path: BuiltinPath): Option[(String, String)] = path match
      case NormalAccess(NormalAccess(base, "prototype"), name) =>
        Some((s"${getString(base)} . prototype", s". $name"))
      case SymbolAccess(NormalAccess(base, "prototype"), symbol) =>
        Some((s"${getString(base)} . prototype", s"[ Symbol . $symbol ]"))
      case _ => None

  // get string of builtin path
  private def getString(path: BuiltinPath): String =
    (new Appender >> path).toString
  private given builtinPathRule: Rule[BuiltinPath] = (app, path) =>
    path match
      case Base(name)               => app >> name
      case NormalAccess(base, name) => app >> base >> "." >> name
      case Getter(base)             => app >> base
      case Setter(base)             => app >> base
      case SymbolAccess(base, symbol) =>
        app >> base >> "[Symbol." >> symbol >> "]"
      case YetPath(name) => app >> "yet:" >> name.replace(" ", "")

  /** for syntactic production */
  def apply(name: String, args: List[Boolean]): Syntactic =
    throw NotSupported("BuiltinSynthesizer.apply")

  /** for lexical production */
  def apply(name: String): Lexical =
    throw NotSupported("BuiltinSynthesizer.apply")

  /** synthesizer builder */
  def builder: Synthesizer.Builder = BuiltinSynthesizer
}
object BuiltinSynthesizer extends Synthesizer.Builder {
  val name: String = "BuiltinSynthesizer"
  def apply(cfg: CFG) = new BuiltinSynthesizer(cfg)
}
