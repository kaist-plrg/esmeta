package esmeta.es.util.mutator

import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.synthesizer.*
import esmeta.es.util.{UnitWalker => AstWalker, *}
import esmeta.es.util.Coverage.*
import esmeta.spec.Grammar
import esmeta.util.*
import esmeta.util.BaseUtils.*

/** A mutator that replaces litrals */
class LexicalMutator(
  val synthesizer: Synthesizer = RandomSynthesizer,
) extends Mutator
  with Util.MultiplicativeListWalker {

  val names = "LexicalMutator" :: RandomMutator.default.names

  import LexicalMutator.*

  /** mutate a program */
  def apply(
    ast: Ast,
    n: Int,
    _target: Option[(CondView, Coverage)],
  ): Seq[(String, Ast)] = {
    // count the number of potential victims
    val k = lexicalCounter(ast)
    if (k == 0)
      (name, walk(ast).head) +: RandomMutator.default(ast, n - 1, _target)
    else {
      val (kc1, kc2) = calcParam(n, k)
      k1 = kc1._1; c1 = kc1._2
      k2 = kc2._1; c2 = kc2._2
      sample(ast, n)
    }
  }

  /** parameter for sampler */
  private var (k1, c1, k2, c2) = (0, 0, 0, 0)

  private def sample(ast: Ast, n: Int) =
    shuffle(walk(ast)).take(n).map((name, _))

  private def getGenNum: Int =
    if k1 > 0 && randBool(k1 / (k1 + k2 + 0.0)) then
      k1 -= 1; c1
    else if k2 > 0 then
      k2 -= 1; c2
    else throw new Error(s"This is a bug in $name")

  private var insideTarget = false

  /** ast walker */
  override def walk(lex: Lexical): List[Lexical] = lex.name match {
    case STRING_LITERAL =>
      lex :: shuffle(specStrings)
        .take(getGenNum - 1)
        .map(s => Lexical(lex.name, s"\'$s\'"))
        .toList
    case IDENTIFIER_NAME | IDENTIFIER_NAME_RESERVED_WORD if insideTarget =>
      lex :: shuffle(specStrings)
        .take(getGenNum - 1)
        .map(s => Lexical(lex.name, s))
        .toList
    case NUMERIC_LITERAL =>
      val num = if randBool then 0 else 1
      val big = if randBool then "n" else ""
      List(Lexical(lex.name, s"$num$big"))
    case BOOLEAN_LITERAL => List(Lexical(lex.name, s"$randBool"))
    case _               => List(lex)
  }

  override def preChild(syn: Syntactic, i: Int) =
    val Syntactic(name, _, rhsIdx, _) = syn
    if (targetSyntactics.contains((name, rhsIdx, i)))
      insideTarget = true

  override def postChild(syn: Syntactic, i: Int) =
    val Syntactic(name, _, rhsIdx, _) = syn
    if (targetSyntactics.contains((name, rhsIdx, i)))
      insideTarget = false
}

object LexicalMutator {
  // macro
  val STRING_LITERAL = "StringLiteral"
  val BOOLEAN_LITERAL = "BooleanLiteral"
  val LITERAL_PROPERTY_NAME = "LiteralPropertyName"
  val IDENTIFIER_NAME = "IdentifierName"
  val IDENTIFIER_NAME_RESERVED_WORD = "IdentifierName \\ ReservedWord"
  val NUMERIC_LITERAL = "NumericLiteral"
  val MEMBER_EXPRESSION = "MemberExpression"
  val CALL_EXPRESSION = "CallExpression"
  val OPTIONAL_CHAIN = "OptionalChain"

  // taarget lexicals
  val targetLexicals = List(
    STRING_LITERAL,
  )
  // target Syntactics that contains Lexicals
  val targetSyntactics = List(
    (LITERAL_PROPERTY_NAME, 0, 0),
    (MEMBER_EXPRESSION, 2, 1),
    (CALL_EXPRESSION, 5, 1),
    (OPTIONAL_CHAIN, 2, 0),
    (OPTIONAL_CHAIN, 7, 1),
  )

  def isTargetLex(ast: Ast): Boolean = ast match {
    case Lexical(name, _) => targetLexicals.contains(name)
    case Syntactic(name, _, rhsIdx, _) =>
      targetSyntactics.exists(info => info._1 == name && info._2 == rhsIdx)
  }

  // count the number of lexicals
  val lexicalCounter = Util.AstCounter(isTargetLex)

  // String literals appearing in CFG
  private var _specStrings: Set[String] = Set()
  lazy val specStrings: Vector[String] = {
    import esmeta.ir.*
    object StrFinder extends util.UnitWalker {
      override def walk(e: Expr): Unit = e match {
        case EStr(str) if str.matches("^[a-zA-Z0-9_]+$") =>
          _specStrings += str
        case _ =>
      }
      override def walk(ref: Ref): Unit = ref match {
        case Prop(ref, expr) => walk(ref)
        case _               => super.walk(ref)
      }
    }
    object StrLocFinder extends util.UnitWalker {
      override def walk(bin: Expr): Unit = bin match {
        case EBinary(BOp.Eq, l, r) =>
          StrFinder.walk(l);
          StrFinder.walk(r);
        case _ => super.walk(bin)
      }

      override def walk(inst: Inst) = inst match {
        case ICall(_, _, as) => walkList(as, StrFinder.walk)
        case _               => super.walk(inst)
      }
    }
    _specStrings = Set()
    StrFinder.walk(cfg.program)
    _specStrings.toVector
  }
}
