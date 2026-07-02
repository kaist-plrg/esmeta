package esmeta.wji.lang

import Expr.*
import Cond.*

/** Parses raw spec prose condition strings into [[Cond]] trees, falling back to
  * [[Cond.Unknown]] for unrecognized patterns.
  */
object CondParser:
  import TextSplit.*

  private val IsTypePos = """(?si)^(.+)\s+\[=is an?\s+([^\]]+)=\]$""".r
  private val IsTypeNeg = """(?si)^(.+)\s+\[=is not an?\s+([^\]]+)=\]$""".r
  private val MatchesNeg =
    """(?s)^(.+?)\s+does not\s+(\[=matches/[^\]]*\])\s+(.+)$""".r
  private val MatchesPos = """(?s)^(.+?)\s+(\[=matches/[^\]]*\])\s+(.+)$""".r
  private val MatchesType = """^\[=matches/([^\]|]+)""".r

  private val MapExistsPos = """(?si)^(.*?)\s+\[=map/exists=\]$""".r
  private val MapExistsNeg =
    """(?si)^(.*?)\s+(?:\[=map/doesn't exist=\]|doesn't \[=map/exist=\])$""".r
  private val ImplementsPos =
    """(?si)^(.*?)\s+\[=implements=\]\s+\{\{([^}]+)\}\}$""".r
  private val ImplementsNeg =
    """(?si)^(.*?)\s+does not \[=implement=\]\s+\{\{([^}]+)\}\}$""".r
  private val UnreachableStep = """(?si)^this step is not reached$""".r

  // Or has lower precedence than And, so we split by Or first
  private val IsOfFormRhs = """(?si)^of the form (.+)$""".r

  private val CompareOps = Seq(
    " >= ",
    " <= ",
    " ≥ ",
    " ⩾ ",
    " > ",
    " < ",
    " &gt;= ",
    " &lt;= ",
    " &gt; ",
    " &lt; ",
  )
  private val NormalizeOp: Map[String, String] = Map(
    " >= " -> ">=",
    " <= " -> "<=",
    " > " -> ">",
    " < " -> "<",
    " ≥ " -> ">=",
    " ⩾ " -> ">=",
    " &gt;= " -> ">=",
    " &lt;= " -> "<=",
    " &gt; " -> ">",
    " &lt; " -> "<",
  )

  def parse(raw: String): Cond =
    val s = raw.trim.stripSuffix(".")
    // Or has lower precedence than And, so we try it first
    findTopLevel(s, " or ") match
      case Some(i) =>
        Or(
          parse(s.substring(0, i).trim),
          parseOrAbbreviated(s.substring(i + 4).trim),
        )
      case None =>
        findTopLevel(s, " and ") match
          case Some(i) =>
            val left = s.substring(0, i).trim.stripSuffix(",").trim
            val right = s.substring(i + 5).trim
            And(parse(left), parseOrAbbreviated(right))
          case None => parseAtomic(s)

  /** Tries full condition parse; if it falls back to [[Unknown]], attempts to
    * salvage the text as an [[Abbreviated]] when [[ExprParser]] recognises it.
    */
  private def parseOrAbbreviated(s: String): Cond =
    parse(s) match
      case Cond.Unknown(text) =>
        ExprParser.parse(text) match
          case Expr.Unknown(_) => Cond.Unknown(text)
          case expr            => Abbreviated(expr)
      case cond => cond

  private def matchType(link: String): String =
    MatchesType.findFirstMatchIn(link).map(_.group(1)).getOrElse(link)

  private def parseAtomic(s: String): Cond = s match
    case UnreachableStep()     => Unreachable
    case MapExistsPos(baseRaw) => MapExists(ExprParser.parse(baseRaw))
    case MapExistsNeg(baseRaw) =>
      MapExists(ExprParser.parse(baseRaw), negated = true)
    case ImplementsPos(exprRaw, face) =>
      Implements(ExprParser.parse(exprRaw), face)
    case ImplementsNeg(exprRaw, face) =>
      Implements(ExprParser.parse(exprRaw), face, negated = true)
    case _ => parseEqOrCompare(s)

  private def parseEqOrCompare(s: String): Cond =
    def parseIsOfForm(lhsRaw: String, rhsText: String, negated: Boolean): Cond =
      val (formRaw, condOpt) = splitTopLevel(rhsText.trim, " where ") match
        case Some((f, c)) => (f.trim, Some(parse(c.trim)))
        case None         => (rhsText.trim, None)
      IsOfForm(
        ExprParser.parse(lhsRaw),
        ExprParser.parse(formRaw),
        condOpt,
        negated,
      )

    def parseRhs(lhsRaw: String, rhsRaw: String, negated: Boolean): Cond =
      rhsRaw.trim match
        case "missing" => IsMissing(ExprParser.parse(lhsRaw.trim), negated)
        case IsOfFormRhs(text) => parseIsOfForm(lhsRaw.trim, text, negated)
        case _ =>
          Eq(
            ExprParser.parse(lhsRaw.trim),
            ExprParser.parse(rhsRaw.trim),
            negated,
          )

    splitTopLevel(s, " is not equal to ")
      .map {
        case (l, r) =>
          Eq(ExprParser.parse(l.trim), ExprParser.parse(r.trim), negated = true)
      }
      .orElse(splitTopLevel(s, " is equal to ").map {
        case (l, r) => Eq(ExprParser.parse(l.trim), ExprParser.parse(r.trim))
      })
      .orElse(splitTopLevel(s, " is not ").map {
        case (l, r) => parseRhs(l, r, negated = true)
      })
      .orElse(
        splitTopLevel(s, " is ")
          .filter { case (_, r) => !r.trim.startsWith("one of") }
          .map { case (l, r) => parseRhs(l, r, negated = false) },
      )
      .orElse(splitTopLevel(s, " does not equal ").map {
        case (l, r) =>
          Eq(ExprParser.parse(l.trim), ExprParser.parse(r.trim), negated = true)
      })
      .orElse(splitTopLevel(s, " equals ").map {
        case (l, r) => Eq(ExprParser.parse(l.trim), ExprParser.parse(r.trim))
      })
      .orElse(findTopLevelAny(s, CompareOps).map {
        case (i, op) =>
          Compare(
            ExprParser.parse(s.substring(0, i).trim),
            NormalizeOp(op),
            ExprParser.parse(s.substring(i + op.length).trim),
          )
      })
      .getOrElse(s match
        case IsTypeNeg(exprRaw, t) =>
          IsType(ExprParser.parse(exprRaw.trim), t.trim, negated = true)
        case IsTypePos(exprRaw, t) =>
          IsType(ExprParser.parse(exprRaw.trim), t.trim)
        case MatchesNeg(l, link, r) =>
          Matches(
            ExprParser.parse(l.trim),
            matchType(link),
            ExprParser.parse(r.trim),
            negated = true,
          )
        case MatchesPos(l, link, r) =>
          Matches(
            ExprParser.parse(l.trim),
            matchType(link),
            ExprParser.parse(r.trim),
          )
        case _ => Cond.Unknown(s),
      )
