package esmeta.wji.compiler

import esmeta.wji.lang as metalang
import esmeta.wji.lang.{Algorithm, Cond, Instr}
import esmeta.wji.lang.Instr.PerformOutcome
import esmeta.wji.bridge.host.WasmHost
import esmeta.ir
import esmeta.ir.*
import esmeta.ty.*
import esmeta.error.UnreachableAfterLowering

/** Compiles a list of [[metalang.Algorithm]]s into a real ESMeta IR [[Program]]
  * (the same `esmeta.ir` types the ECMA-262 spec compiles to), so compiled WJI
  * functions can be merged into the same `CFG` and run by the same
  * `esmeta.interpreter.Interpreter` as ordinary ES abstract operations.
  *
  * The overall pipeline: Algorithm → Func (one per algorithm) Instr →
  * List[Inst] (may expand to multiple; see If-chain grouping) metalang.Expr /
  * Cond → ir.Expr
  */
object Compiler:

  /** ECMA-262's spec-internal "no meaningful return value" sentinel, mirroring
    * `~unused~` (`esmeta.lang.Expression.EnumLiteral("unused")`, compiled by
    * `esmeta.compiler.Compiler` to the same `EEnum("unused")`). Used wherever a
    * WJI algorithm has nothing to return — see `compileAlgo`'s fall-off-the-end
    * handling and `compileInstr`'s bare `Return.` case.
    */
  private val EUnused = EEnum("unused")
  // A well-known-symbol reference written `%Symbol.NAME%` (e.g.
  // `%Symbol.iterator%`), reaching us as a `SpecTerm` once `ExprParser`
  // strips the `{{...}}` autolink braces. Mirrors mainline ESMeta's
  // `SymbolLiteral(sym) => toERef(GLOBAL_SYMBOL, EStr(sym))`.
  private val SymbolTerm = """^%Symbol\.(.+)%$""".r

  /** Throws immediately for an AST shape that lowering guarantees eliminates
    * before `Compiler` ever runs — unlike `EYet`, which only fails once the IR
    * is actually evaluated. See [[UnreachableAfterLowering]].
    */
  private def impossible(msg: String): Nothing =
    throw UnreachableAfterLowering(msg)

  def compile(algos: List[Algorithm]): Program =
    Program(algos.flatMap(compileAlgo))

  // ── Algorithm → Func ────────────────────────────────────────────────────────

  private def compileAlgo(algo: Algorithm): Option[Func] =
    algo.name.orElse(algo.id).map { name =>
      val params = algo.params
        .map(p => Param(Name(stripPipes(p.name)), optional = p.optional))
      Func(
        main = false,
        kind = FuncKind.AbsOp,
        // lower-cased to match `nameFromLink`: Bikeshed link matching is
        // case-insensitive (e.g. a sentence-initial "Read the imports" links
        // to a dfn written "read the imports"), but Scala map lookups
        // (`cfg.fnameMap`) aren't, so both the registered name and every
        // reference to it are normalized to the same case.
        name = name.toLowerCase,
        params = params,
        retTy = UnknownType,
        // Falling off the end implicitly returns `~unused~` (see
        // `compileInstr`'s `Return(None, ...)` case), mirroring
        // `esmeta.compiler`'s own fall-off-the-end handling.
        //
        // Only appended when the body has no top-level `Return` at all — not
        // just when the last instruction isn't one. An `IfChain`'s branches
        // each reach the function exit directly (harmless dead code after),
        // but sibling `IReturn`s in a flat list share one CFG `Block`, which
        // runs every inst in sequence with no early exit — so appending
        // after an already-unconditional `Return` would silently overwrite
        // its value. Checking "no Return anywhere" (not just "last isn't
        // Return") also turns a spec typo — stray steps left after a
        // `Return` — into a loud `NoReturnValue` instead of silently
        // corrupting the real value.
        body = ISeq(
          if algo.body.exists(_.isInstanceOf[Instr.Return]) then
            compileSeq(algo.body)
          else compileSeq(algo.body) :+ IReturn(EUnused),
        ),
      )
    }

  // ── Instruction sequence ─────────────────────────────────────────────────────

  /** Compiles a flat metalang instruction list into a single [[ISeq]].
    *
    * `If`/`ElseIf`/`Else` siblings are collected and folded into a nested
    * [[IIf]] chain before individual instructions are compiled.
    */
  private def compileInstrs(instrs: List[Instr]): Inst =
    ISeq(compileSeq(instrs))

  private def compileSeq(instrs: List[Instr]): List[Inst] = instrs match
    case Nil           => Nil
    case instr :: rest => compileInstr(instr) ::: compileSeq(rest)

  // ── Single instruction ───────────────────────────────────────────────────────

  private def compileInstr(instr: Instr): List[Inst] = instr match

    case Instr.Let(lhs, expr, body) =>
      val binding: Inst = lhs match
        case metalang.Expr.Var(name)    => ILet(Name(name), compileExpr(expr))
        case metalang.Expr.Tuple(elems) =>
          // ExpandDestructuringLetPass unconditionally rewrites every
          // Tuple-lhs Let before compilation ever sees it, and no later pass
          // synthesizes one — see UnreachableAfterLowering's doc.
          impossible(
            s"Tuple-lhs Let not eliminated by ExpandDestructuringLetPass: $lhs",
          )
        case _ => ILet(Name("_"), EYet(s"unsupported Let lhs: $lhs"))
      binding :: compileSeq(body)

    case Instr.Set(lhs, expr, body) =>
      IAssign(compileRef(lhs), compileExpr(expr)) :: compileSeq(body)

    case Instr.Return(Some(expr), body) =>
      compileSeq(body) :+ IReturn(compileExpr(expr))

    case Instr.Return(None, body) =>
      // Bare "Return." — same as falling off the end (see `compileAlgo`);
      // see `EUnused`.
      compileSeq(body) :+ IReturn(EUnused)

    case Instr.Assert(cond, body) =>
      IAssert(compileCond(cond)) :: compileSeq(body)

    case Instr.Throw(target, body) =>
      compileSeq(body) :+ IExpr(EYet(s"throw $target")) // TODO: proper throw

    case Instr.While(cond, body) =>
      IWhile(compileCond(cond), compileInstrs(body)) :: Nil

    case Instr.ForEach(elem, collection, body) =>
      // TODO: proper foreach — needs ERef of collection + loop var
      IExpr(
        EYet(
          s"foreach ${metalang.ExprPrinter
            .render(elem)} in ${metalang.ExprPrinter.render(collection)}",
        ),
      ) :: compileSeq(body)

    case Instr.For(elem, collection, body) =>
      // TODO: proper counting loop — needs a real IWhile over the collection
      IExpr(
        EYet(
          s"for ${metalang.ExprPrinter
            .render(elem)} in ${metalang.ExprPrinter.render(collection)}",
        ),
      ) :: compileSeq(body)

    case Instr.Perform(func, args, outcome, body) =>
      val callArgs = args.map(compileExpr)
      val name = nameFromLink(func)
      val bodyInsts = compileSeq(body)
      // a Wasm Embedding function (e.g. `module_imports`) isn't a WJI/ES
      // function in `cfg.fnameMap` — it's dispatched to the live WasmHost via
      // a dedicated IR node instead of an ordinary closure call.
      def mkCall(lhs: Name): Inst =
        if WasmHost.names.contains(name) then ICallEmbed(lhs, name, callArgs)
        else ICall(lhs, EClo(name, Nil), callArgs)
      outcome match
        case PerformOutcome.Discard =>
          mkCall(Name("_")) :: bodyInsts
        case PerformOutcome.BindResult(v) =>
          mkCall(Name(stripPipes(v))) :: bodyInsts
        case PerformOutcome.ReturnResult =>
          // ExpandPerformReturnResultPass rewrites every ReturnResult-outcome
          // Instr.Perform before compilation ever sees it (unlike the
          // superficially similar PerformClosure case below, which is
          // ExpandClosureCallPass's normal output shape, not eliminated by
          // any pass) — see UnreachableAfterLowering's doc.
          impossible(
            s"Instr.Perform's ReturnResult outcome not eliminated by ExpandPerformReturnResultPass: $func",
          )

    case Instr.PerformClosure(closure, args, outcome, body) =>
      val callArgs = args.map(compileExpr)
      val calleeExpr = compileExpr(closure)
      val bodyInsts = compileSeq(body)
      // unlike Instr.Perform's mkCall, the callee is a runtime value (e.g. a
      // |var| parameter), never a WasmHost embedding-function name, so this
      // always emits ICall — never ICallEmbed.
      def mkCall(lhs: Name): Inst = ICall(lhs, calleeExpr, callArgs)
      outcome match
        case PerformOutcome.Discard =>
          mkCall(Name("_")) :: bodyInsts
        case PerformOutcome.BindResult(v) =>
          mkCall(Name(stripPipes(v))) :: bodyInsts
        case PerformOutcome.ReturnResult =>
          // NOT dead code, despite looking like Instr.Perform's eliminated
          // case above: ExpandClosureCallPass always rewrites
          // `Return(Some(ClosureCall(...)))` directly into this shape, so
          // this is the normal, always-exercised path for a returned closure
          // call, not a fallback for something a pass failed to remove.
          val tmp = Name("_ret")
          bodyInsts :+ mkCall(tmp) :+ IReturn(ERef(tmp))

    case Instr.Append(item, collection, body) =>
      IPush(
        compileExpr(item),
        compileExpr(collection),
        front = false,
      ) :: compileSeq(body)

    case Instr.Continue(_) =>
      IExpr(EYet("continue")) :: Nil // TODO: represent loop continue

    case Instr.RunInParallel(body) =>
      compileSeq(body) // Parallele steps are simply executed immediately

    case Instr.Note(_, _) => Nil // notes are informational only

    case Instr.Unknown(text, body) =>
      IExpr(EYet(text)) :: compileSeq(body)

    case Instr.IfChain(branches, fallback) =>
      def buildChain(bs: List[(Cond, List[Instr])]): Inst = bs match
        case Nil => if fallback.isEmpty then INop() else compileInstrs(fallback)
        case (c, b) :: rest =>
          IIf(compileCond(c), compileInstrs(b), buildChain(rest))
      buildChain(branches) :: Nil

    case i @ (_: Instr.If | _: Instr.ElseIf | _: Instr.Else) =>
      // GroupIfChainPass folds every If/ElseIf/Else sibling group into a
      // single IfChain before compilation ever sees it, unconditionally and
      // recursively — see UnreachableAfterLowering's doc.
      impossible(s"bare If/ElseIf/Else not grouped by GroupIfChainPass: $i")

  // ── Expression ───────────────────────────────────────────────────────────────

  private def compileExpr(expr: metalang.Expr): ir.Expr = expr match
    case metalang.Expr.Var(name)             => ERef(Name(name))
    case metalang.Expr.This                  => ERef(Global("this"))
    case metalang.Expr.Num(s)                => compileNum(s)
    case metalang.Expr.Byte(v)               => ENumber(v.toDouble)
    case metalang.Expr.Bool(b)               => EBool(b)
    case metalang.Expr.Str(s)                => EStr(s)
    case metalang.Expr.SpecTerm("null")      => ENull()
    case metalang.Expr.SpecTerm("undefined") => EUndef()
    // "the current Realm Record" (ECMA-262 9.4 Execution Contexts): the Realm
    // component of the running execution context, i.e. the top frame of the
    // execution context stack. Mirrors `esmeta.compiler.currentRealm`.
    case metalang.Expr.SpecTerm("current Realm") =>
      ERef(Field(GLOBAL_CONTEXT, EStr("Realm")))
    // "the surrounding agent" (ECMA-262 9.7 Agents): mainline ESMeta already
    // models this as the `AGENT_RECORD` global heap record (see
    // `esmeta.compiler.Compiler`'s `AgentRecord()` case, `GLOBAL_AGENT_RECORD`
    // in `esmeta.ir.package`). The Wasm JS-API spec extends that same Agent
    // Record with several of its own slots via plain prose rather than a
    // `<div algorithm>` (e.g. `[=associated store=]`, index.bs:334: "Each
    // agent has an associated store... set to store_init()") — accessed
    // throughout the spec as `the [=surrounding agent=]'s [=X=]`, which
    // `ExprParser` already parses as `Field(SpecTerm("surrounding agent"), X)`
    // the same as a `.[[slot]]` access, so mapping the base here covers every
    // such field uniformly.
    case metalang.Expr.SpecTerm("surrounding agent") =>
      ERef(GLOBAL_AGENT_RECORD)
    case metalang.Expr.SpecTerm(SymbolTerm(sym)) =>
      ERef(Field(GLOBAL_SYMBOL, EStr(sym)))
    case metalang.Expr.SpecTerm(s) => EEnum(s)
    case metalang.Expr.Field(base, name) =>
      ERef(Field(compileRef(base), EStr(name)))
    case metalang.Expr.Index(base, key) =>
      ERef(Field(compileRef(base), compileExpr(key)))
    case metalang.Expr.New(iface)      => ERecord(iface, Nil)
    case metalang.Expr.List_(elems)    => EList(elems.map(compileExpr))
    case metalang.Expr.Length(e)       => ESizeOf(compileExpr(e))
    case metalang.Expr.BinOp(l, op, r) => compileBinOp(op, l, r)
    case metalang.Expr.Pow(base, exp) =>
      EBinary(BOp.Pow, compileExpr(base), compileExpr(exp))
    case metalang.Expr.Neg(e)      => EUnary(UOp.Neg, compileExpr(e))
    case metalang.Expr.AsMath(e)   => compileExpr(e)
    case metalang.Expr.AsNumber(e) => EConvert(COp.ToNumber, compileExpr(e))
    // ExpandAbruptPass expands every `?`/`!` in the direct-RHS position of
    // Let/Set/Return into real completion-record inspection before
    // compilation ever sees it; this only remains for a *nested* occurrence
    // (e.g. `?`/`!` used as an argument to another call), which that pass
    // doesn't yet reach — TODO: generalize ExpandAbruptPass beyond top-level
    // Let/Set/Return RHS, the way mainline's `returnIfAbrupt` handles it at
    // any expression depth.
    case metalang.Expr.Abrupt(marker, e) =>
      EYet(s"nested $marker-abrupt: $e")
    case metalang.Expr.AlgoCall(link, args) =>
      // zero-arg AlgoCall used as an expression value (e.g. [=error=])
      if args.isEmpty then ERef(Global(nameFromLink(link)))
      else EYet(s"call $link") // TODO: inline call-as-expr
    // a SpecTec Wasm Core Spec constructor/variant application (e.g. "the
    // [=external value=] [=external value/func=] |funcaddr|") used as a
    // value — builds a real `Wasm(CaseV(tag, ...))` to send back to SpecTec.
    // `tag` needs SpecTec's own runtime constructor id (e.g. `FUNC`), a
    // different namespace than `nameFromLink`'s WJI-algorithm-name lowercase
    // convention — see `metalang.Expr.runtimeCaseTag`.
    case metalang.Expr.Case(tag, args) =>
      ECase(metalang.Expr.runtimeCaseTag(tag), args.map(compileExpr))
    case metalang.Expr.Tuple(elems) => EYet(s"tuple(${elems.mkString})") // TODO
    case metalang.Expr.Map_(entries) =>
      EMap(
        (UnknownType, UnknownType),
        entries.map((k, v) => (compileExpr(k), compileExpr(v))),
      )
    case metalang.Expr.UnknownNew(raw) => EYet(raw)
    case metalang.Expr.Described(link, desc) =>
      EYet(s"$link which $desc") // TODO: relative-clause construction
    case metalang.Expr.SuchThat(desc, cond) =>
      EYet(
        s"$desc such that $cond",
      ) // TODO: existential/definite-description search
    case metalang.Expr.Unknown(raw) => EYet(raw)
    case metalang.Expr.Closure(name, captured) =>
      EClo(name, captured.map(Name(_)))
    case metalang.Expr.TupleProj(base, idx) => EProj(compileExpr(base), idx)
    case metalang.Expr.CaseTag(base)        => ECaseTag(compileExpr(base))

    // ── unreachable after lowering ──
    // Every shape below is guaranteed gone by the time `Compiler` runs —
    // each eliminated unconditionally by its own named pass (`ResolveLinksPass`,
    // `ExpandNewByteSequencePass`, `ExpandIndexOfPass`, `ExpandFollowingStepsPass`)
    // or, for `JSCall`/nested `ClosureCall`, simply never produced by any spec
    // text seen so far. Grouped here, all crashing via `impossible`, rather than
    // interleaved with the genuine `EYet` "not yet implemented" cases above —
    // confirmed empirically by converting every candidate to `impossible` and
    // recompiling the whole corpus (`sbt testOnly esmeta.wji.SnapshotSpec`),
    // one at a time, to see which ones actually fire.
    case metalang.Expr.Link(link, _) => impossible(s"unresolved link: $link")
    case metalang.Expr.JSCall(name, args) =>
      impossible(s"$$${name}(${args.mkString})")
    case metalang.Expr.NewByteSequence(length) =>
      impossible(s"new byte sequence of length ${length}")
    case metalang.Expr.Range(low, high) =>
      impossible(s"range ${low} to ${high}")
    case metalang.Expr.IndexOf(list, elem) =>
      impossible(s"index of ${list} where ${elem} is found")
    case metalang.Expr.FollowingSteps(params) =>
      impossible(
        s"unlowered following-steps closure given ${params.mkString(", ")}",
      )
    case metalang.Expr.ClosureCall(closure, args) =>
      impossible(
        s"call ${metalang.ExprPrinter.render(closure)} given ${args
          .map(metalang.ExprPrinter.render)
          .mkString(", ")}",
      )

  // ── Condition ────────────────────────────────────────────────────────────────

  private def compileCond(cond: Cond): ir.Expr = cond match
    case Cond.Eq(l, r, false) => EBinary(BOp.Eq, compileExpr(l), compileExpr(r))
    case Cond.Eq(l, r, true) =>
      EUnary(UOp.Not, EBinary(BOp.Eq, compileExpr(l), compileExpr(r)))
    case Cond.Compare(l, op, r) =>
      compileCompare(op, compileExpr(l), compileExpr(r))
    case Cond.And(l, r) => EBinary(BOp.And, compileCond(l), compileCond(r))
    case Cond.Or(l, r)  => EBinary(BOp.Or, compileCond(l), compileCond(r))
    case Cond.HasField(e, false)  => EExists(compileRef(e))
    case Cond.HasField(e, true)   => EUnary(UOp.Not, EExists(compileRef(e)))
    case Cond.IsType(e, t, false) => ETypeCheck(compileExpr(e), irTypeOf(t))
    case Cond.IsType(e, t, true) =>
      EUnary(UOp.Not, ETypeCheck(compileExpr(e), irTypeOf(t)))
    case Cond.Abbreviated(e)      => compileExpr(e)
    case Cond.Unreachable         => EBool(false)
    case Cond.IsMissing(e, false) => EUnary(UOp.Not, EExists(compileRef(e)))
    case Cond.IsMissing(e, true)  => EExists(compileRef(e))
    case Cond.HasSlot(e, slot, false) =>
      EExists(Field(compileRef(e), EStr(slot)))
    case Cond.HasSlot(e, slot, true) =>
      EUnary(UOp.Not, EExists(Field(compileRef(e), EStr(slot))))
    case Cond.Contains(elem, list, false) =>
      EContains(compileExpr(list), compileExpr(elem))
    case Cond.Contains(elem, list, true) =>
      EUnary(UOp.Not, EContains(compileExpr(list), compileExpr(elem)))
    case Cond.Implements(e, iface, neg) => EYet(s"implements $iface") // TODO
    case Cond.IsOfForm(e, f, _, neg)    => EYet(s"is of form $f") // TODO
    case Cond.Matches(l, t, r, neg)     => EYet(s"matches $t") // TODO
    case Cond.Unknown(raw)              => EYet(raw)

    // ── unreachable after lowering ──
    // See compileExpr's identically-named, identically-verified group above:
    // `ExpandHasDuplicatesPass`/`ExpandMatchesExistsPass`/`ExpandThrowsPass`
    // each intend to eliminate every occurrence (not just the narrow shape
    // their own doc comments enumerate), and empirically do, for every
    // algorithm in the corpus today.
    case Cond.HasDuplicates(e, neg) => impossible("contains duplicates")
    case Cond.Exists(binder, _, _)  => impossible(s"exists $binder")
    case Cond.Throws(kind) =>
      impossible(s"throws${kind.fold("")(k => s" $k")}")

  // ── Ref ──────────────────────────────────────────────────────────────────────

  /** Converts a metalang Expr that appears in a writable position to an IR Ref.
    */
  private def compileRef(expr: metalang.Expr): Ref = expr match
    case metalang.Expr.Var(name)                     => Name(name)
    case metalang.Expr.This                          => Global("this")
    case metalang.Expr.SpecTerm("surrounding agent") => GLOBAL_AGENT_RECORD
    case metalang.Expr.SpecTerm("current Realm") =>
      Field(GLOBAL_CONTEXT, EStr("Realm"))
    case metalang.Expr.Field(base, name) => Field(compileRef(base), EStr(name))
    case metalang.Expr.Index(base, key) =>
      Field(compileRef(base), compileExpr(key))
    case metalang.Expr.AlgoCall(link, Nil) => Global(nameFromLink(link))
    case other => Name(s"@@yet: unresolved ref: $other")

  // ── Helpers ──────────────────────────────────────────────────────────────────

  private def compileNum(s: String): ir.Expr =
    val norm = s.replace("−", "-")
    val (neg, raw) =
      if norm.startsWith("-") then (true, norm.drop(1)) else (false, norm)
    val bd =
      if raw.startsWith("0x") then
        BigDecimal(java.lang.Long.parseLong(raw.drop(2), 16))
      else BigDecimal(raw)
    EMath(if neg then -bd else bd)

  private def nameFromLink(link: String): String =
    val name = link.stripPrefix("[=").stripSuffix("=]").trim
    // only `[=...=]` WJI links are case-insensitive by Bikeshed convention;
    // a bare name (as JSCall's `[$...$]` extracts it, with no brackets here)
    // is an exact ECMA-262 AO name and must keep its case to match
    // `cfg.fnameMap`.
    if link.startsWith("[=") then name.toLowerCase else name

  private def stripPipes(s: String): String =
    s.stripPrefix("|").stripSuffix("|")

  /** `Cond.IsType`'s type-name strings, mapped to the matching `esmeta.ty`
    * singletons mainline itself uses for the same ECMA-262 `Type(x)` categories
    * (`esmeta.compiler.Compiler`'s `TypeCheckCondition` case).
    * `AbruptCompletion` isn't itself a `Type(x)` category — it's
    * `ExpandAbruptPass`'s check for whether a value is an abrupt completion —
    * but mainline already models that too, as `ty.AbruptT`.
    */
  private val typeOf: Map[String, ValueTy] = Map(
    "Object" -> ObjectT,
    "BigInt" -> BigIntT,
    "Number" -> NumberT,
    "String" -> StrT,
    "AbruptCompletion" -> AbruptT,
  )

  private def irTypeOf(name: String): ir.Type =
    ir.Type(typeOf.getOrElse(name, UnknownTy(Some(name))))

  private def compileBinOp(
    op: metalang.Expr.BOp,
    l: metalang.Expr,
    r: metalang.Expr,
  ): ir.Expr =
    val bop = op match
      case metalang.Expr.BOp.Add => BOp.Add
      case metalang.Expr.BOp.Sub => BOp.Sub
      case metalang.Expr.BOp.Mul => BOp.Mul
      case metalang.Expr.BOp.Div => BOp.Div
      case metalang.Expr.BOp.Mod => BOp.Mod
    EBinary(bop, compileExpr(l), compileExpr(r))

  /** ESMeta's `BOp` only has `Lt` among ordering operators (no `Le`/`Gt`/`Ge`,
    * unlike WJI's own former operator set), so the other three are desugared
    * here via negation / operand-swap: `l<=r` is `!(r<l)`, `l>r` is `r<l`,
    * `l>=r` is `!(l<r)`.
    */
  private def compileCompare(
    op: Cond.CompareOp,
    l: ir.Expr,
    r: ir.Expr,
  ): ir.Expr =
    op match
      case Cond.CompareOp.Lt => EBinary(BOp.Lt, l, r)
      case Cond.CompareOp.Le => EUnary(UOp.Not, EBinary(BOp.Lt, r, l))
      case Cond.CompareOp.Gt => EBinary(BOp.Lt, r, l)
      case Cond.CompareOp.Ge => EUnary(UOp.Not, EBinary(BOp.Lt, l, r))
