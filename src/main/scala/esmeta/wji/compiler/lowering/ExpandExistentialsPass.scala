package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}

/** Expands "existential description" constructs — `Expr.SuchThat` ("the VALUE
  * such that COND", value-producing) and `Cond.Exists` ("a VALUE exists such
  * that COND", boolean-producing) — that don't need a real runtime search
  * despite the phrasing, because the shapes this pass recognizes each solve
  * algebraically instead:
  *
  *   - The "inverse of signed_N" shape (e.g. "Let |u32| be the unsigned integer
  *     such that |i32| is [=signed_32=](|u32|).", `ToWebAssemblyValue`'s
  *     i32/i64/i31 cases, index.bs:1425/1429/1458) becomes a direct call to
  *     `inv_signed_N` (`WasmHost.paramNames`'s `inv_signed_31`/`32`/`64`
  *     entries, `server.ml`'s `call_inv_signed`) — "the unsigned integer such
  *     that |i32| is [=signed_32=](|u32|)" describes exactly
  *     `inv_signed_32(|i32|)` (`3.1-numerics.scalar.spectec`'s own
  *     `hint(inverse ...)` on `$signed_`/`$inv_signed_` says as much):
  * {{{
  *       Let(u32, SuchThat("unsigned integer", Eq(Var(i32), AlgoCall("[=signed_32=]", [Var(u32)]))))
  * }}}
  * becomes
  * {{{
  *       Let(u32, AlgoCall("inv_signed_32", [Var(i32)]))
  * }}}
  * — left as a plain `AlgoCall`-with-args rather than expanded further here, so
  * [[ExpandInlineAlgoCallPass]] (which already handles exactly this `Let(x,
  * AlgoCall(f, args), body)` shape generically, turning it into `Perform(f,
  * args, BindResult(x), body)`) does the rest; that's also why this pass must
  * run before it. `signed_N`'s own link resolves to [[Expr.AlgoCall]] rather
  * than [[Expr.Case]] here (unlike most bracket-args links this pass sees
  * elsewhere) because it's a genuinely-extracted WJI algorithm name, not just a
  * SpecTec variant tag — confirmed via [[ResolveLinksPass]]'s own `known`-name
  * branch, not its `Case`/`AlgoCall` fallback heuristic.
  *
  *   - The "smallest missing map key" shape (e.g. "Let [=host address=]
  * |hostaddr| be the smallest address such that |map|[|hostaddr|]
  * [=map/exists=] is false.", index.bs:1471, the [=host value cache=]'s own
  * allocator) becomes the map's size directly — this map is only ever grown by
  * this very rule (fill the smallest missing address) and never removed from
  * anywhere in the spec, so its key domain is always exactly `0..size-1`; the
  * smallest missing address is therefore just its current size (`Obj.size`'s
  * `MapObj` case), with no search needed:
  * {{{
  *       Let(hostaddr, SuchThat("smallest address", HasField(Index(Var(map), Var(hostaddr)), negated = true)))
  * }}}
  * becomes
  * {{{
  *       Let(hostaddr, Length(Var(map)))
  * }}}
  *
  *   - The "does some key map to this value" shape (e.g. "If a [=host address=]
  * |hostaddr| exists such that |map|[|hostaddr|] is the same as |v|,",
  * index.bs:1469, the same [=host value cache=]'s reverse lookup) reuses the
  * very same contiguous-key-domain fact as the previous case, but the other
  * direction: instead of computing a value directly, it needs a genuine (if
  * trivially bounded) search — walk `0..size-1` looking for a match, since
  * nothing shorter can decide existence. `binder` doubles as both the loop
  * counter and, once found, the result the original body goes on to reference
  * (e.g. this construct's own `Return [=ref.host=]
  * |hostaddr|.` right after):
  * {{{
  *       If(Exists(hostaddr, Eq(Index(Var(map), Var(hostaddr)), Var(v))))
  *         ...body referencing hostaddr...
  * }}}
  * becomes
  * {{{
  *       Let(_found1, false)
  *       Let(hostaddr, 0)
  *       While(_found1 = false and hostaddr < length(map)) {
  *         If(map[hostaddr] = v) { Set(_found1, true) }
  *         Else { Set(hostaddr, hostaddr + 1) }
  *       }
  *       If(_found1 = true)
  *         ...body referencing hostaddr...
  * }}}
  * — the same found-accumulator idiom [[ExpandMatchesExistsPass]] uses for
  * `Cond.Any`'s explicit-collection search, but hand-built here rather than
  * shared: `Cond.Any` always knows its collection(s) up front and binds
  * `binder` to *elements*, so its hoist is a mechanical "materialize a known
  * collection into a loop"; `Cond.Exists` states no collection at all —
  * discovering *what* to search (here, that `body` indexes a map by `binder`
  * itself, so the search domain is that map's own contiguous key range) is
  * exactly the kind of structural pattern recognition this pass already does
  * for `Expr.SuchThat`, not a mechanical materialization.
  *
  * All three only fire for the one narrow shape actually seen in the corpus
  * today — the `SuchThat` cases require the clause's own bound variable (the
  * `Let` it's the RHS of) be exactly the variable the condition constrains; the
  * `Exists` case requires `body` be exactly an equality test against a
  * `map[binder]` read. Every other shape — implementation-defined choices
  * constrained to a range (the NaN-payload `SuchThat` cases, index.bs:1434/
  * 1442) and any other kind of `Exists` — is left untouched: `Compiler` reports
  * it as `EYet`.
  *
  * Category: Spec-dependent — SpecTec.
  */
object ExpandExistentialsPass extends LoweringPass:

  /** Requires:
    *   - [[ResolveLinksPass]]: `Expr.SuchThat`'s `cond` is a real [[Cond]] as
    *     of `ExprParser`/`CondParser` (parsed together, `Expr.SuchThat`'s own
    *     doc), but any link inside it (e.g. `signed_32`) is still a raw
    *     `Expr.Link` until this runs — all patterns below match on the resolved
    *     `Expr.AlgoCall`/`Cond.HasField` shape, not `Link`.
    *   - [[GroupIfChainPass]]: the `Cond.Exists` hoist matches on
    *     `Instr.IfChain`, not a raw `If`.
    */
  override def requires: Set[LoweringPass] =
    Set(ResolveLinksPass, GroupIfChainPass)
  override def mustPrecede: Set[LoweringPass] = Set(ExpandInlineAlgoCallPass)

  // "[=signed_31=]" / "[=signed_32=]" / "[=signed_64=]" — the bit width.
  private val SignedLink = """(?s)^\[=signed_(31|32|64)=\]$""".r

  /** Generates this pass's `_foundN` names for a single algorithm. Scoped as a
    * value local to each [[run]] iteration rather than a mutable field on this
    * `object` — the latter is JVM-wide singleton state, so concurrent `run`
    * calls (e.g. multiple ScalaTest suites compiling algorithms in parallel,
    * which sbt's default `Test / parallelExecution` allows) would race on
    * incrementing/resetting a shared counter, producing nondeterministic naming
    * depending on thread interleaving.
    */
  private class Counter:
    private var n = 0
    def fresh(prefix: String): String = { n += 1; s"_$prefix$n" }

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map { a =>
      val counter = Counter()
      a.copy(body = transform(a.body, counter))
    }

  private def transform(instrs: List[Instr], counter: Counter): List[Instr] =
    instrs.flatMap(expandInstr(_, counter))

  private def expandInstr(instr: Instr, counter: Counter): List[Instr] =
    instr match
      case Instr.Let(
            lhs @ Expr.Var(lhsName),
            Expr.SuchThat(desc, cond),
            body,
          ) =>
        val newRhs = cond match
          case Cond.Eq(
                Expr.Var(valueVar),
                Expr.AlgoCall(SignedLink(bits), List(Expr.Var(targetVar))),
                false,
              ) if targetVar == lhsName =>
            // `inv_signed_N`'s declared parameter type is `int` (a
            // mathematical integer, AL `Int`), but the value being inverted
            // here (e.g. ToInt32's result) is a plain ECMAScript `Number`
            // (AL `Real` once it crosses `WasmHost` — see `Interpreter.toAL`)
            // — `AsMath` (ECMA-262's ℝ(number), `EConvert(ToMath, ...)`)
            // bridges that, matching `inv_signed_`'s own `int`-typed second
            // parameter (`3.1-numerics.scalar.spectec`).
            Expr.AlgoCall(
              s"inv_signed_$bits",
              List(Expr.AsMath(Expr.Var(valueVar))),
            )
          case Cond.HasField(Expr.Index(mapExpr, Expr.Var(indexVar)), true)
              if indexVar == lhsName =>
            // the map is only ever grown by this same rule (fill the
            // smallest missing address, never removed from — see
            // index.bs's own use of `host value cache`), so its key domain
            // is always exactly 0..size-1; the smallest missing address is
            // therefore just its current size, with no search needed.
            Expr.Length(mapExpr)
          case _ => Expr.SuchThat(desc, cond)
        List(Instr.Let(lhs, newRhs, transform(body, counter)))
      case Instr.Assert(cond, body) if needsHoist(cond) =>
        val (pre, simplified) = hoist(cond, counter)
        pre :+ Instr.Assert(simplified, transform(body, counter))
      case Instr.While(cond, body) if needsHoist(cond) =>
        val (pre, simplified) = hoist(cond, counter)
        pre :+ Instr.While(simplified, transform(body, counter))
      case Instr.IfChain(List((cond, body)), fallback) if needsHoist(cond) =>
        val (pre, simplified) = hoist(cond, counter)
        pre :+ Instr.IfChain(
          List((simplified, transform(body, counter))),
          transform(fallback, counter),
        )
      case other => List(other.mapBody(transform(_, counter)))

  private def needsHoist(cond: Cond): Boolean = cond match
    case Cond.Exists(binder, Cond.Eq(Expr.Index(_, Expr.Var(iv)), _, false)) =>
      iv == binder
    case Cond.And(l, r) => needsHoist(l) || needsHoist(r)
    case Cond.Or(l, r)  => needsHoist(l) || needsHoist(r)
    case _              => false

  /** Returns the instructions to run first, and a pure replacement condition
    * (no known-hoistable `Exists` left) to check afterward.
    */
  private def hoist(cond: Cond, counter: Counter): (List[Instr], Cond) =
    cond match
      case Cond.Exists(
            binder,
            Cond.Eq(Expr.Index(mapExpr, Expr.Var(indexVar)), value, false),
          ) if indexVar == binder =>
        val found = counter.fresh("found")
        val pre = List(
          Instr.Let(Expr.Var(found), Expr.Bool(false)),
          Instr.Let(Expr.Var(binder), Expr.Num("0")),
          Instr.While(
            Cond.And(
              Cond.Eq(Expr.Var(found), Expr.Bool(false)),
              Cond.Compare(
                Expr.Var(binder),
                Cond.CompareOp.Lt,
                Expr.Length(mapExpr),
              ),
            ),
            List(
              Instr.IfChain(
                List(
                  (
                    Cond.Eq(Expr.Index(mapExpr, Expr.Var(binder)), value),
                    List(Instr.Set(Expr.Var(found), Expr.Bool(true))),
                  ),
                ),
                List(
                  Instr.Set(
                    Expr.Var(binder),
                    Expr.BinOp(Expr.Var(binder), Expr.BOp.Add, Expr.Num("1")),
                  ),
                ),
              ),
            ),
          ),
        )
        (pre, Cond.Eq(Expr.Var(found), Expr.Bool(true)))
      case Cond.And(l, r) =>
        val (lp, lc) = hoist(l, counter)
        val (rp, rc) = hoist(r, counter)
        (lp ++ rp, Cond.And(lc, rc))
      case Cond.Or(l, r) =>
        val (lp, lc) = hoist(l, counter)
        val (rp, rc) = hoist(r, counter)
        (lp ++ rp, Cond.Or(lc, rc))
      case other => (Nil, other)
