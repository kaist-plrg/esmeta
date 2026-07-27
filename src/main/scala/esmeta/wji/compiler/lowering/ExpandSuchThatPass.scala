package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Expr, Instr}

/** Expands the "inverse of signed_N" shape of `Expr.SuchThat` (e.g. "Let
  * |u32| be the unsigned integer such that |i32| is [=signed_32=](|u32|).",
  * `ToWebAssemblyValue`'s i32/i64/i31 cases, index.bs:1425/1429/1458) into a
  * direct call to `inv_signed_N` (`WasmHost.paramNames`'s
  * `inv_signed_31`/`32`/`64` entries, `server.ml`'s `call_inv_signed`) — "the
  * unsigned integer such that |i32| is [=signed_32=](|u32|)" describes exactly
  * `inv_signed_32(|i32|)` (`3.1-numerics.scalar.spectec`'s own `hint(inverse
  * ...)` on `$signed_`/`$inv_signed_` says as much), so this needs no runtime
  * search — just calling the already-known inverse function directly:
  * {{{
  *   Let(u32, SuchThat("unsigned integer", "|i32| is [=signed_32=](|u32|)"))
  * }}}
  * becomes
  * {{{
  *   Let(u32, AlgoCall("inv_signed_32", [Var(i32)]))
  * }}}
  * — left as a plain `AlgoCall`-with-args rather than expanded further here, so
  * [[ExtractInlineAlgoCallPass]] (which already handles exactly this `Let(x,
  * AlgoCall(f, args), body)` shape generically, turning it into `Perform(f,
  * args, BindResult(x), body)`) does the rest; that's also why this pass must
  * run before it.
  *
  * Only fires when the `such that` clause's own bound variable (the `Let` it's
  * the RHS of) is exactly the variable `signed_N` is applied to inside the
  * condition — true of every occurrence in the corpus today, and the only shape
  * that's unambiguously "solve for this one variable" rather than some other
  * existential. Every other `Expr.SuchThat` shape — implementation-defined
  * choices constrained to a range (the NaN-payload cases, index.bs:1434/1442)
  * and existence/smallest-element searches (index.bs:1469/1471) — is left
  * untouched, same as before: `Compiler` reports it as `EYet(s"$desc such that
  * $cond")`. See `personal/TODO.md` #2.
  *
  * Category: SpecTec dependent.
  */
object ExpandSuchThatPass extends LoweringPass:

  /** Requires: nothing — `Expr.SuchThat`'s `cond` is raw prose text, untouched
    * by any earlier pass (unlike a real `Expr`/`Cond` tree, nothing resolves
    * links inside it), so this can run anywhere before
    * [[ExtractInlineAlgoCallPass]] needs to see the `AlgoCall` it produces.
    */
  override def requires: Set[LoweringPass] = Set.empty
  override def mustPrecede: Set[LoweringPass] = Set(ExtractInlineAlgoCallPass)

  // "|i32| is [=signed_32=](|u32|)" — the value being inverted, the bit
  // width, and the variable `signed_N` is applied to, in that order.
  private val SignedInverseCond =
    """(?s)^\|([^|]+)\|\s+is\s+\[=signed_(31|32|64)=\]\(\|([^|]+)\|\)$""".r

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = transform(a.body)))

  private def transform(instrs: List[Instr]): List[Instr] =
    instrs.map {
      case Instr.Let(
            lhs @ Expr.Var(lhsName),
            Expr.SuchThat(desc, cond),
            body,
          ) =>
        val newRhs = cond.trim match
          case SignedInverseCond(valueVar, bits, targetVar)
              if targetVar.trim == lhsName =>
            // `inv_signed_N`'s declared parameter type is `int` (a
            // mathematical integer, AL `Int`), but the value being inverted
            // here (e.g. ToInt32's result) is a plain ECMAScript `Number`
            // (AL `Real` once it crosses `WasmHost` — see `Interpreter.toAL`)
            // — `AsMath` (ECMA-262's ℝ(number), `EConvert(ToMath, ...)`)
            // bridges that, matching `inv_signed_`'s own `int`-typed second
            // parameter (`3.1-numerics.scalar.spectec`).
            Expr.AlgoCall(
              s"inv_signed_$bits",
              List(Expr.AsMath(Expr.Var(valueVar.trim))),
            )
          case _ => Expr.SuchThat(desc, cond)
        Instr.Let(lhs, newRhs, transform(body))
      case other => other.mapBody(transform)
    }
