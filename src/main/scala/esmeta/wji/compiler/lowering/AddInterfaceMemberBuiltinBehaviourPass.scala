package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, AlgorithmKind, Cond, Expr, Instr, WjiParam}

/** Reshapes every Getter/Setter/Constructor-kind [[Algorithm]] — 3 of the 4
  * kinds WebIDL calls an interface "member", per `webidl/index.bs`'s own
  * "Members" section ("The constructor steps, getter steps, setter steps, and
  * method steps ... have access to a this value") — into the `<BUILTIN>:`
  * calling convention mainline's own `Call`/`BuiltinCallOrConstruct` machinery
  * expects, the same two fix-ups [[AddBuiltinBehaviourPass]] applies to a
  * hoisted `CreateBuiltinFunction` closure, for the same reason (a
  * calling-convention requirement, not conditional on whether the algorithm
  * itself can abruptly complete):
  *
  * `Method` is deliberately left untouched for now (TODO) — unlike the other 3
  * kinds, a WebIDL method's name isn't unique per interface (`WebAssembly`
  * declares two overloads named `instantiate`, disambiguated only by parameter
  * type, which `AlgorithmExtractor` can't tell apart — see `SpecPatch` #3's
  * rename of one of them), and at least one
  * (`INTRINSICS.WebAssembly.instantiate`) already has a hand-written
  * `manuals/funcs/...ir` glue file claiming that exact intrinsic name for
  * unrelated reasons (WebIDL return-type coercion this pipeline doesn't do
  * generically yet — see `docs/hardcodes.md`). Reshaping `Method` the same way
  * as the other 3 would silently collide with that file. A `Method` algorithm
  * therefore still compiles the old way (plain `AbsOp`, lowercased name — see
  * `Compiler.compileAlgo`), same as before this pass existed.
  *
  *   - '''parameter unpacking''': `BuiltinCallOrConstruct` always invokes a
  *     builtin as `func.__CODE__(this, argumentsList, newTarget)` — a fixed
  *     3-argument shape — regardless of what parameters the algorithm itself
  *     declares (e.g. `Table.get(|index|)`, `Instance(|moduleObject|,
  * |importObject|)`). Every originally-declared `|param|` is unpacked from
  * `ArgumentsList` positionally, mirroring
  * [[AddBuiltinBehaviourPass.unpackArgumentsList]] (same shape, just reading
  * the capitalized `ArgumentsList`/`NewTarget`/`this` names this convention
  * uses — see [[BuiltinParams]] — instead of that pass's lowercase
  * `argumentsList`/`newTarget`/`thisArgument`, a different convention for
  * hoisted closures rather than top-level interface members). `**this**` itself
  * needs no such unpacking: it now compiles directly to the same local the
  * `|this|` parameter declares (see `esmeta.wji.compiler.Compiler`'s
  * `Expr.This` case), so simply declaring `|this|` as a parameter is already
  * enough to bind it — no `Set **this** to |this|.` prefix needed.
  *   - '''WebIDL's implicit setter argument''': a `Setter`-kind algorithm's
  *     `**the given value**` (`Expr.GivenValue`) is WebIDL's other implicit
  *     member-only binding, alongside `**this**` — unpacked from
  *     `ArgumentsList[0]` the same way, since (unlike every declared `|param|`)
  *     it was never a real Bikeshed `|pipe|` variable `extractParams` could
  *     have already found.
  *   - '''Completion-record wrapping''': every exit path must return a real
  *     Completion Record, same as [[AddBuiltinBehaviourPass]]'s own reason
  *     (mainline's Call machinery always expects one back, regardless of
  *     whether the algorithm itself can abruptly complete — see that pass's
  *     doc). Unlike that pass, this one *can* just reuse
  *     [[WrapCompletionReturnsPass]]'s work when it already ran — a member that
  *     transitively calls something abrupt (`Table.length`, `Global.value`) is
  *     already `returnsCompletion = true` and has already been wrapped by it;
  *     only one with no abrupt-completion signal of its own (e.g.
  *     `Instance.exports`, just `return **this**.\[[Exports]]`) still needs
  *     [[CompletionWrapping.expandAlgorithm]] called directly here, or it
  *     wouldn't be wrapped by anything at all.
  *
  * `esmeta.wji.compiler.Compiler.compileAlgo` handles the remaining, genuinely
  * compiler-level half — registering the result under the exact case-preserved
  * name `manuals/intrinsics` references for each kind (e.g.
  * `INTRINSICS.get:WebAssembly.Instance.prototype.exports`,
  * `INTRINSICS.set:WebAssembly.Global.prototype.value`, `INTRINSICS.
  * WebAssembly.Instance`) with `FuncKind.Builtin` — since naming/`FuncKind`
  * aren't things this metalang-level pipeline has any other reason to know
  * about.
  *
  * Category: Structural desugaring — Injection.
  */
object AddInterfaceMemberBuiltinBehaviourPass extends LoweringPass:

  /** Requires:
    *   - [[ExpandFollowingStepsPass]]/[[ExtractInlineAlgoCallPass]]: same
    *     reason [[AddBuiltinBehaviourPass]] needs them before calling the same
    *     [[CompletionWrapping.expandAlgorithm]] utility — its own `Return`
    *     handling assumes a call already sitting in `Instr.Perform` form, not a
    *     raw inline `Expr.AlgoCall`.
    *   - [[WrapCompletionReturnsPass]]: needs its wrapping already applied to a
    *     `returnsCompletion = true` member's body, so this pass can tell that
    *     case apart from one it still has to wrap itself (see class doc).
    */
  override def requires: Set[LoweringPass] = Set(
    ExpandFollowingStepsPass,
    ExtractInlineAlgoCallPass,
    WrapCompletionReturnsPass,
  )

  private val BuiltinParams =
    List(
      WjiParam("|this|"),
      WjiParam("|ArgumentsList|"),
      WjiParam("|NewTarget|"),
    )

  private def stripPipes(s: String): String =
    s.stripPrefix("|").stripSuffix("|")

  /** the `params.zipWithIndex` prefix instructions that unpack `ArgumentsList`
    * positionally into the algorithm's own originally-declared parameter names,
    * defaulting to `undefined` past the end of the list — mirrors
    * [[AddBuiltinBehaviourPass.unpackArgumentsList]] (see that method's own
    * doc); duplicated rather than shared since the two conventions use
    * differently-cased names for the list itself.
    */
  private def unpackArgumentsList(params: List[String]): List[Instr] =
    params.zipWithIndex.map {
      case (p, i) =>
        Instr.IfChain(
          List(
            Cond.Compare(
              Expr.Num(i.toString),
              Cond.CompareOp.Lt,
              Expr.Length(Expr.Var("ArgumentsList")),
            ) -> List(
              Instr.Let(
                Expr.Var(p),
                Expr.Index(Expr.Var("ArgumentsList"), Expr.Num(i.toString)),
              ),
            ),
          ),
          List(Instr.Let(Expr.Var(p), Expr.SpecTerm("undefined"))),
        )
    }

  /** `**the given value**`'s binding, for a `Setter` only — WebIDL passes it as
    * the setter's sole argument, so it's `ArgumentsList[0]`, same shape as
    * [[unpackArgumentsList]] but for a name that was never a declared `|param|`
    * in the first place.
    */
  private def givenValueBinding(kind: AlgorithmKind): List[Instr] = kind match
    case AlgorithmKind.Setter(_) =>
      List(
        Instr.Let(
          Expr.Var("givenValue"),
          Expr.Index(Expr.Var("ArgumentsList"), Expr.Num("0")),
        ),
      )
    case _ => Nil

  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map { a =>
      a.kind match
        case AlgorithmKind.Getter(_) | AlgorithmKind.Setter(_) |
            AlgorithmKind.Constructor(_) =>
          val originalParams = a.params.map(p => stripPipes(p.name))
          val wrappedBody =
            if a.returnsCompletion then a.body
            else CompletionWrapping.expandAlgorithm(a.body)
          a.copy(
            params = BuiltinParams,
            body = unpackArgumentsList(originalParams) ++
              givenValueBinding(a.kind) ++ wrappedBody,
          )
        case AlgorithmKind.Plain | AlgorithmKind.Method(_) => a
    }
