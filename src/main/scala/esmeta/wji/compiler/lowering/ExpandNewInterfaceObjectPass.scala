package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}

// DEAD CODE — not registered in Lowering.pipeline, and the function it emits
// a Perform for (`create_new_object_implementing_the_interface`) doesn't
// exist anywhere in the repo (no extracted algorithm, no manuals/funcs
// stub, no other reference). `Expr.New(iface)` is actually compiled
// directly by `esmeta.wji.compiler.Compiler.compileExpr`
// (`ERecord(iface, ordinaryObjectFields(iface))`) — see `docs/hardcodes.md`
// #7. Left in place as a record of the originally-intended direction (call
// WebIDL's real "internally create a new object implementing the
// interface", webidl/index.bs:13827) for whenever that gets mechanized for
// real; don't wire this back into the pipeline as-is without also fixing
// `create_new_object_implementing_the_interface` itself.
object ExpandNewInterfaceObjectPass extends LoweringPass:
  def run(algos: List[Algorithm]): List[Algorithm] =
    algos.map(a => a.copy(body = transform(a.body)))

  private def transform(instrs: List[Instr]): List[Instr] =
    instrs.flatMap(expandInstr)

  private def expandInstr(instr: Instr): List[Instr] = instr match
    case Instr.Let(Expr.Var(x), Expr.New(iface), body) =>
      val ifaceObj = Expr.Field(
        Expr.Field(Expr.SpecTerm("current Realm"), "Intrinsics"),
        s"%WebAssembly.$iface%",
      )
      List(
        Instr.Perform(
          "create_new_object_implementing_the_interface",
          List(ifaceObj),
          Instr.PerformOutcome.BindResult(x),
          transform(body),
        ),
      )
    case _ =>
      List(instr.mapBody(transform))
