package esmeta.wji.compiler.lowering

import esmeta.wji.lang.{Algorithm, Cond, Expr, Instr}

object Tmp extends LoweringPass:
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
