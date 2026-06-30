package esmeta.wji.interpreter

import esmeta.cfg.CFG
import esmeta.interpreter.{Interpreter => EsInterpreter}
import esmeta.ir.Local
import esmeta.state.*
import esmeta.wji.state.WjValue
import scala.collection.mutable.{Map => MMap}

/** Calls an ESMeta IR abstract operation (e.g. `ToBoolean`) from WJI IR.
  *
  * Unlike the Wasm embedding ([[esmeta.wji.bridge.host.WasmHost]]), ESMeta IR
  * functions run in-process: there is no communication protocol. Given a
  * compiled [[CFG]], this resolves `fname`, runs ESMeta's own
  * [[esmeta.interpreter.Interpreter]] on a fresh [[State]] whose context is the
  * resolved function, and reads the result back from `%RESULT`
  * ([[esmeta.state.GLOBAL_RESULT]]).
  *
  * Values are converted at the boundary by [[toEs]] / [[fromEs]]. Only scalar
  * values cross for now; heap-backed records/lists and Wasm-owned values are an
  * intended future extension and raise an [[InterpreterError]] until then.
  */
object IrCaller:

  /** Run ESMeta IR function `fname` with `args`, or [[None]] if `fname` is not
    * an IR function in `cfg`.
    */
  def call(cfg: CFG, fname: String, args: List[WjValue]): Option[WjValue] =
    cfg.fnameMap.get(fname).map { func =>
      val vs = args.map(toEs)
      val params = func.params
      val required = params.count(!_.optional)
      if vs.length < required || vs.length > params.length then
        throw InterpreterError(
          s"$fname: expected ${params.length} args, got ${vs.length}",
        )
      val locals = MMap.empty[Local, Value]
      params.zip(vs).foreach((p, v) => locals += p.lhs -> v)
      // seed the standard initial globals/heap (type-name globals like
      // `@Undefined`, intrinsics, ...) so abstract operations can run, mirroring
      // `Initialize.from` but with our own context and no source text.
      val st = State(
        cfg,
        context = Context(func, locals),
        globals = MMap.from(cfg.init.initGlobal),
        heap = cfg.init.initHeap.copied,
      )
      EsInterpreter(st)
      st.globals.get(GLOBAL_RESULT).map(fromEs).getOrElse(WjValue.Undef)
    }

  /** WJI value -> ESMeta IR value (scalars only for now). */
  private def toEs(v: WjValue): Value = v match
    case WjValue.Undef   => Undef
    case WjValue.Null    => Null
    case WjValue.Bool(b) => Bool(b)
    case WjValue.Num(n)  => Number(n)
    case WjValue.Math(n) => Math(n)
    case WjValue.Str(s)  => Str(s)
    case WjValue.Enum(n) => Enum(n)
    case other =>
      throw InterpreterError(
        s"cannot pass $other into an ESMeta IR function (scalars only)",
      )

  /** ESMeta IR value -> WJI value (scalars only for now). */
  private def fromEs(v: Value): WjValue = v match
    case Undef     => WjValue.Undef
    case Null      => WjValue.Null
    case Bool(b)   => WjValue.Bool(b)
    case Number(n) => WjValue.Num(n)
    case Math(n)   => WjValue.Math(n)
    case BigInt(n) => WjValue.Math(BigDecimal(n))
    case Str(s)    => WjValue.Str(s)
    case Enum(n)   => WjValue.Enum(n)
    case other =>
      throw InterpreterError(
        s"cannot return $other from an ESMeta IR function (scalars only)",
      )
