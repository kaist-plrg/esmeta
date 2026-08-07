package esmeta.state.util

import esmeta.error.{NoList, NoWasmValue, WasmHostFailure}
import esmeta.state.*

/** Converts a [[Value]] crossing the WasmHost boundary (an `ICallEmbed`
  * argument or a [[esmeta.wji.bridge.host.HostFunction]] result) to an
  * [[ALValue]]. A heap-allocated ES list (e.g. from an Infra-spec list literal
  * like `« |payload| »`, compiled to `EList`/`Addr`) is recursively converted
  * into an [[ALValue.ListV]] — this is the one case that isn't already a
  * [[Wasm]] value, needed by embedding functions whose `val*`/`externval*`
  * argument wasn't itself built by a prior embedding call.
  */
def toAL(st: State, v: Value): ALValue = v match
  case Wasm(av)                   => av
  case Str(s)                     => ALValue.TextV(s)
  case Bool(b)                    => ALValue.BoolV(b)
  case Number(n)                  => ALValue.NumV(ALNum.Real(n))
  case Math(n) if n.toBigInt >= 0 => ALValue.NumV(ALNum.Nat(n.toBigInt))
  case Math(n)                    => ALValue.NumV(ALNum.Int(n.toBigInt))
  case addr: Addr =>
    st(addr) match
      case ListObj(vs) => ALValue.ListV(vs.map(toAL(st, _)).toList)
      case other       => throw NoList(other)
  case other => throw NoWasmValue(other)

/** [[toAL]]'s inverse for a single `` `Nat ``-/`` `Int ``-tagged AL number, the
  * shape a single wasm byte always arrives as crossing the WasmHost boundary.
  */
def fromALNum(av: ALValue): Value = av match
  case ALValue.NumV(ALNum.Nat(n)) => Math(n)
  case ALValue.NumV(ALNum.Int(n)) => Math(n)
  case other =>
    throw WasmHostFailure(s"fromALNum: expected a byte, got $other")
