package esmeta.wji.state

import esmeta.wji.state.ALValue

/** Runtime values manipulated by the WJI [[Interpreter]].
  *
  * Most values are "wjmeta-owned" (allocated and inspected on this side), but
  * [[Wasm]] wraps an [[ALValue]] that is owned by SpecTec — a store, module,
  * address, `val`, etc. that we only pass back and forth across the
  * [[esmeta.wji.bridge.host.WasmHost]] boundary without interpreting its contents.
  *
  * [[Record]] and [[List]] are handles into the [[Heap]]; the actual fields and
  * elements live there so they can be mutated in place.
  */
enum WjValue:
  /** A value owned by SpecTec, passed opaquely across the WasmHost boundary */
  case Wasm(v: ALValue)
  case Undef
  case Null
  case Bool(b: Boolean)
  case Num(n: Double)
  case Math(n: BigDecimal)
  case Str(s: String)

  /** Enumeration value, e.g. `~normal~`, `~throw~`, `~return~` */
  case Enum(name: String)

  /** Handle to a heap-allocated record */
  case Record(id: Int)

  /** Handle to a heap-allocated list */
  case List(id: Int)

  /** Closure / first-order function reference */
  case Clo(fname: String)
