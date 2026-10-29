package esmeta.interpreter

import esmeta.state.*
import esmeta.state.util.*

// Garbage Collector
object GC {

  /** `extraFrames`: outer `(context, callStack)` pairs
    * [[Interpreter. invokeCallable]] has temporarily suspended (see
    * `Interpreter.suspendedFrames`'s own doc) — `st` alone doesn't reach them
    * while `st.context`/`st.callStack` hold a reentrant call's own callee
    * instead, so they're walked as extra roots here too.
    *
    * `extraValues`: values held only outside the heap (e.g. the callees of Wasm
    * host functions, see `Interpreter.hostFuncCallees`), walked as extra roots
    * as well.
    */
  def apply(
    st: State,
    extraFrames: List[(Context, List[CallContext])] = Nil,
    extraValues: List[Value] = Nil,
  ): Unit = {
    var addrSet: Set[Addr] = Set()
    val walker = new UnitWalker {
      override def walk(addr: Addr): Unit = addrSet += addr
      override def walk(heap: Heap): Unit = {}
    }
    walker.walk(st)
    for (context, callStack) <- extraFrames do
      walker.walk(context)
      callStack.foreach(walker.walk)
    extraValues.foreach(walker.walk)

    val heap = st.heap
    val map = heap.map
    def aux(diff: Set[Addr]): Unit = {
      val prev = addrSet
      diff.foreach(addr =>
        map.get(addr) match {
          case Some(obj) => walker.walk(obj)
          case None      =>
        },
      )
      val newDiff = addrSet -- prev
      if (!newDiff.isEmpty) aux(newDiff)
    }
    aux(addrSet)

    for {
      (addr, obj) <- map
      addr <- addr match {
        case _: NamedAddr => None
        case dyn          => Some(dyn)
      }
      if !(addrSet contains addr)
    } map -= addr
  }
}
