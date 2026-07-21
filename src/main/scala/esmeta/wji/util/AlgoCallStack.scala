package esmeta.wji.util

import esmeta.state.State

/** Helpers for inspecting the call stack of compiled WJI algorithms during
  * interpretation — used by the `wji-interp`/`wji-eval` debugging phases, but
  * not tied to error reporting specifically.
  */
object AlgoCallStack:

  /** Prints the call stack (outermost first) plus the innermost frame's own
    * cursor — the exact node execution stopped at, not just which function.
    * Only tells the whole story for a failure in the *outermost* running
    * context, though: a reentrant call via `Interpreter.invokeCallable` (e.g.
    * servicing a Wasm `host_func_invoke`) restores the outer `st.context`
    * before its own exception propagates here, so a failure *inside* one of
    * those instead surfaces via that method's own stderr log, not this one.
    */
  def printCallStack(st: State): Unit =
    val stack =
      (st.context.name :: st.callStack.map(_.context.name)).reverse
    println("Call stack (outermost first):")
    stack.zipWithIndex.foreach((f, i) => println(s"  ${"  " * i}$f"))
    println(s"Cursor: ${st.context.cursor}")
