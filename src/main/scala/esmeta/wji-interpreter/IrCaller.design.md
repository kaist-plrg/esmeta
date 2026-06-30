# Design note: calling between WJI IR and ESMeta IR

This documents how WJI IR calls ESMeta IR today, and how the design extends to the
reverse direction (ESMeta IR calling WJI), so that mutually recursive calls
(WJI → IR → WJI → …) work.

## Today: WJI → ESMeta IR (one way)

The WJI interpreter (`Interpreter.scala`) resolves an `ICall` callee first against the
WJI `Program.funcMap`. When the name is not a WJI function, it falls back to ESMeta IR via
`IrCaller`. ESMeta IR functions run **in-process** — there is no communication protocol
(unlike the Wasm embedding `WasmHost`, which talks to SpecTec over JSON-RPC).

`IrCaller.call(fname, args)`:
1. resolves `fname` in `cfg.fnameMap` (`None` ⇒ not an IR function),
2. converts each WJI arg `WjValue` → ESMeta `state.Value` (scalars only for now),
3. binds the params and runs ESMeta's own `Interpreter` on a `State`,
4. reads the result back from `%RESULT` (`GLOBAL_RESULT`) and converts it back to a
   `WjValue`.

Only scalars cross the boundary for now (`Undef`, `Null`, `Bool`, `Num`, `Math`, `Str`,
`Enum`). Heap-backed records/lists and Wasm-owned values are an intended future extension
and raise an `InterpreterError` until then.

## The reverse direction: ESMeta IR → WJI (future)

This must be a **synchronous, reentrant nested call**, mirroring the reentrancy the
WasmHost demo already exercises (a host function calls an embedding mid-callback). The JVM
call stack naturally interleaves the two interpreters:

```
WJI.callFunc(f)                  -- WJI frame on the JVM stack
  └ IrCaller.call(g)             -- runs EsInterpreter(st).result
      └ ESMeta hits a call to WJI h     -- foreign-call hook
          └ WJI.callFunc(h)      -- nested WJI frame
              └ IrCaller.call(k) -- nested ESMeta run
```

Three pieces are required:

### 1. A back-reference (the helper stops being one-way)
For IR → WJI, the IR side needs to call back into the WJI interpreter. So `IrCaller` is not
a free-standing `object`; the WJI `Interpreter` and `IrCaller` hold each other — resolved
by passing `this` (or a `(fname, args) => WjValue` callback) into `IrCaller` at
construction. This is symmetric to how `toHostFunc` already closes over the WJI interpreter
to let SpecTec call back into WJI.

### 2. A foreign-call hook in ESMeta's interpreter
ESMeta's `eval(call: Call)` resolves a `Clo(func, …)` where `func` is a `cfg.Func`. A WJI
function is not in the CFG, so the plan is to **subclass ESMeta's `Interpreter`** and
override the call path: when the callee denotes a WJI function, do **not** push an ESMeta
`CallContext`; instead synchronously compute the WJI result, convert it, and
`setCallResult(lhs, …)` — i.e. treat the foreign call like a primitive that returns a
value. Open sub-question for that pass: *how a WJI function surfaces as an ESMeta callable*
(a reserved name space, or a dedicated `Callable`). The override point itself is clear.

### 3. State management — the crux (and why `IrCaller` is stateful now)
A single persistent ESMeta `State` is the right model: its `heap` + `globals` are the
actual mutable state and **must be shared** so effects are visible across the boundary (and
so WJI values can later reference ESMeta heap objects).

But the same `State` also carries the *execution position* — `st.context` and
`st.callStack`. Under reentrancy, a nested `IrCaller.call` would otherwise **clobber the
suspended outer ESMeta frame**. So `IrCaller.call` saves/restores the execution cursor and
stack around each run, while leaving heap/globals shared:

```scala
def call(fname, args) = cfg.fnameMap.get(fname).map { func =>
  val savedCtx   = st.context      // protect the suspended outer ESMeta frame
  val savedStack = st.callStack
  try
    st.context   = Context(func, boundLocals)
    st.callStack = Nil             // so this run's ExitCursor writes %RESULT
    EsInterpreter(st).result       // shares st.heap / st.globals
    st.globals.get(GLOBAL_RESULT).map(fromEs).getOrElse(WjValue.Undef)
  finally
    st.context   = savedCtx
    st.callStack = savedStack
}
```

The WJI side is already reentrancy-safe: `callFunc` pushes/pops its own `callStack` and is
just normal JVM recursion.

## Decision for the current change

Kept deliberately minimal so the example just runs: `IrCaller` is a stateless `object`,
and each `call` builds a **fresh** `State(cfg, Context(func, locals))`, runs ESMeta's
interpreter to completion, and reads back `%RESULT`. The WJI interpreter holds
`cfg: Option[CFG]` and delegates unresolved callees to `IrCaller.call`.

This is enough for pure scalar abstract operations (e.g. `ToBoolean`). The persistent
shared `State` + save/restore discipline described above (and the IR → WJI foreign-call
hook) are deferred to when mutual recursion and ESMeta-heap-backed WJI values are actually
needed.
