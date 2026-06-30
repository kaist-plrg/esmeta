package esmeta.wji.ir

/** A program-level global variable declaration.
  *
  * @param name
  *   the name bound as a [[Global]] ref
  * @param init
  *   a body that produces the initial value; it must end in [[IReturn]] (like a
  *   zero-arg function body), so initializers may call embedding operations /
  *   other functions, not just evaluate a pure expression. Evaluated once,
  *   before any function runs.
  */
case class GlobalDecl(name: String, init: Inst)

/** A collection of [[Func]]s that form a complete IR program.
  *
  * `funcMap` is the fast lookup index used by the interpreter to resolve
  * [[EClo]] callees. `globals` declares module-level [[Global]] variables,
  * initialized in order before any function runs.
  */
case class Program(funcs: List[Func], globals: List[GlobalDecl] = Nil):
  lazy val funcMap: Map[String, Func] = funcs.map(f => f.name -> f).toMap
