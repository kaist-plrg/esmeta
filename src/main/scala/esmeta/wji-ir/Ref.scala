package esmeta.wji.ir

sealed trait Ref

/** `base.[[name]]` or `base[expr]` */
case class Field(base: Ref, key: Expr) extends Ref

sealed trait Var extends Ref

/** Global variable, e.g. `%Store` */
case class Global(name: String) extends Var

/** Named local variable, e.g. `|bytes|` */
case class Name(name: String) extends Var

/** Compiler-generated temporary */
case class Temp(idx: Int) extends Var
