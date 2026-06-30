package esmeta.wji.ir

/** A parameter of an [[Func]].
  *
  * @param name
  *   the local [[Name]] bound when the function is called
  * @param optional
  *   true if the parameter may be omitted at the call site
  */
case class Param(name: Name, optional: Boolean = false)

/** A single IR function / abstract operation.
  *
  * @param name
  *   unique identifier used to look up the function (e.g. from an [[EClo]]
  *   callee in [[ICall]])
  * @param params
  *   formal parameter list, in call-site order
  * @param body
  *   the function body as a single [[Inst]] (typically [[ISeq]])
  */
case class Func(
  name: String,
  params: List[Param],
  body: Inst,
)
