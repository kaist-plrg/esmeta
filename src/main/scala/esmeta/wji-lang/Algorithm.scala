package esmeta.wji.lang

/** An algorithm extracted from a `<div algorithm>` block.
  *
  * @param id     the value of the `algorithm="..."` attribute, if the
  *               `<div>` tag has one (e.g. `"read-the-imports"`)
  * @param name   the algorithm/operation name, taken from the first
  *               `<dfn>` element found in `head`, if any (e.g.
  *               `"compile a WebAssembly module"`, or `"validate"` for
  *               `<dfn>validate(|bytes|, |options|)</dfn>` - any trailing
  *               `(|arg1|, |arg2|)`/`()` parameter list is stripped, since
  *               it is captured in `params` instead)
  * @param params every distinct `|variable|` reference in `head`, in order
  *               of first appearance
  * @param head   the descriptive text preceding the step list, e.g. "To
  *               <dfn>compile a WebAssembly module</dfn> from source bytes
  *               |bytes|, perform the following steps:"
  * @param body   the algorithm's steps, parsed into [[Instr]]s, in source
  *               order
  */
case class Algorithm(
  id: Option[String],
  name: Option[String],
  params: List[String],
  head: String,
  body: List[Instr],
)
