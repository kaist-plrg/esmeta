package esmeta.wji.lang

/** One `text: ...` entry out of js-api/index.bs's own `<pre class="anchors">`
  * block, resolved against every `urlPrefix:`/`spec:`/`type:`/`for:`/`url:` key
  * active in its enclosing indentation scope (see [[AnchorExtractor.extract]]).
  *
  * @param url
  *   the fully-resolved URL (`urlPrefix` concatenated with the entry's own or
  *   an enclosing `url:`), not just the relative path the source writes
  */
case class Anchor(
  text: String,
  url: String,
  spec: Option[String],
  typ: Option[String],
  forScope: Option[String],
)
