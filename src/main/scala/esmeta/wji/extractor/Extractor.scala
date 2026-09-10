package esmeta.wji.extractor

import esmeta.util.SystemUtils.*
import esmeta.wji.lang.{
  Algorithm,
  AlgorithmKind,
  Definition,
  DefinitionKind,
  MemberKind,
  Operation,
  Param,
}
import esmeta.wji.spec.{Spec, SpecFile, SpecPatch}

/** Unified entry point producing a [[Spec]] from the WebAssembly JS-API spec
  * sources — mirrors mainline's `esmeta.extractor.Extractor` (which takes a
  * parsed HTML `Document`); this one works directly on raw Bikeshed text
  * instead, like every other WJI extractor, since WJI has no separate
  * HTML-parsing step.
  */
object Extractor:
  def apply(): Spec =
    // patched once and reused for algorithms/definitions/anchors alike,
    // rather than re-reading and re-patching the same file 3 times over
    val jsApiSource = SpecPatch(readFile(SpecFile.jsApiIndex.toString))
    val jsApiAlgorithms = AlgorithmExtractor.extract(jsApiSource)
    val webidlAlgorithms = AlgorithmExtractor
      .extractFromFile(SpecFile.webidlIndex)
      .filter(a => a.name.exists(SpecFile.webidlFilter.contains))
    val definitions = DefinitionExtractor.extract(jsApiSource)
    val anchors = AnchorExtractor.extract(jsApiSource)
    // `AlgorithmExtractor` produces `AlgorithmKind.Method(for)` for any "The
    // <dfn method for="X">..." dfn uniformly, without knowing whether `X` is a
    // real WebIDL interface or a *namespace* (`WebAssembly` itself) — that
    // distinction only exists once `definitions` (computed above) is in
    // hand, via each one's `DefinitionKind`. It matters structurally, not
    // just for naming: WebIDL's "create a namespace object" installs a
    // namespace's own operations directly on the namespace object itself,
    // while "create an interface object"/"create an interface prototype
    // object" installs an interface's members on a *separate* interface
    // prototype object — two different algorithms building and populating
    // two different objects, not one mechanism with two names. A namespace
    // method/getter is restamped `AlgorithmKind.NamespaceMethod`/
    // `NamespaceGetter` here (each kind's own doc has the full rationale);
    // a `Method` that's neither a known interface nor a known namespace
    // (shouldn't occur in this corpus, but no dfn text guarantees it can't)
    // falls back to `Plain`, same as before this distinction existed.
    val interfaceNames = definitions
      .filter(_.kind == DefinitionKind.Interface)
      .map(_.name)
      .toSet
    val namespaceNames = definitions
      .filter(_.kind == DefinitionKind.Namespace)
      .map(_.name)
      .toSet
    val algorithms = (jsApiAlgorithms ++ webidlAlgorithms)
      .map { a =>
        a.kind match
          case AlgorithmKind.Method(forName, _) if namespaceNames(forName) =>
            a.copy(kind = AlgorithmKind.NamespaceMethod(forName))
          case AlgorithmKind.Method(forName, _) if !interfaceNames(forName) =>
            a.copy(kind = AlgorithmKind.Plain)
          case AlgorithmKind.Getter(forName) if namespaceNames(forName) =>
            a.copy(kind = AlgorithmKind.NamespaceGetter(forName))
          case _ => a
      }
      .map(enrichParamTypes(_, definitions))
    Spec(algorithms, definitions, anchors)

  /** Finds `algo`'s matching WebIDL operation (its interface's or namespace's
    * `Definition`, looked up by `AlgorithmKind.Method`/`NamespaceMethod`/
    * `Constructor`'s own `interface`/`namespace` name, then the `Operation`
    * member matching `algo.name`/its `Constructor` kind), and:
    *   - for a `Method`, stamps `AlgorithmKind.Method.static` from that
    *     operation's own `MemberKind` (`StaticOperation` vs `RegularOperation`)
    *     — the only place this can be determined at all, since the dfn prose
    *     `enrichParamTypes` itself is named for never spells out `static` (see
    *     [[esmeta.wji.lang.AlgorithmKind.Method]]'s own doc).
    *   - if found and its positional param count agrees, stamps each
    *     `WjiParam.idlType`/`WjiParam.optional`/`WjiParam.default` with that
    *     operation's declared WebIDL type text/`optional` keyword/default-value
    *     text — see [[esmeta.wji.lang.WjiParam]]'s own doc for what consumes
    *     these. `optional`/`default` here are WebIDL's own declaration (e.g.
    *     `optional any value`, `optional WebAssemblyCompileOptions options =
    *     {}`), a different source from `AlgorithmExtractor.extractParams`'s
    *     "using optional X |Y|" prose detection (which a `Method`/`Constructor`
    *     dfn's own head, e.g. `grow(|delta|, |value|)`, never spells out — only
    *     the separate `<pre class=idl>` block does) — `p.optional` is OR'd with
    *     the WebIDL flag rather than overwritten, so either source marking a
    *     param optional is enough.
    *   - if found, stamps `Algorithm.idlReturnType` with that operation's own
    *     declared return type text (e.g. `"undefined"`) — see that field's own
    *     doc for what consumes it.
    *
    * A `Getter`/`Setter`/`Plain` algorithm is left untouched: getters take no
    * arguments, and a setter's implicit "the given value" isn't a positional
    * `WjiParam` at all (see
    * `esmeta.wji.compiler.lowering.AddInterfaceMemberBuiltinBehaviourPass.givenValueBinding`),
    * so neither has anything here to stamp. Both also always have a real
    * (non-`"undefined"`) declared type of their own regardless — a getter
    * returns its attribute's type, and WebIDL gives setters no declared return
    * type to speak of at all — so `idlReturnType` staying `None` for them costs
    * nothing.
    */
  private def enrichParamTypes(
    algo: Algorithm,
    definitions: List[Definition],
  ): Algorithm =
    def matchingOperation(
      owner: String,
      ownerKind: DefinitionKind,
      matches: Operation => Boolean,
    ): Option[Operation] =
      definitions
        .find(d => d.kind == ownerKind && d.name == owner)
        .flatMap(_.members.collectFirst {
          case op: Operation if matches(op) => op
        })
    val webidlOp: Option[Operation] = algo.kind match
      case AlgorithmKind.Method(iface, _) =>
        matchingOperation(
          iface,
          DefinitionKind.Interface,
          op =>
            op.kind != MemberKind.Constructor && op.id == algo.name.getOrElse(
              "",
            ),
        )
      case AlgorithmKind.NamespaceMethod(namespace) =>
        matchingOperation(
          namespace,
          DefinitionKind.Namespace,
          _.id == algo.name.getOrElse(""),
        )
      case AlgorithmKind.Constructor(iface) =>
        matchingOperation(
          iface,
          DefinitionKind.Interface,
          _.kind == MemberKind.Constructor,
        )
      case _ => None
    val staticStamped = (algo.kind, webidlOp) match
      case (AlgorithmKind.Method(iface, _), Some(op)) =>
        algo.copy(kind =
          AlgorithmKind
            .Method(iface, static = op.kind == MemberKind.StaticOperation),
        )
      case _ => algo
    // a `Constructor`'s own `Operation` (see `DefinitionExtractor.parseMember`)
    // always has `ret = ""` (WebIDL constructors declare no return type of
    // their own), so `nonEmpty` here also doubles as "only a real Method/
    // NamespaceMethod return type counts" without needing to match on `kind`
    // again.
    val returnTypeStamped = webidlOp match
      case Some(op) if op.ret.nonEmpty =>
        staticStamped.copy(idlReturnType = Some(op.ret))
      case _ => staticStamped
    webidlOp.map(_.params) match
      case Some(ps) if ps.length == returnTypeStamped.params.length =>
        returnTypeStamped.copy(params = returnTypeStamped.params.zip(ps).map {
          case (p, wp) =>
            p.copy(
              idlType = Some(wp.ty),
              optional = p.optional || wp.optional,
              default = Option.when(wp.default.nonEmpty)(wp.default),
            )
        })
      case _ => returnTypeStamped
