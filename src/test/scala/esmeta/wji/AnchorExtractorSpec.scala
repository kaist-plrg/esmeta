package esmeta.wji

import org.scalatest.funsuite.AnyFunSuite
import esmeta.wji.lang.{AnchorExtractor, SpecFile}
import esmeta.wji.bridge.host.WasmHost

class AnchorExtractorSpec extends AnyFunSuite:

  private lazy val anchors =
    AnchorExtractor.extractFromFile(SpecFile.jsApiIndex)

  /** Anchors the spec's own `<pre class="anchors">` block declares but that
    * `WasmHost` doesn't need to mirror: `mem_read`/`mem_write` are registered
    * there, yet never actually referenced anywhere in js-api/index.bs's own
    * prose (no `[=mem_read=]`/`[=mem_write=]` call site), and aren't defined in
    * the Wasm Core Spec's own embedding interface (`appendix/ embedding.rst`)
    * or its SpecTec backend (`embedding.ml`) either — dead anchor
    * registrations, not a real gap in `WasmHost`.
    */
  private val unusedAnchors: Set[String] = Set("mem_read", "mem_write")

  /** `WasmHost` names with no matching js-api/index.bs anchor at all, because
    * they aren't part of the Wasm Core Spec's own Embedding API
    * (`embedding.rst`) to begin with: `expand` is a `wjmeta-bridge`-specific
    * convenience wrapping the `$Expand` relation, added to work around
    * js-api/index.bs destructuring a `deftype` without ever calling `$expand`
    * itself — see `WasmHost.paramNames`'s `expand` entry and
    * `docs/spec_errors.md`. Not a spec gap `WasmHost` needs to close; the spec
    * was never going to anchor it.
    */
  private val bridgeOnlyNames: Set[String] = Set("expand")

  test(
    "WasmHost.names matches every embedding function js-api/index.bs's anchors block declares",
  ) {
    val specNames =
      AnchorExtractor.embeddingFunctionNames(anchors) -- unusedAnchors
    val missing = specNames -- WasmHost.names
    val extra = WasmHost.names -- specNames -- bridgeOnlyNames
    assert(missing.isEmpty, s"WasmHost is missing: ${missing.toList.sorted}")
    assert(
      extra.isEmpty,
      s"WasmHost declares names the spec's anchors block doesn't: ${extra.toList.sorted}",
    )
  }
