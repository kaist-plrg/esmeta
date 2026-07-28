package esmeta.wji

import org.scalatest.funsuite.AnyFunSuite
import esmeta.wji.extractor.AnchorExtractor
import esmeta.wji.spec.SpecFile
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
    * `docs/spec_errors.md`. `signed_31`/`signed_32`/`signed_64` are Wasm Core
    * spec numerics (`signed_(N)`) that js-api prose calls directly by their
    * rendered per-width name; the SpecTec server translates that name back to
    * the real parametric `signed` op itself (`server.ml`'s `call_signed`), so
    * `WasmHost` just forwards the call like any other embedding function. None
    * of these are a spec gap `WasmHost` needs to close; the spec was never
    * going to anchor them. `inv_signed_31`/`inv_signed_32`/`inv_signed_64` are
    * the same story one level further removed: js-api prose never even names
    * `signed_N`'s inverse directly (only describes it via "the unsigned integer
    * such that ... is [=signed_N=](...)"), so there's no dfn text to anchor at
    * all — `ExpandSuchThatPass` recognizes that idiom and emits these as a
    * WJI-invented per-width name, translated the same way (`server.ml`'s
    * `call_inv_signed`). `mem_read_bytes`/`mem_write_bytes` are a third case: a
    * wjmeta-bridge-specific bulk-transfer extension for the
    * `WebAssembly.Memory.prototype.buffer` JS/wasm byte-content sync bridge
    * (`docs/hardcodes.md`) — `embedding.rst`'s own `mem_read`/`mem_write` are
    * byte-at-a-time and (per `unusedAnchors` above) unused by js-api itself, so
    * there was never a dfn to anchor a bulk variant against either.
    */
  private val bridgeOnlyNames: Set[String] =
    Set(
      "expand",
      "signed_31",
      "signed_32",
      "signed_64",
      "inv_signed_31",
      "inv_signed_32",
      "inv_signed_64",
      "mem_read_bytes",
      "mem_write_bytes",
    )

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
