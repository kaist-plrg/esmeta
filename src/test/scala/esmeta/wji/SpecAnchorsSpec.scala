package esmeta.wji

import org.scalatest.funsuite.AnyFunSuite
import esmeta.wji.lang.{SpecAnchors, SpecFile}
import esmeta.wji.bridge.host.WasmHost
import java.nio.file.Files

class SpecAnchorsSpec extends AnyFunSuite:

  private lazy val source = Files.readString(SpecFile.jsApiIndex)

  /** Anchors the spec's own `<pre class="anchors">` block declares but that
    * `WasmHost` doesn't need to mirror: `mem_read`/`mem_write` are registered
    * there, yet never actually referenced anywhere in js-api/index.bs's own
    * prose (no `[=mem_read=]`/`[=mem_write=]` call site), and aren't defined in
    * the Wasm Core Spec's own embedding interface (`appendix/ embedding.rst`)
    * or its SpecTec backend (`embedding.ml`) either — dead anchor
    * registrations, not a real gap in `WasmHost`.
    */
  private val unusedSpecAnchors: Set[String] = Set("mem_read", "mem_write")

  test(
    "WasmHost.names matches every embedding function js-api/index.bs's anchors block declares",
  ) {
    val specNames =
      SpecAnchors.embeddingFunctionNames(source) -- unusedSpecAnchors
    val missing = specNames -- WasmHost.names
    val extra = WasmHost.names -- specNames
    assert(missing.isEmpty, s"WasmHost is missing: ${missing.toList.sorted}")
    assert(
      extra.isEmpty,
      s"WasmHost declares names the spec's anchors block doesn't: ${extra.toList.sorted}",
    )
  }
