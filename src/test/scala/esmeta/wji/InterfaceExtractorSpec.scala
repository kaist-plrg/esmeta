package esmeta.wji

import org.scalatest.funsuite.AnyFunSuite
import esmeta.wji.lang.*

class InterfaceExtractorSpec extends AnyFunSuite:

  private lazy val interfaces =
    InterfaceExtractor.extractFromFile(SpecFile.jsApiIndex)

  test("extracts every WebIDL interface in index.bs") {
    assert(
      interfaces.map(_.name).toSet ==
      Set("Module", "Instance", "Memory", "Table", "Global", "Tag", "Exception"),
    )
  }

  test("doesn't truncate a body containing a nested {} default value") {
    // Module's members each open with `optional WebAssemblyCompileOptions
    // options = {}` (constructor) or reference `{}`-nested defaults — a naive
    // regex spanning the whole body up to the first `}` would cut this off
    // after the empty `{}`, well before Module's own closing brace.
    val module = interfaces.find(_.name == "Module").get
    assert(module.members.size == 4)
    assert(module.members.exists(_.startsWith("constructor(")))
    assert(module.members.count(_.startsWith("static ")) == 3)
  }

  test(
    "doesn't pick up dictionary/enum blocks sharing the same <pre class=idl>",
  ) {
    val names = interfaces.map(_.name).toSet
    assert(!names.contains("ModuleExportDescriptor"))
    assert(!names.contains("TableDescriptor"))
    assert(!names.contains("ImportExportKind"))
  }

  test("Instance has the exports attribute and a constructor") {
    val instance = interfaces.find(_.name == "Instance").get
    assert(
      instance.members.exists(_.contains("readonly attribute object exports")),
    )
    assert(instance.members.exists(_.startsWith("constructor(")))
  }
