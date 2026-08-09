package esmeta.wji

import org.scalatest.funsuite.AnyFunSuite
import esmeta.wji.lang.*
import esmeta.wji.extractor.DefinitionExtractor
import esmeta.wji.spec.SpecFile

class DefinitionExtractorSpec extends AnyFunSuite:

  private lazy val definitions =
    DefinitionExtractor.extractFromFile(SpecFile.jsApiIndex)
  private lazy val interfaces =
    definitions.filter(_.kind == DefinitionKind.Interface)
  private lazy val namespaces =
    definitions.filter(_.kind == DefinitionKind.Namespace)

  test("extracts every WebIDL interface in index.bs") {
    assert(
      interfaces.map(_.name).toSet ==
      Set("Module", "Instance", "Memory", "Table", "Global", "Tag", "Exception"),
    )
  }

  test("extracts the WebAssembly namespace") {
    assert(namespaces.map(_.name) == List("WebAssembly"))
    val wasm = namespaces.head
    assert(wasm.extAttr == List(ExtendedAttribute("Exposed", Some("*"))))
    val operations = wasm.members.collect { case o: Operation => o }
    assert(
      operations.map(_.id) ==
      List("validate", "compile", "instantiate", "instantiate_object"),
    )
    assert(
      wasm.members.contains(Attribute("JSTag", "Tag", readonly = true)),
    )
  }

  test("doesn't truncate a body containing a nested {} default value") {
    // Module's members each open with `optional WebAssemblyCompileOptions
    // options = {}` (constructor) or reference `{}`-nested defaults — a naive
    // regex spanning the whole body up to the first `}` would cut this off
    // after the empty `{}`, well before Module's own closing brace.
    val module = interfaces.find(_.name == "Module").get
    assert(module.members.size == 4)
    val ops = module.members.collect { case o: Operation => o }
    assert(ops.exists(_.kind == MemberKind.Constructor))
    assert(ops.count(_.kind == MemberKind.StaticOperation) == 3)
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
      instance.members.contains(
        Attribute("exports", "object", readonly = true),
      ),
    )
    assert(
      instance.members.exists {
        case Operation("constructor", _, _, MemberKind.Constructor, _) => true
        case _                                                         => false
      },
    )
  }

  test("parses a definition's extended attribute list") {
    val module = interfaces.find(_.name == "Module").get
    assert(
      module.extAttr == List(
        ExtendedAttribute("LegacyNamespace", Some("WebAssembly")),
        ExtendedAttribute("Exposed", Some("*")),
      ),
    )
  }

  test("parses an extended attribute value containing parens/commas") {
    // A synthetic fixture rather than a real corpus interface: every
    // `Exposed=(...)`-shaped interface in js-api/index.bs (`Tag`/`Exception`)
    // is normalized to `Exposed=*` by `SpecPatch` #37 (`Tag`/`Exception`
    // should be exposed everywhere `WebAssembly` itself is, like every other
    // interface in this namespace — see `docs/spec_inconsistencies.md` #12),
    // so no real interface with a parens/commas-list value survives
    // extraction to exercise this parsing rule against.
    val defs = DefinitionExtractor.extract(
      """<pre class="idl">
        |[Exposed=(Window,Worker,Worklet)]
        |interface Foo {
        |};
        |</pre>""".stripMargin,
    )
    val foo = defs.find(_.name == "Foo").get
    assert(
      foo.extAttr == List(
        ExtendedAttribute("Exposed", Some("(Window,Worker,Worklet)")),
      ),
    )
  }

  test("parses a parameter's optional flag, default value, and ext attrs") {
    val module = interfaces.find(_.name == "Module").get
    val ctor = module.members.collectFirst {
      case o @ Operation("constructor", _, _, MemberKind.Constructor, _) => o
    }.get
    assert(
      ctor.params == List(
        Param(
          "AllowSharedBufferSource",
          optional = false,
          default = "",
          extAttribute = List(ExtendedAttribute("AllowResizable", None)),
        ),
        Param(
          "WebAssemblyCompileOptions",
          optional = true,
          default = "{}",
          extAttribute = Nil,
        ),
      ),
    )
  }

  test("keeps a multi-word type intact for a parameter") {
    val exception = interfaces.find(_.name == "Exception").get
    val getArg = exception.members.collectFirst {
      case o @ Operation("getArg", _, _, _, _) => o
    }.get
    assert(getArg.params.last.ty == "unsigned long")
  }

  test("keeps a parenthesized union type intact for an attribute") {
    val exception = interfaces.find(_.name == "Exception").get
    assert(
      exception.members.contains(
        Attribute("stack", "(DOMString or undefined)", readonly = true),
      ),
    )
  }
