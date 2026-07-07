package esmeta.wji

import org.scalatest.{Args, Status}
import org.scalatest.funsuite.AnyFunSuite
import java.nio.file.{Files, Paths}
import esmeta.BASE_DIR
import esmeta.wji.lang.{AlgorithmExtractor, InstrPrinter, SpecFile}
import esmeta.wji.compiler.lowering.Lowering
import esmeta.wji.compiler.Compiler

/** Golden-file snapshot tests for the full pipeline.
  *
  * On mismatch, the diff is written to `<name>.diff` next to the `.expected`
  * file so it can be inspected. When a change is intentional, regenerate the
  * expected files:
  * {{{
  *   sbt "testOnly esmeta.wji.SnapshotSpec -- -Dupdate=true"
  * }}}
  */
class SnapshotSpec extends AnyFunSuite:

  private val goldenDir =
    Paths.get(BASE_DIR).resolve("src/test/resources/golden/wji")
  private var update = false

  override def run(testName: Option[String], args: Args): Status =
    update = args.configMap.getWithDefault("update", "false") == "true"
    super.run(testName, args)

  private lazy val algorithms = SpecFile.loadAllAlgorithms()

  test("metalang (metalang.expected)") {
    val sb = new StringBuilder
    sb.append(s"${algorithms.size} algorithm(s)\n")
    algorithms.foreach(a => sb.append(InstrPrinter.render(a)).append("\n"))
    checkSnapshot("metalang.expected", sb.toString)
  }

  test("IR (ir.expected)") {
    val program = Compiler.compile(Lowering.run(algorithms))
    val sb = new StringBuilder
    sb.append(s"${program.funcs.size} func(s)\n")
    sb.append(program.toString()).append("\n")
    checkSnapshot("ir.expected", sb.toString)
  }

  private def checkSnapshot(name: String, actual: String): Unit =
    val expectedFile = goldenDir.resolve(name)
    val diffFile = goldenDir.resolve(name.replaceAll("\\.expected$", ".diff"))

    val actualFile =
      goldenDir.resolve(name.replaceAll("\\.expected$", ".actual"))

    if update then
      Files.createDirectories(expectedFile.getParent)
      Files.writeString(expectedFile, actual)
      List(diffFile, actualFile).filter(Files.exists(_)).foreach(Files.delete)
      info(s"Updated: $expectedFile")
    else
      assert(
        Files.exists(expectedFile),
        s"Expected file missing: $expectedFile  —  run with UPDATE_SNAPSHOTS=true to create.",
      )
      val expected = Files.readString(expectedFile)
      if actual == expected then
        List(diffFile, actualFile).filter(Files.exists(_)).foreach(Files.delete)
      else
        Files.writeString(actualFile, actual)
        Files.writeString(diffFile, unifiedDiff(expectedFile.toString, actual))
        val firstDiff = firstDiffHint(expected, actual)
        fail(
          s"Snapshot mismatch for $name.$firstDiff\nActual : $actualFile\nDiff   : $diffFile\nRun with UPDATE_SNAPSHOTS=true to update.",
        )

  private def firstDiffHint(expected: String, actual: String): String =
    val expLines = expected.linesIterator.toVector
    val actLines = actual.linesIterator.toVector
    val firstDiff = expLines.zip(actLines).indexWhere { case (e, a) => e != a }
    if firstDiff >= 0 then
      s"\nFirst difference at line ${firstDiff + 1}:" +
      s"\n  - ${expLines(firstDiff)}" +
      s"\n  + ${actLines(firstDiff)}"
    else s"\nLine count changed: ${expLines.size} → ${actLines.size}"

  private def unifiedDiff(expectedPath: String, actual: String): String =
    val tmp = Files.createTempFile("snapshot-actual-", ".txt")
    try
      Files.writeString(tmp, actual)
      val proc =
        ProcessBuilder("diff", "-u", expectedPath, tmp.toString).start()
      val output = scala.io.Source.fromInputStream(proc.getInputStream).mkString
      proc.waitFor()
      output
    finally Files.delete(tmp)
