package esmeta.es.util.fuzzer

import esmeta.BASE_DIR
import esmeta.es.Script
import esmeta.es.util.injector.Injector
import esmeta.es.util.{Coverage, USE_STRICT}
import esmeta.js.Target
import esmeta.phase.ConformTest
import esmeta.util.SystemUtils.{listFiles, readFile}

object SelectionEval {
  lazy val bugDB: List[String] = ???

  def getCoverage(baseDir: String): Coverage = Coverage.fromLog(baseDir)

  def checkBug(bugScript: Script): Boolean = ???

  def evaluate(
    baseDir: String,
    targets: Iterable[Target],
  ): (Double, Double, Int) =
    println("evaluation start...")
    val numMinimals = getCoverage(baseDir).minimalScripts.size
    var expGroup: Map[Int, Int] = Map()
    val fileList = listFiles(s"$BASE_DIR/reported-bugs")
    println(s"found ${fileList.size} js bug files")
    var idx = 0
    var cleanHit = 0
    for {
      bugCode <- fileList
      name = bugCode.getName
      code = USE_STRICT + readFile(bugCode.getPath).trim()
      script = Script(code, name)
    } {
      if (idx % 10 == 0) {
        println(s"index: $idx")
      }
      val cov = getCoverage(baseDir)
      val (_, _, covered, blockingSet) = cov.runAndCheckBlockings(script)
      val minBugCodeSize =
        if covered || blockingSet.isEmpty then {
          println(s"index $idx clean hit!"); cleanHit += 1; 1000
        } else
          blockingSet.toList
            .sortBy(-_.code.length)
            .foldLeft(0) {
              case (codeSize, script) =>
                if codeSize >= script.code.length then codeSize
                else if (
                  !targets.foldLeft(false) {
                    case (isTargetBug, target) =>
                      if isTargetBug then true
                      else
                        ConformTest
                          .doConformTest(
                            target,
                            target.isTrans,
                            Script(
                              Injector(script.code, true, false).toString,
                              script.name,
                            ),
                            true,
                          )
                          .isDefined
                  },
                ) then 0
                else script.code.length
            }

      expGroup += idx -> minBugCodeSize
      idx += 1
    }
    println("controlGroup start ######################")
    println(expGroup)
    println("#########################################")
    val avgBugSize1k =
      controlGroup1k.values.sum.toDouble / controlGroup1k.size.toDouble
    val avgBugSize2k =
      controlGroup2k.values.sum.toDouble / controlGroup2k.size.toDouble
    val avgBugSizeExp =
      expGroup.values.sum.toDouble / expGroup.size.toDouble
    println(s"# of clean hits: $cleanHit")
    println(
      s"avgBugSize: $avgBugSizeExp, avgBugSize1k: $avgBugSize1k, avgBugSize2k: $avgBugSize2k",
    )
    val ratioAvgBugSize1k = expGroup.toList.map {
      case (i, l) => l.toDouble / controlGroup1k(i).toDouble
    }.sum / expGroup.size.toDouble
    val ratioAvgBugSize2k = expGroup.toList.map {
      case (i, l) => l.toDouble / controlGroup2k(i).toDouble
    }.sum / expGroup.size.toDouble

    (ratioAvgBugSize1k, ratioAvgBugSize2k, numMinimals)

  private val engineTargets: List[Target] = List(
    Target("d8", false),
    Target("node", false),
    Target("js", false),
    Target("sm", false),
    Target("jsc", false),
  )

  private val transTargets: List[Target] = List(
    Target("babel", true),
    Target("swc", true),
    Target("terser", true),
    Target("obfuscator", true),
  )
  val controlGroup1k = Map(
    69 -> 77,
    138 -> 60,
    101 -> 72,
    0 -> 45,
    88 -> 60,
    170 -> 60,
    115 -> 72,
    5 -> 26,
    120 -> 53,
    10 -> 49,
    56 -> 35,
    142 -> 82,
    153 -> 35,
    174 -> 72,
    42 -> 60,
    24 -> 58,
    37 -> 60,
    25 -> 45,
    52 -> 61,
    14 -> 72,
    110 -> 72,
    125 -> 35,
    157 -> 60,
    20 -> 48,
    46 -> 35,
    93 -> 82,
    152 -> 35,
    57 -> 89,
    78 -> 82,
    29 -> 35,
    164 -> 61,
    179 -> 38,
    106 -> 60,
    121 -> 26,
    84 -> 82,
    147 -> 60,
    61 -> 35,
    132 -> 82,
    116 -> 53,
    1 -> 60,
    74 -> 77,
    89 -> 82,
    133 -> 60,
    6 -> 60,
    85 -> 60,
    102 -> 40,
    60 -> 48,
    117 -> 60,
    160 -> 81,
    70 -> 60,
    165 -> 60,
    33 -> 60,
    28 -> 77,
    38 -> 82,
    21 -> 77,
    137 -> 60,
    92 -> 60,
    65 -> 97,
    97 -> 60,
    156 -> 72,
    9 -> 40,
    53 -> 60,
    169 -> 46,
    141 -> 46,
    109 -> 82,
    124 -> 82,
    77 -> 26,
    96 -> 63,
    173 -> 40,
    13 -> 60,
    129 -> 82,
    41 -> 72,
    134 -> 63,
    73 -> 35,
    128 -> 46,
    105 -> 60,
    2 -> 60,
    166 -> 35,
    148 -> 60,
    45 -> 72,
    161 -> 63,
    64 -> 60,
    17 -> 32,
    149 -> 45,
    32 -> 97,
    34 -> 60,
    176 -> 60,
    22 -> 35,
    44 -> 46,
    27 -> 82,
    59 -> 26,
    118 -> 60,
    71 -> 26,
    12 -> 26,
    54 -> 89,
    144 -> 60,
    49 -> 45,
    86 -> 63,
    159 -> 72,
    172 -> 60,
    113 -> 60,
    81 -> 60,
    76 -> 82,
    7 -> 55,
    39 -> 120,
    98 -> 60,
    103 -> 35,
    140 -> 48,
    91 -> 45,
    66 -> 119,
    155 -> 120,
    108 -> 60,
    130 -> 82,
    135 -> 77,
    3 -> 60,
    80 -> 35,
    167 -> 82,
    35 -> 48,
    162 -> 72,
    112 -> 60,
    123 -> 82,
    145 -> 49,
    48 -> 63,
    63 -> 82,
    18 -> 26,
    150 -> 69,
    95 -> 45,
    50 -> 60,
    67 -> 35,
    177 -> 36,
    16 -> 49,
    127 -> 63,
    31 -> 53,
    154 -> 60,
    11 -> 53,
    72 -> 58,
    175 -> 26,
    143 -> 119,
    43 -> 26,
    99 -> 97,
    87 -> 46,
    104 -> 106,
    40 -> 35,
    26 -> 48,
    158 -> 82,
    171 -> 46,
    139 -> 58,
    23 -> 26,
    55 -> 35,
    114 -> 35,
    8 -> 56,
    75 -> 60,
    82 -> 69,
    119 -> 97,
    58 -> 35,
    151 -> 53,
    36 -> 82,
    168 -> 53,
    146 -> 38,
    30 -> 82,
    51 -> 35,
    19 -> 40,
    107 -> 60,
    4 -> 106,
    79 -> 26,
    94 -> 106,
    126 -> 97,
    136 -> 72,
    15 -> 63,
    163 -> 48,
    68 -> 35,
    62 -> 63,
    178 -> 60,
    131 -> 60,
    47 -> 26,
    122 -> 72,
    83 -> 53,
    100 -> 60,
    90 -> 38,
    111 -> 60,
  )
  val controlGroup2k = Map(
    69 -> 64,
    138 -> 48,
    101 -> 48,
    0 -> 48,
    88 -> 71,
    170 -> 48,
    115 -> 48,
    5 -> 120,
    120 -> 48,
    10 -> 48,
    56 -> 48,
    142 -> 48,
    153 -> 48,
    174 -> 83,
    42 -> 71,
    24 -> 83,
    37 -> 57,
    25 -> 48,
    52 -> 71,
    14 -> 48,
    110 -> 71,
    125 -> 54,
    157 -> 71,
    20 -> 48,
    46 -> 48,
    93 -> 80,
    152 -> 110,
    57 -> 71,
    78 -> 83,
    29 -> 94,
    164 -> 48,
    179 -> 48,
    106 -> 83,
    121 -> 83,
    84 -> 48,
    147 -> 48,
    61 -> 48,
    132 -> 83,
    116 -> 83,
    1 -> 48,
    74 -> 83,
    89 -> 83,
    133 -> 83,
    6 -> 83,
    85 -> 48,
    102 -> 83,
    60 -> 53,
    117 -> 83,
    160 -> 53,
    70 -> 48,
    165 -> 94,
    33 -> 91,
    28 -> 83,
    38 -> 83,
    21 -> 83,
    137 -> 83,
    92 -> 83,
    65 -> 48,
    97 -> 83,
    156 -> 60,
    9 -> 48,
    53 -> 48,
    169 -> 53,
    141 -> 72,
    109 -> 71,
    124 -> 83,
    77 -> 48,
    96 -> 83,
    173 -> 53,
    13 -> 48,
    129 -> 83,
    41 -> 56,
    134 -> 48,
    73 -> 120,
    128 -> 83,
    105 -> 83,
    2 -> 83,
    166 -> 54,
    148 -> 54,
    45 -> 48,
    161 -> 53,
    64 -> 120,
    17 -> 83,
    149 -> 71,
    32 -> 94,
    34 -> 48,
    176 -> 71,
    22 -> 48,
    44 -> 71,
    27 -> 53,
    59 -> 53,
    118 -> 83,
    71 -> 83,
    12 -> 71,
    54 -> 71,
    144 -> 83,
    49 -> 48,
    86 -> 48,
    159 -> 83,
    172 -> 71,
    113 -> 48,
    81 -> 83,
    76 -> 83,
    7 -> 83,
    39 -> 83,
    98 -> 83,
    103 -> 48,
    140 -> 71,
    91 -> 83,
    66 -> 83,
    155 -> 60,
    108 -> 48,
    130 -> 84,
    135 -> 48,
    3 -> 48,
    80 -> 71,
    167 -> 54,
    35 -> 72,
    162 -> 83,
    112 -> 84,
    123 -> 30,
    145 -> 48,
    48 -> 30,
    63 -> 48,
    18 -> 60,
    150 -> 83,
    95 -> 54,
    50 -> 64,
    67 -> 48,
    177 -> 120,
    16 -> 48,
    127 -> 83,
    31 -> 83,
    154 -> 48,
    11 -> 48,
    72 -> 48,
    175 -> 48,
    143 -> 71,
    43 -> 71,
    99 -> 71,
    87 -> 71,
    104 -> 83,
    40 -> 83,
    26 -> 55,
    158 -> 57,
    171 -> 71,
    139 -> 72,
    23 -> 84,
    55 -> 83,
    114 -> 48,
    8 -> 48,
    75 -> 58,
    82 -> 30,
    119 -> 83,
    58 -> 60,
    151 -> 110,
    36 -> 48,
    168 -> 71,
    146 -> 48,
    30 -> 48,
    51 -> 83,
    19 -> 48,
    107 -> 48,
    4 -> 69,
    79 -> 48,
    94 -> 71,
    126 -> 48,
    136 -> 51,
    15 -> 120,
    163 -> 53,
    68 -> 48,
    62 -> 57,
    178 -> 71,
    131 -> 48,
    47 -> 83,
    122 -> 57,
    83 -> 71,
    100 -> 83,
    90 -> 91,
    111 -> 83,
  )
}
