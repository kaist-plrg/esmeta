package esmeta.test262

import esmeta.*

trait Test262Test extends ESMetaTest {
  def category: String = "test262"
}
object Test262Test {
  lazy val test262 = Test262(Test262.currentVersion)
}
