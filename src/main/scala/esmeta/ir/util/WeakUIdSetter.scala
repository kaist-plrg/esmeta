package esmeta.ir.util

import esmeta.ir.*

/** weak unique id setters */
class WeakUIdSetter extends UnitWalker {
  private var iidCount: Int = 0
  private def newIId: Int = { val id = iidCount; iidCount += 1; id }
  private var eidCount: Int = 0
  private def newEId: Int = { val id = eidCount; eidCount += 1; id }
  override def walk(inst: Inst): Unit = { inst.id = newIId; super.walk(inst) }
  override def walk(expr: Expr): Unit = { expr.id = newEId; super.walk(expr) }
}
