package esmeta.util

/** weak unique ids */
trait WeakUId[+T <: WeakUId[T]] { self: T =>

  /** unique ids */
  var id: Int = -1

  /** get simple string */
  def simpleString: String = s"${getClass.getSimpleName}[$id]"

  /** reference for weak unique ids */
  def idRef: WeakUIdRef[T] = new WeakUIdRef[T](id) { def get: T = self }

  /** override hashCode using unique ids */
  override def hashCode: Int = id
}

/** reference for weak unique ids */
trait WeakUIdRef[+T <: WeakUId[T]](val id: Int) {

  /** getter for the original data */
  def get: T

  /** get simple string */
  def simpleString: String = get.simpleString
}
