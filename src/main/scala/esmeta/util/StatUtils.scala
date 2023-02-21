package esmeta.util

import breeze.stats.distributions.ChiSquared
import breeze.stats.distributions.Rand.VariableSeed.randBasis
import scala.collection.mutable.Map as MMap

object StatUtils {

  /** data must have a default value */
  def chiSqIdpTest(data: MMap[String, MMap[String, Int]]): Double =
    val original = encodeData(data)
    val expected = original.nullExpected
    val difference = original - expected
    val chiSq = (original - expected).elemWiseSq.elemWiseDiv(expected)
    val dF = (original.rows - 1) * (original.cols - 1)
    if dF <= 0 then 1
    else
      val dist = new ChiSquared(dF.toDouble)
      val pValue = 1 - dist.cdf(chiSq.data.sum)
      pValue

  /** data must have a default value */
  private def encodeData(
    data: MMap[String, MMap[String, Int]],
  ): Matrix =
    val ins = data.filter(_._2.nonEmpty).keys
    val outs = data.values.flatMap(_.keys).toList.distinct
    println(ins)
    println(outs)
    Matrix(
      ins.size,
      outs.size,
      (for {
        inFeature <- data.filter(_._2.nonEmpty).keys
        outFeature <- data.values.flatMap(_.keys)
      } yield data(inFeature)(outFeature).toDouble).toList,
    )
}

case class Matrix(val rows: Int, val cols: Int, data: List[Double]) {
  {
    assert(rows * cols == data.size)
  }

  def apply(rowIdx: Int, colIdx: Int): Double = data(rowIdx * cols + colIdx)

  def +(that: Matrix): Matrix = {
    assert(rows == that.rows && cols == that.cols)
    this.copy(data = (data zip that.data).map((x, y) => x + y))
  }

  def -(that: Matrix): Matrix =
    assert(rows == that.rows && cols == that.cols)
    this.copy(data = (data zip that.data).map((x, y) => x - y))

  def elemWiseMul(that: Matrix): Matrix =
    assert(rows == that.rows && cols == that.cols)
    this.copy(data = (data zip that.data).map((x, y) => x * y))

  def elemWiseSq: Matrix = this.elemWiseMul(this)

  def elemWiseDiv(that: Matrix): Matrix =
    assert(rows == that.rows && cols == that.cols)
    this.copy(data = (data zip that.data).map((x, y) => x / y))

  private def rowSum(rowIdx: Int): Double =
    (0 until cols).map(this(rowIdx, _)).sum

  private def colSum(colIdx: Int): Double =
    (0 until rows).map(this(_, colIdx)).sum

  def nullExpected: Matrix = this.copy(data = data.zipWithIndex.map {
    case (_, idx) =>
      val rowIdx = idx / cols
      val colIdx = idx % cols
      (rowSum(rowIdx) * colSum(colIdx)) / (rows * cols)
  })
}
