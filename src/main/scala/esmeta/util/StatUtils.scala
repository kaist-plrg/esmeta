package esmeta.util

import breeze.stats.distributions.ChiSquared
import breeze.stats.distributions.Rand.VariableSeed.randBasis
import scala.collection.mutable.Map as MMap

object StatUtils {

  /** data must have a default value */
  def chiSqIdpTest(data: MMap[String, MMap[String, Int]]): Double =
    val original = encodeData(data)
    val expected = original.nullExpected
    val chiSq = (original - expected).elemWiseSq.elemWiseDiv(expected)
    val dF = (original.rows - 1) * (original.cols - 1)
    if dF <= 0 then 0
    else
      val dist = new ChiSquared(dF.toDouble)
      val pValue = 1 - dist.cdf(chiSq.data.sum)
      pValue

  /** data must have a default value */
  private def encodeData(
    data: MMap[String, MMap[String, Int]],
  ): Matrix =
    val ins = data.filter(_._2.nonEmpty).keys.toList
    val outs = data.values.flatMap(_.keys).toList.distinct
    val matData =
      (for {
        inFeature <- ins
        outFeature <- outs
      } yield data(inFeature).getOrElse(outFeature, 0).toDouble)
//    println(s"ins(${ins.size}): $ins")
//    println(s"outs(${outs.size}): $outs")
//    println(s"matData(${matData.size}): $matData")
    Matrix(
      ins.size,
      outs.size,
      matData,
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
