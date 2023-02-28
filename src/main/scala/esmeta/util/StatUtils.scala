package esmeta.util

import breeze.stats.distributions.{Binomial, ChiSquared, Gaussian}
import breeze.stats.distributions.Rand.VariableSeed.randBasis

import scala.collection.mutable.Map as MMap
import scala.math.sqrt

object StatUtils {

  /** one-tailed (left) binomial test */
  def leftBinTest(n: Int, p: Double, k: Int): Double =
    if n > 64 then {
      // large n => approx.
      val gaussian = new Gaussian(0, 1)
      val z =
        (k - n * p - 0.5) / sqrt(
          n * p * (1 - p),
        ) // 0.5 for continuity correction
      val pValue = 1 - gaussian.cdf(z)
      pValue
    } else {
      val binomial = new Binomial(n, p)
      val pValue = (k to n).map(binomial.probabilityOf).sum
      pValue
    }

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
    Matrix(
      ins.size,
      outs.size,
      matData,
    )
}

case class Matrix(rows: Int, cols: Int, data: List[Double]) {
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
