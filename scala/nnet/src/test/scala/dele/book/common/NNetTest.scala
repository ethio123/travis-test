package dele.book.common

import breeze.linalg.{DenseMatrix, DenseVector}
import org.scalatest.{FlatSpec, Matchers}


/**
  * Created by dele on 2016-09-15.
  */
class NNetTest extends FlatSpec with Matchers {
  //implicit val session = SparkUtils.init

  "init" should "gen random w,b values" in {
    val nnet = new NNet(IndexedSeq(2, 3, 1))
    val lp1 = nnet.layerParamMap(1)
    lp1.w.rows shouldBe 3
    lp1.w.cols shouldBe 2
    println(s"w:\n${lp1.w}")
    println(s"b:\n${lp1.b}")
  }

  val DoubleDiff = 1E-6
  "[2 2 2] network example" should "ok" in {
    val nnet = new NNet(
      IndexedSeq(2, 2, 2),
      IndexedSeq(
        new DenseMatrix[Double](2, 2, Array(0.15, 0.25, 0.2, 0.3)) -> new DenseVector[Double](Array(0.35, 0.35)),
        new DenseMatrix[Double](2, 2, Array(0.4, 0.5, 0.45, 0.55)) -> new DenseVector[Double](Array(0.6, 0.6))
      )
    )
    //val lp1 = nnet.layerParamMap(1)
    //lp1.w.rows shouldBe 2
    //lp1.w.cols shouldBe 2
    //println(s"w:\n${lp1.w}")
    //println(s"b:\n${lp1.b}")
    val in = new DenseVector[Double](Array(0.05, 0.1))
    val out = nnet.forward(in)
    val diff = out - new DenseVector[Double](Array(0.75136507, 0.772928465))
    diff.foreach(_ shouldBe <(DoubleDiff))

    val expected = new DenseVector[Double](Array(0.01, 0.99))
    implicit val costFunc = NNet.SquaredError
    val cost = nnet.error(in, expected)
    cost-0.298371109 shouldBe <(DoubleDiff)

    nnet.backprop(in, expected, CostFunc.QuadraticCost)

    nnet.updateByBatch(
      Array(
        in -> expected
      ),
      0.5,
      CostFunc.QuadraticCost
    )
    //nnet.layerParamMap shouldBe 2
  }

  "[2 2 2] network example compared to python" should "ok" in {
    val nnet = new NNet(
      IndexedSeq(2, 2, 2),
      IndexedSeq(
        new DenseMatrix[Double](2, 2, Array(0.15, 0.35, 0.25, 0.45)) -> new DenseVector[Double](Array(0.05, 0.95)),
        new DenseMatrix[Double](2, 2, Array(0.55, 0.75, 0.65, 0.85)) -> new DenseVector[Double](Array(1.05, 0.01))
      )
    )
    //val lp1 = nnet.layerParamMap(1)
    //lp1.w.rows shouldBe 2
    //lp1.w.cols shouldBe 2
    //println(s"w:\n${lp1.w}")
    //println(s"b:\n${lp1.b}")
    val in = new DenseVector[Double](Array(0.1, 0.2))
    val out = nnet.forward(in)
    //    val diff = out - new DenseVector[Double](Array(0.75136507, 0.772928465))
    //    diff.foreach(_ shouldBe <(DoubleDiff))

    val expected = new DenseVector[Double](Array(0.8, 0.9))
    implicit val costFunc = NNet.SquaredError

    //nnet.backprop(in, expected, CostFunc.QuadraticCost)

    nnet.updateByBatch(
      Array(
        in -> expected
      ),
      3.0,
      CostFunc.QuadraticCost
    )
    //nnet.layerParamMap shouldBe 2
  }


}
