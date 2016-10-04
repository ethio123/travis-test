package dele.book.common

import breeze.linalg.DenseVector

/**
  * Created by dele on 2016-10-01.
  */
trait CostFunc {
  def cost(out:DenseVector[Double], expected:DenseVector[Double]):Double
  def der(z:DenseVector[Double]):DenseVector[Double]
}


object CostFunc {
  object QuadraticCost extends CostFunc {
    def cost(out:DenseVector[Double], expected:DenseVector[Double]):Double = NNet.SquaredError(out, expected)
    def der(z:DenseVector[Double]) = NNet.activation_der(z)
  }

  object CrossEntropyCost extends CostFunc {
    def cost(out:DenseVector[Double], expected:DenseVector[Double]):Double = NNet.CrossEntropy(out, expected)
    def der(z:DenseVector[Double]):DenseVector[Double] = DenseVector.ones[Double](z.length)
  }
}