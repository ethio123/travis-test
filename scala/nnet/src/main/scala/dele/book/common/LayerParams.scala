package dele.book.common

import breeze.linalg.{DenseMatrix, DenseVector}

/**
  * Created by dele on 2016-09-15.
  */

case class LayerParams(w:DenseMatrix[Double], b:DenseVector[Double])

case class LayerParamsJson(layerIndex:Int, rows:Int, cols:Int, w:Array[Double], b:Array[Double])

case class LayerResults(param:LayerParams, in:DenseVector[Double]) {
  val z:DenseVector[Double] = param.w * in + param.b
  val z_der = NNet.activation_der(z)
  val a:DenseVector[Double] = NNet.activation(z)
}

case class LayerBackPropResults(costFunc:CostFunc, forwardResults:LayerResults, forwardResults_1:Option[LayerResults], delta_1:DenseVector[Double], isOutput:Boolean = false) {
  val delta =
    if (isOutput) delta_1 :* costFunc.der(forwardResults.z) //forwardResults.z_der
    else {
      val wd = forwardResults_1.get.param.w.t * delta_1
      wd :* forwardResults.z_der
    }

  val w_der:DenseMatrix[Double] = delta * forwardResults.in.t
  val b_der:DenseVector[Double] = delta
  def newWB(learnFactor:Double):(DenseMatrix[Double],DenseVector[Double]) =
    (forwardResults.param.w - w_der * learnFactor, forwardResults.param.b - b_der * learnFactor)
}
