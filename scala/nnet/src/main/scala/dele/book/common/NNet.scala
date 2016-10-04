package dele.book.common

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.numerics.{exp, log}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by dele on 2016-09-15.
  */
import NNet._

class NNet(val layerNodeCounts: IndexedSeq[Int], init:IndexedSeq[(DenseMatrix[Double],DenseVector[Double])] = EmptyInit) {

  private def genLayerParams(init:IndexedSeq[(DenseMatrix[Double],DenseVector[Double])] = EmptyInit):Map[Int,LayerParams] = {
    mutable.Map[Int,LayerParams]()
    // init
    (1 until layerNodeCounts.size).map{ lidx =>
      val rows = layerNodeCounts(lidx)
      val cols = layerNodeCounts(lidx-1)

      val (w, b) = if (init.isEmpty) {
        (DenseMatrix.rand(rows, cols, NormalDistr)) -> //SparkUtils.NormalDistrSamples(rows*cols)) -> //Matrices.dense(rows, cols, Array.fill[Double](rows*cols)(Math.random))//normalVectorRDD(session.sparkContext, layerNodeCounts(lidx), layerNodeCounts(lidx-1)).map()
          DenseVector.rand(rows, NormalDistr)
      }
      else {
        init(lidx-1)
      }
      lidx -> LayerParams(w, b)
    }.toMap
  }
  var layerParamMap = genLayerParams(init)

  def forward(in:DenseVector[Double]):DenseVector[Double] = {
    var lin = in
    (1 until layerNodeCounts.size).map{ lidx =>
      val lp = layerParamMap(lidx)
      val lr = LayerResults(lp, lin)
      lin = lr.a
    }
    lin
  }

  def error(in:DenseVector[Double], expected:DenseVector[Double])(implicit costFunc:(DenseVector[Double],DenseVector[Double]) => Double):Double = {
    val out = forward(in)
    costFunc(out, expected)
  }

  def backprop(x:DenseVector[Double], y:DenseVector[Double], costFunc:CostFunc):Map[Int,LayerBackPropResults] = {
    var in = x
    val layerResults = mutable.Map[Int, LayerResults]()

    var lin = in
    (1 until layerNodeCounts.size).map{ lidx =>
      val lp = layerParamMap(lidx)
      val lr = LayerResults(lp, lin)
      layerResults(lidx) = lr
      lin = lr.a
    }

    val diff = lin - y
    val layerIndices = layerResults.keys.toList.sorted.reverse
    var delta_1 = diff
    layerIndices.map{ lidx =>
      val isOutputLayer = lidx == layerNodeCounts.size-1
      val bpResult = LayerBackPropResults(costFunc, layerResults(lidx), if (isOutputLayer) None else Option(layerResults(lidx+1)), delta_1, isOutputLayer)
      delta_1 = bpResult.delta
      //println("New WB:")
      //println(bpResult.newWB(lfactor))
      lidx -> bpResult
    }.toMap
  }

  val _lastLayerIndex = layerNodeCounts.size-1

  def updateByBatch(batchInput:Array[(DenseVector[Double], DenseVector[Double])], lfactor:Double, costFunc:CostFunc):Unit = {
    val bpResults = batchInput.map{ case (x, y) => backprop(x, y, costFunc) }
    //val costs = batchInput.indices.map { idx =>
    //  costFunc(bpResults(idx)(_lastLayerIndex).forwardResults.a, batchInput(idx)._2)
    //}
    //println(s"cost: ${costs.sum}")
    val all = (1 until layerNodeCounts.size).map{ lidx =>
      lidx -> bpResults.map(r => r(lidx).w_der -> r(lidx).b_der)
    }.toMap

    val avg:Map[Int,(DenseMatrix[Double],DenseVector[Double])] = all.map{ case (idx, tpList) =>
      val lp = layerParamMap(idx)
      val s = tpList.foldLeft(new DenseMatrix[Double](lp.w.rows, lp.w.cols) -> new DenseVector[Double](lp.b.length))((p1, p2) => (p1._1 + p2._1, p1._2 + p2._2))
      idx -> (s._1*(lfactor/batchInput.length), s._2*(lfactor/batchInput.length))
    }

    val newWB = (1 until layerNodeCounts.size).map { lidx =>
      //val nonZeroDelta = avg(lidx)._1.toArray.filter(_ > 1E-6)
      //if (lidx == 1 && nonZeroDelta.nonEmpty) {
      //  println(nonZeroDelta)
      //}
      val newW = layerParamMap(lidx).w - avg(lidx)._1
      val newB = layerParamMap(lidx).b - avg(lidx)._2
      newW -> newB
    }
    layerParamMap = genLayerParams(newWB)
  }
}

object NNet {
  val EmptyInit = IndexedSeq[(DenseMatrix[Double],DenseVector[Double])]()

  val NormalDistr = breeze.stats.distributions.Gaussian(0,1)

  val SquaredError:(DenseVector[Double],DenseVector[Double]) => Double = (v1, v2) => {
    val diff:DenseVector[Double] = (v1 - v2)
    val squared:DenseVector[Double] = diff :* diff
    squared.toArray.sum/2
  }

  def sigmoid(x:Double) = 1/(exp(-x)+1)
  def sigmoid_der(x:Double) = sigmoid(x)*(1-sigmoid(x))
  def activation(x:DenseVector[Double]):DenseVector[Double] = x.map(sigmoid)
  def activation_der(x:DenseVector[Double]):DenseVector[Double] = x.map(sigmoid_der)

  val CrossEntropy:(DenseVector[Double],DenseVector[Double]) => Double = (v1, v2) => {
    val ones = DenseVector.ones[Double](v1.length)
    val all:DenseVector[Double] = v2 :* log(v1) + (ones-v2) :* log(ones-v1)
    //val squared:DenseVector[Double] = diff :* diff
    all.toArray.sum
  }
}