package dele.book.common

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.numerics.{exp, log, sigmoid}
import org.json4s.DefaultFormats

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

  def SGD(training:Array[(Array[Double],Array[Double])], testData:Array[(Array[Double],Array[Double])], batchSize:Int, epochs:Int, eta:Double):Unit = {
    val idxRange = training.length-batchSize
    //val randIndices = .map(_ => (Math.random*idxRange).toInt / idxRange).toArray

    (1 to epochs).foreach { epk =>
      val randIdx = (Math.random*idxRange).toInt / idxRange
      val tr2 = training.slice(randIdx, training.length) ++ training.slice(0, randIdx)
      val mini_batches = (0 until training.length/batchSize).map(i => training.slice(i*batchSize, i*batchSize+batchSize))
      //val s10 = training.slice(randIdx, randIdx + batchSize).map(p => SparkUtils.normalizePixelArray(p._1) -> p._2)
      mini_batches.foreach{ b =>
        updateByBatch(
          b.map(d => new DenseVector(d._1) -> new DenseVector(d._2)),
          eta,
          CostFunc.CrossEntropyCost
        )
      }
      val c = evaluate(testData)
      println(s"$epk: $c / ${testData.length}")
    }
    println("\n\n")

  }

  def evaluate(testData:Array[(Array[Double],Array[Double])]) = {
    val ff = testData.map(td => forward(new DenseVector(td._1)) -> td._2)
    val ffr = ff.map{p =>
      val r = p._1.toArray
      val res = r.indices.maxBy(p._1(_))
      val expected = p._2.indices.filter(p._2(_) > 0.1).head
      res -> expected
    }
    val correctCount = ffr.count(p => p._1 == p._2)
    correctCount
  }

  def evaluate(in:Array[Double]):Int = {
    val out:Array[Double] = forward(new DenseVector(in)).toArray
    out.indices.maxBy(i => out(i))
  }

  import org.json4s.jackson.Serialization._

  def toJson = {
    val ordered = layerParamMap.keys.toList.sorted.map{ lidx =>
      val lp = layerParamMap(lidx)
      LayerParamsJson(lidx, lp.w.rows, lp.w.cols, lp.w.toArray, lp.b.toArray)
    }
    writePretty(ordered)
  }

}

object NNet {
  implicit val _fmt = DefaultFormats
  val EmptyInit = IndexedSeq[(DenseMatrix[Double],DenseVector[Double])]()

  val NormalDistr = breeze.stats.distributions.Gaussian(0,1)

  val SquaredError:(DenseVector[Double],DenseVector[Double]) => Double = (v1, v2) => {
    val diff:DenseVector[Double] = (v1 - v2)
    val squared:DenseVector[Double] = diff :* diff
    squared.toArray.sum/2
  }

  //def sigmoid(x:Double) = 1/(exp(-x)+1)
  //def sigmoid_der(x:Double) = sigmoid(x)*(1-sigmoid(x))
  def activation(x:DenseVector[Double]):DenseVector[Double] = sigmoid(x)//x.map(sigmoid)
  def activation_der(x:DenseVector[Double]):DenseVector[Double] = sigmoid(x) - (sigmoid(x):*sigmoid(x))

  val CrossEntropy:(DenseVector[Double],DenseVector[Double]) => Double = (v1, v2) => {
    val ones = DenseVector.ones[Double](v1.length)
    val all:DenseVector[Double] = v2 :* log(v1) + (ones-v2) :* log(ones-v1)
    //val squared:DenseVector[Double] = diff :* diff
    all.toArray.sum
  }


  import org.json4s.jackson.JsonMethods._
  def fromJson(j:String):NNet = {
    val layerParams = parse(j).extract[List[LayerParamsJson]]
    val nodeCounts = ListBuffer[Int]()
    nodeCounts += layerParams.head.cols
    nodeCounts ++= layerParams.map(_.rows)
    val lps = layerParams.map{ lp =>
      new DenseMatrix[Double](lp.rows, lp.cols, lp.w) -> new DenseVector[Double](lp.b)
    }
    new NNet(nodeCounts.toIndexedSeq, lps.toIndexedSeq)
  }
}