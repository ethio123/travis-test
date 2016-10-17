package org.ditw.mateng.utils

import org.ditw.mateng.ErrorHandling.MatcherErrorNoConsecutiveNegMatchers

import scala.collection.mutable.{ListBuffer, ArrayBuffer}

/**
  * Created by jiaji on 2016-02-11.
  */
//import scala.collection.mutable.Seq

object HelperFuncs {
  private def _combine2[T](a:Set[Seq[T]], b:Set[T]):Set[Seq[T]] = for (x <- a; y <- b) yield x ++ Seq(y)
  def combine[T](s:Seq[Set[T]]):Set[Seq[T]] = s.foldLeft(Set[Seq[T]](Seq()))(_combine2[T])

  def it2Map[T](it:Iterable[T])(keyFunc:T => String):Map[String,T] = it.map(elem => (keyFunc(elem), elem)).toMap

  def negMatcherIndexTransform(indices:IndexedSeq[Int]):IndexedSeq[Int] = {
    checkNegMatacherIndices(indices)
    val r:ArrayBuffer[Int] = ArrayBuffer()
    var offset = 0
    indices.foreach(
      idx => {
        r += idx-offset
        offset = offset + 1
      }
    )
    r
  }


  def checkNegMatacherIndices(indices:IndexedSeq[Int]):Unit = {
    if (indices.nonEmpty) {
      (1 to indices.size-1).foreach(
        idx => {
          if (indices(idx) - indices(idx-1) <= 1) throw MatcherErrorNoConsecutiveNegMatchers(indices)
        }
      )
    }
  }

  def calcInBetweenRanges(rangeSeq:Seq[Range]):List[Option[Range]] = {
    val allIndexes = new Array[Int](rangeSeq.size*2)
    rangeSeq.indices.foreach(idx => {
      allIndexes(idx*2) = rangeSeq(idx).start
      allIndexes(idx*2+1) = rangeSeq(idx).end
    })
    val r = ListBuffer[Option[Range]]()
    (0 to rangeSeq.size-2).foreach(idx => {
      val start = allIndexes(1+idx*2)+1
      val end = allIndexes(2+idx*2)-1
      r += (if (end >= start) Option(start to end) else None)
    })
    r.toList
  }

  def allGaps(all:Range, rangeSeq:Seq[Range]):List[Option[Range]] = {
    var ranges = calcInBetweenRanges(rangeSeq)
    val s = rangeSeq.head.start
    val head = if (s > 0) Option(0 to s-1) else None
    ranges =  head:: ranges

    val e = rangeSeq.last.end
    val tail = if (e < all.last) Option(e+1 to all.last) else None
    ranges ::: List(tail)
  }

}
