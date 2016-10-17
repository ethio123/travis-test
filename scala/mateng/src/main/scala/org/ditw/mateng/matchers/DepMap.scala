package org.ditw.mateng.matchers

/**
  * Created by jiaji on 2016-02-10.
  */

import TMatcher._
import org.ditw.mateng.TMatchResultPool

import scala.collection.mutable
sealed class DepMap private {
  import TMatcher._
  import DepMap._
  private[matchers] val _map = mutable.Map[MId,mutable.SortedSet[MId]]()

  private def getValueSet(k:MId):mutable.SortedSet[MId] = {
    if (!_map.contains(k)) _map += k -> mutable.SortedSet()
    _map.get(k).get
  }

  //def +=(k:TId, v:TId):Unit = getValueSet(k) += v
  //def +=(k:TId, v:Seq[TId]):Unit = getValueSet(k) ++= v
  def +=(matcher:TMatcher) = {
    val depMatcherIds = matcher.depMatcherIds
    depMatcherIds.foreach(
      getValueSet(_) += matcher.id.get
    )
  }

  //todo: error handling

  def getMatcherIdsDepOn(id:MId):mutable.SortedSet[MId] = _map.get(id).getOrElse(EmptySortedSet)
}

object DepMap {
  def create = new DepMap

  val EmptySortedSet = mutable.SortedSet[MId]()

  val EmptySet = Set[IndexedSeq[MId]]()

  // merge circles that has overlap
  def mergeCircles(in:Set[IndexedSeq[MId]]):Set[Set[MId]] = {
    val r = mutable.Set[Set[MId]]()
    val s = mutable.Set[Set[MId]]()
    s ++= in.map(_.toSet)
    while (s.nonEmpty) {
      val x = s.head
      s -= x

      val overlaps = s.filter(other => x.intersect(other).nonEmpty)
      if (overlaps.nonEmpty) {
        s += x ++ overlaps.reduce(_ ++ _)
      }
      else r += x
    }

    r.toSet
  }

  def computeAllCircles(dep:Map[MId,Set[MId]]):Set[Set[MId]] = {
    val computed = mutable.Map[MId, Set[IndexedSeq[MId]]]()
    dep.keys.foreach(mid => _findCircles(mid, IndexedSeq[MId](), computed, dep))
    val circles = if (computed.values.nonEmpty) computed.values.reduce(_ ++ _) else EmptySet
    mergeCircles(circles)
  }

  private def _findCircles(start:MId, path:IndexedSeq[MId], computed:mutable.Map[MId, Set[IndexedSeq[MId]]], dep:Map[MId,Set[MId]]):Set[IndexedSeq[MId]] = {
    if (computed.contains(start)) computed(start)
    else {
      val r = if (path.contains(start)) {
        val idx = path.indices.find(path(_) == start).get
        Set(path.slice(idx, path.size) :+ start)
      }
      else {
        val depIds = dep.getOrElse(start, Set())
        val rec = depIds.map{ depId => _findCircles(depId, path :+ start, computed, dep) }
        if (rec.nonEmpty) rec.reduce(_ ++ _)
        else EmptySet
      }
      computed(start) = r
      r
    }

  }

  def findDepCircles(start:MId, dep:Map[MId,Set[MId]]):Set[IndexedSeq[MId]] = _findDepCircles(start, IndexedSeq(), dep)

  private def _findDepCircles(start:MId, path:IndexedSeq[MId], dep:Map[MId,Set[MId]]):Set[IndexedSeq[MId]] = {
    if (path.contains(start)) {
      val idx = path.indices.find(path(_) == start).get
      Set(path.slice(idx, path.size) :+ start)
    }
    else {
      val depIds = dep.getOrElse(start, Set())
      val rec = depIds.map{ depId => _findDepCircles(depId, path :+ start, dep) }
      if (rec.nonEmpty) rec.reduce(_ ++ _)
      else EmptySet
    }
  }
}
