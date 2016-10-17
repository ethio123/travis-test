package org.ditw.mateng.matchers

import org.ditw.mateng.ErrorHandling.NotImplemented
import org.ditw.mateng.AtomSeqMatch

/**
  * Created by jiaji on 2016-10-08.
  */
import MatcherOpCache._
class MatcherOpCache {
  import collection.mutable

  private val cacheMap = mutable.Map[TMatcher, MatcherOpInfo]()
  def isCached(matcher:TMatcher):Boolean = cacheMap.contains(matcher)

  def cache(matcher:TMatcher, depMatcherIds:Set[String], results:Set[AtomSeqMatch]): Unit = {
    if (cacheMap.contains(matcher)) throw NotImplemented("already caches?!")
    else {
      cacheMap(matcher) = new MatcherOpInfo(matcher, depMatcherIds, results)
    }
  }

  /*
  def getCached(matcher:TMatcher):Set[AtomSeqMatch] = {
    if (!cacheMap.contains(matcher)) throw NotImplemented("not cached!")
    else cacheMap(matcher).results
  }
  */
  def cached(matcher:TMatcher):Option[Set[AtomSeqMatch]] = cacheMap.get(matcher).map(_.results)

  def invalidate(matcher:TMatcher):Unit = {
    if (matcher.id.nonEmpty) {
      val toRemove = cacheMap.filter(p => p._2.depMatcherIds.contains(matcher.id.get))
      toRemove.foreach(p => cacheMap.remove(p._1))
    }
  }
}

object MatcherOpCache {
  private[matchers] class MatcherOpInfo(val matcher:TMatcher, val depMatcherIds:Set[String], val results:Set[AtomSeqMatch])
}