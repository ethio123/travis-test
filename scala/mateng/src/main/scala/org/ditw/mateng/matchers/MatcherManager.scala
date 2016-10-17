package org.ditw.mateng.matchers

import org.ditw.mateng.ErrorHandling.MatcherManagerErrorMatcherIdAlreadyExist
import MatcherManager._
import TMatcher.MId
import org.ditw.mateng._
import org.ditw.mateng.{AtomSeqMatch, TContext, TInput, TMatchResultPool}

import scala.collection.immutable.ListSet
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Try

/**
  * Created by jiaji on 2016-02-10.
  */
sealed class MatcherManager private () {
  import TMatcher._
  import DepMap._

  implicit val matcherOrder:Ordering[TMatcher] = Ordering.by(m => m.id)

  private val matchers = mutable.SortedSet[TMatcher]()

  private val matcherMap = mutable.Map[MId, TMatcher]()

  def allMIds = matcherMap.keySet.toSet

  private val depMap = DepMap.create

  def add(matcher:TMatcher) = {
    matchers += matcher
    if (matcher.id.nonEmpty) {
      val mid = matcher.id.get
      if (matcherMap.contains(mid))
        throw MatcherManagerErrorMatcherIdAlreadyExist(getMatcherBy(mid), matcher)
      matcherMap += mid -> matcher
      depMap += matcher
    }
  }

  def getMatcherBy(id:String):TMatcher = matcherMap.get(id).get

  import scala.collection.mutable
  private def mergeMatches(matches:Set[AtomSeqMatch]):Set[AtomSeqMatch] = {
    if (matches.size <= 1) matches
    else {
      var merged = mutable.Set[AtomSeqMatch]()
      matches.foreach(
        m => {
          var canMerge = false
          val mergedTmp = mutable.Set[AtomSeqMatch]()
          merged.foreach { mm =>
            val c = MatcherManager.testEqual(m, mm) // MatcherManager.testCover(m, mm)
            if (c.nonEmpty) {
              mergedTmp += c.get
              canMerge = true
            }
            else {
              mergedTmp += mm
            }
          }
          if (!canMerge) mergedTmp += m
          merged = mergedTmp
        }
      )
      merged.toSet
    }
  }

  private def _mergeOverlap(resultPool:TMatchResultPool): Unit = {
    resultPool.allMatcherIds.foreach{ mid =>
      val matches = resultPool.query(mid)
      //println(s"merging $mid: $matches")

      val merged = mergeMatches(matches)
      resultPool.update(matcherMap(mid), merged)
    }
  }

  private var stoppedByMatcherManager:StoppedByMatcherManager = new StoppedByMatcherManager(Map(), AlwaysPassChecker)
  def setStoppedByManager(sbm:StoppedByMatcherManager):Unit = {
    stoppedByMatcherManager = sbm
  }

  lazy val (independentMIds, depCircles, independentMIds2, otherMIds) = {
    val depmap = matcherMap.map(p => p._1 -> p._2.depMatcherIds).toMap
    val (indep, rem) = shuffleBasedOnDependency(depmap)
    val m = depMap._map.map(p => p._1 -> p._2.toSet).toMap
    val depCircleInfo = computeAllCircles(m)

    val excludeSet = indep.toSet ++ depCircleInfo.flatten
    val depmap2 = depmap.filter(p => !excludeSet.contains(p._1))
    val (indep2, rem2) = shuffleBasedOnDependency(depmap2, excludeSet)
    //val other = rem2 -- depCircleInfo.flatten

    (indep.toList, depCircleInfo, indep2, rem2)
  }

  /*
  private def isAlmostContainedIn(m1:AtomSeqMatch, m2:AtomSeqMatch):Boolean = {
    val intersect = m1.range.intersect(m2.range)
    if (intersect != m1.range) false
    else m1.leafMatches subsetOf m2.leafMatches
  }
  */

  /*
  private def almostEqual(m1:AtomSeqMatch, m2:AtomSeqMatch):Boolean = m2.range == m1.range && m1.leafMatches == m2.leafMatches

  /// Note: almostContained requires:
  ///   1. ranges being equal (extremely important! otherwise, we'll end in tricky dead-loop situations
  ///   2. leaf matches being contained
  private def almostContained(m1:AtomSeqMatch, m2:AtomSeqMatch):Boolean = m2.range == m1.range && (m1.leafMatches subsetOf m2.leafMatches)

  private def setAlmostEqual(m1:Set[AtomSeqMatch], m2:Set[AtomSeqMatch]):Boolean = m1.size == m2.size && m1.forall(a => m2.exists(almostEqual(a, _)))

  private[matchers] def removeAlmostDuplicates(matches:Set[AtomSeqMatch]):Set[AtomSeqMatch] = {
    val r = mutable.Set[AtomSeqMatch]()
    matches.foreach{ m =>
      //if (!r.exists(almostEqual(m, _))) {
      //  r += m
      //}
      // almostContained is a more radical option, todo: investigate the difference
      if (!r.exists(almostContained(m, _))) {
        val toRemove = r.filter(almostContained(_, m))
        r --= toRemove
        r += m
      }
    }
    r.toSet
  }
  */

  private[matchers] def removeDuplicates(matches:Set[AtomSeqMatch]):Set[AtomSeqMatch] = {
    val r = mutable.Set[AtomSeqMatch]()
    matches.foreach{ m =>
      //if (!r.exists(almostEqual(m, _))) {
      //  r += m
      //}
      // almostContained is a more radical option, todo: investigate the difference
      if (!r.contains(m)) r += m
    }
    r.toSet

  }

  private def runMatchers(input:TInput, matcherIds:List[MId], filteredByContext:Set[MId], resultPool:TMatchResultPool):Set[MId] = {
    val s = mutable.LinkedHashSet[MId]() //matcherMap.keySet.toSet //note: use this to enforce order -- matcherMap.keySet.toList.sorted.toSet
    s ++= matcherIds
    //val ttt = s.filter(!stoppedByMatcherManager.contextChecker(input.context, _))
    //println(ttt)
    s --= filteredByContext

    val r = mutable.LinkedHashSet[MId]()

    while (s.nonEmpty) {
      val mid = s.head
      //ctr.incMCount(mid)
      s -= mid
      val matcher = matcherMap.get(mid).get
      val existing = resultPool.query(mid)
      val rawMatches = matcher.m(resultPool)

      //input.context.getFlag()
      var matches = removeDuplicates(rawMatches) // removeAlmostDuplicates(rawMatches)
      if (matches.nonEmpty) matches = stoppedByMatcherManager.filter(mid, matches, resultPool)
      //if (mergeOverlap) {
      //matches = removeAlmostDuplicates(matches) //mergeMatches(matches)
      //}
      //if (!setAlmostEqual(matches, existing)) {
      // needs optimization!!
      //val oldmc = AtomSeqMatch.t
      if (matches != existing) {
        val depMatcherIds = depMap.getMatcherIdsDepOn(mid)
        val toUpdate = (stoppedByMatcherManager.updateMatches(mid, matches, resultPool) ++ depMatcherIds).toSet -- filteredByContext
        val contained = matcherIds.filter(toUpdate.contains)
        s ++= contained
        r ++= (toUpdate -- contained)
        resultPool.update(matcher, matches)
      }
      //println(s"equality test count: ${AtomSeqMatch.t - oldmc}")
      //println(s"$mid: ${s.size}")
    }

    r.toSet
  }

  private def runMatcherList(s:mutable.Set[MId], input:TInput, matcherIds:List[MId], filteredByContext:Set[MId], resultPool:TMatchResultPool, remainingSet:Set[MId]):Unit = {
    val filtered = if (remainingSet.nonEmpty) matcherIds.filter(remainingSet.contains) else matcherIds
    s --= filtered
    s ++= runMatchers(input, filtered, filteredByContext, resultPool)
    //ctr.totolRuns
  }

  private def runMatcherSetInOrder(s:mutable.Set[MId], input:TInput, matcherIds:Set[MId], filteredByContext:Set[MId], resultPool:TMatchResultPool, remainingSet:Set[MId]):Unit = {
    val filtered = if (remainingSet.nonEmpty) remainingSet.intersect(matcherIds) else matcherIds
    s --= filtered
    s ++= runMatchers(input, filtered.toList, filteredByContext, resultPool)
    //ctr.totolRuns
  }

  private def oneRun(input:TInput, filteredByContext:Set[MId], resultPool:TMatchResultPool, remainingIds:Set[MId] = EmptyIdSet):Set[MId] =  {
    val s = mutable.Set[MId]()
    runMatcherList(s, input, independentMIds, filteredByContext, resultPool, remainingIds)
    //val indMIds = if (remainingIds.nonEmpty) independentMIds.filter(remainingIds.contains) else independentMIds
    //s ++= runMatcherSet(input, indMIds, filteredByContext, resultPool, ctr)
    //ctr.showTimeElapsed
    depCircles.foreach{ cir =>
      runMatcherSetInOrder(s, input, cir, filteredByContext, resultPool, remainingIds)
      //ctr.showTimeElapsed
      /*
      val cirMIds = if (remainingIds.nonEmpty) remainingIds.intersect(cir) else cir
      s --= cirMIds
      s ++= runMatcherSet(input, cirMIds.toList, filteredByContext, resultPool, ctr)
      */
    }
    runMatcherList(s, input, independentMIds2, filteredByContext, resultPool, remainingIds)
    /*
    val indMIds2 = if (remainingIds.nonEmpty) independentMIds2.filter(remainingIds.contains) else independentMIds2
    s --= indMIds2
    s ++= runMatcherSet(input, indMIds2, filteredByContext, resultPool, ctr)
    */
    //ctr.showTimeElapsed
    /*
    val othMIds = if (remainingIds.nonEmpty) otherMIds.filter(remainingIds.contains) else otherMIds
    s --= othMIds
    s ++ runMatcherSet(input, othMIds.toList, filteredByContext, resultPool, ctr)
    */
    runMatcherSetInOrder(s, input, otherMIds, filteredByContext, resultPool, remainingIds)
    //ctr.showTimeElapsed

    s.toSet
  }

  def m(input:TInput, subMatchCheckerLib: SubMatchCheckerLib, mergeOverlap:Boolean = true):TMatchResultPool = {
    //var s = Set[MId]() //matcherMap.keySet.toSet //note: use this to enforce order -- matcherMap.keySet.toList.sorted.toSet
    //s ++= orderedMIds
    //val ttt = s.filter(!stoppedByMatcherManager.contextChecker(input.context, _))
    //println(ttt)
    val filteredByContext = allMIds.filter(!stoppedByMatcherManager.contextChecker(input.context, _)).toSet
    //s --= filteredByContex

    //input.context.

    val resultPool = TMatchResultPool.create(input, subMatchCheckerLib)
    //import scala.util.control.Breaks._
    //breakable {

    var s:Set[MId] = oneRun(input, filteredByContext, resultPool)

    //println(s"equality test count: ${AtomSeqMatch.t}")
    while (s.nonEmpty) {
      s = oneRun(input, filteredByContext, resultPool, s)
      /*
      val mid = s.head
      ctr.incMCount(mid)
      s -= mid
      val matcher = matcherMap.get(mid).get
      val existing = resultPool.query(mid)
      val rawMatches = matcher.m(input, resultPool)

      //input.context.getFlag()
      var matches = rawMatches //removeDuplicates(rawMatches) // removeAlmostDuplicates(rawMatches)
      if (matches.nonEmpty) matches = stoppedByMatcherManager.filter(mid, matches, resultPool)
      //if (mergeOverlap) {
      //matches = removeAlmostDuplicates(matches) //mergeMatches(matches)
      //}
      //if (!setAlmostEqual(matches, existing)) {
      // needs optimization!!
      //val oldmc = AtomSeqMatch.t
      if (matches != existing) {
        val depMatcherIds = depMap.getMatcherIdsDepOn(mid)
        s ++= depMatcherIds
        s ++= stoppedByMatcherManager.updateMatches(mid, matches, resultPool)
        s --= filteredByContext
        resultPool.update(mid, matches)
      }
      //println(s"equality test count: ${AtomSeqMatch.t - oldmc}")
      //println(s"$mid: ${s.size}")
      */
    }

    //ctr.showTimeElapsed

    //println(TMatcher.profCount.toList.sortBy(_._2).mkString("----profCount:\n\t", "\n\t", "\n----profCount end-----"))
    //ctr.showStatsByCount(this)
    //ctr.showMatchStatsByDepth(resultPool)
    //println(input.atoms)
    //println(s"equality test count: ${AtomSeqMatch.t}")

    //}
    if (mergeOverlap) {
      //println("b4 mergeOverlap")
      _mergeOverlap(resultPool)
      //println("after mergeOverlap")
    }
    resultPool

  }

}

object MatcherManager {
  type ContextChecker = (TContext, MId) => Boolean

  val AlwaysPassChecker:ContextChecker = (ctx, mid) => true

  private def testEqual(m1:AtomSeqMatch, m2:AtomSeqMatch): Option[AtomSeqMatch] = if (m1 == m2) Option(m1) else None


  private def subMatchCovers(m:AtomSeqMatch, subm:AtomSeqMatch):Boolean = m.subMatches.exists(
    sm => {
      val coverMatch = testCover(sm, subm)
      coverMatch.nonEmpty && sm.range == coverMatch.get.range
    }
  )

  def testCover(m1:AtomSeqMatch, m2:AtomSeqMatch):Option[AtomSeqMatch] = {
    val rangeOverlap = m1.range.intersect(m2.range)

    var result:Option[AtomSeqMatch] = None
    if (m1.range == rangeOverlap) {
      // test if m2 covers m1
      if (m2.subMatches.size >= m1.subMatches.size) {
        if (!m1.subMatches.exists(!subMatchCovers(m2, _))) result = Option(m2)
      }
    }
    if (result.isEmpty && m2.range == rangeOverlap) {
      // test if m1 covers m2
      if (m1.subMatches.size >= m2.subMatches.size) {
        if (!m2.subMatches.exists(!subMatchCovers(m1, _))) result = Option(m1)
      }
    }
    result
  }

  //def create0:MatcherManager = new MatcherManager()
  def create():MatcherManager = new MatcherManager()

  val EmptyExcludeSet = Set[MId]()
  private[MatcherManager] def findIndependentMatchers(dep:mutable.Map[MId, Set[MId]], exclude:Set[MId] = EmptyExcludeSet): Iterable[MId] = {
    val filtered =
      if (exclude.isEmpty) dep.filter(_._2.isEmpty)
      else dep.filter(_._2.forall(exclude.contains))
    filtered.keySet
  }

  private[matchers] def shuffleBasedOnDependency(dep:Map[MId, Set[MId]], excludeSet:Set[MId] = EmptyExcludeSet):(List[MId], Set[MId]) = {
    var result = mutable.LinkedHashSet[MId]()
    val depMap = mutable.Map[MId,Set[MId]]()
    depMap ++= dep
    var indepMatchers = findIndependentMatchers(depMap, excludeSet)

    while (indepMatchers.nonEmpty) {
      result ++= indepMatchers
      depMap --= indepMatchers
      indepMatchers = findIndependentMatchers(depMap, excludeSet ++ result)
    }

    (result.toList, dep.keySet -- result)
  }
}
