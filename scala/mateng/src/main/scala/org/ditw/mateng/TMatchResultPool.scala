package org.ditw.mateng

import org.ditw.mateng.ErrorHandling.SubMatchCheckerErrorUnknownCheckerId
import org.ditw.mateng.matchers.TMatcher.MId
import org.ditw.mateng.matchers.{MatcherOpCache, SubMatchCheckerLib, TMatcher, TSubMatchChecker}

import scala.collection.mutable.ListBuffer

/**
  * Created by jiaji on 2016-02-10.
  */
trait TMatchResultPool {
  def query(matcherId:MId):Set[AtomSeqMatch]
  def add(matcherId:MId, matches:Set[AtomSeqMatch]):Unit
  def clear(matcherId:MId):Unit
  def update(matcher:TMatcher, matches:Set[AtomSeqMatch]) = {
    val matcherId = matcher.id.get
    clear(matcherId)
    add(matcherId, matches)
    matcherOpCache.invalidate(matcher)
  }
  val input:TInput
  def getSubMatchChecker(id:String):TSubMatchChecker
  val subMatchCheckerLib:SubMatchCheckerLib
  //val subMatchCheckerMap:Map[String, TSubMatchChecker]
  def handleStoppedMatch(mid:MId, ms:Set[AtomSeqMatch]):Unit

  val matcherOpCache:MatcherOpCache

  def allMatcherIds:Set[MId]
  //def trace:String
}

object TMatchResultPool {
  import scala.collection.mutable

  val EmptyQueryResult = Set[AtomSeqMatch]()
  private[TMatchResultPool] class _ResultPool(val input:TInput, val subMatchCheckerLib:SubMatchCheckerLib) extends TMatchResultPool {
    val matcherOpCache = new MatcherOpCache

    private val _map = mutable.Map[MId,Set[AtomSeqMatch]]()
    def query(matcherId:MId):Set[AtomSeqMatch] = _map.getOrElse(matcherId, EmptyQueryResult)
    def add(matcherId:MId, matches:Set[AtomSeqMatch]):Unit = _map.put(matcherId, query(matcherId) ++ matches)
    def clear(matcherId:MId):Unit = _map.put(matcherId, EmptyQueryResult)
    val subMatchCheckerMap:Map[String, TSubMatchChecker] = subMatchCheckerLib.forInput(input)
    def getSubMatchChecker(id:String):TSubMatchChecker = subMatchCheckerMap.getOrElse(id, TSubMatchChecker.NoCheck)
      //if (subMatchCheckerMap.contains(id)) subMatchCheckerMap.get(id).get else throw SubMatchCheckerErrorUnknownCheckerId(id)

    private val _stoppedMap = mutable.Map[MId,Set[AtomSeqMatch]]()
    def handleStoppedMatch(mid:MId, ms:Set[AtomSeqMatch]):Unit = {
      val existing = _stoppedMap.getOrElse(mid, Set[AtomSeqMatch]())
      _stoppedMap.put(mid, existing ++ ms)
    }

    def allMatcherIds = _map.keySet.toSet
  }

  def create(input:TInput, subMatchCheckerLib:SubMatchCheckerLib):TMatchResultPool = new _ResultPool(input, subMatchCheckerLib)
}