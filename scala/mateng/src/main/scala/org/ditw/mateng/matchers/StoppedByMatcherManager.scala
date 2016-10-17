package org.ditw.mateng.matchers

import MatcherManager._
import org.ditw.mateng.AtomSeqMatch
import TMatcher.MId
import org.ditw.mateng.{AtomSeqMatch, TMatchResultPool}

import scala.collection.mutable.ListBuffer

/**
  * Created by jiaji on 2016-04-25.
  */
import StoppedByMatcherManager._
class StoppedByMatcherManager(stoppedByMapping:Iterable[(MId,List[StoppedByConfig])], val contextChecker: ContextChecker = AlwaysPassChecker) {
  private val _stoppedByMapping:Map[MId, List[StoppedByConfig]] = stoppedByMapping.groupBy(_._1).map(p => p._1 -> p._2.flatMap(_._2).toList)
  private def _getStopByLists(mid:MId):List[StoppedByConfig] =
    if (_stoppedByMapping.contains(mid)) _stoppedByMapping.get(mid).get else EmptyStoppedByList
  //def getStopByLists(listName:String):java.util.List[StoppedBy] = _getStopByLists(listName).asJava
  private val _reverseStopByMap:Map[MId,(Boolean, List[MId])] = {
    import scala.collection.mutable.{Map => MutMap}
    val mm:MutMap[String,(Boolean,ListBuffer[String])] = MutMap()
    _stoppedByMapping.foreach(
      (kvp) => {
        val listName:String = kvp._1
        val stoppedByLists = kvp._2
        stoppedByLists.foreach(sl => {
          if (!mm.contains(sl.mid)) mm.put(sl.mid, sl.overlap -> ListBuffer())
          mm(sl.mid)._2 += listName
        })
      }
    )
    mm.map(kvp => (kvp._1, kvp._2._1 -> kvp._2._2.toList.sorted)).toMap
  }
  def getListsStopBy(mid: MId):Option[(Boolean, List[MId])] = _reverseStopByMap.get(mid)
  //def getListsStopBy(mid: MId):List[MId] = _getListsStopBy(mid)


  import scala.util.control.Breaks._
  import scala.collection.mutable
  def filter(mid:MId, currMatches:Set[AtomSeqMatch], resultPool:TMatchResultPool):Set[AtomSeqMatch] = {
    val stoppedByConfigs = _getStopByLists(mid)
    if (stoppedByConfigs.isEmpty) currMatches
    else {
      var toRemove:mutable.Set[AtomSeqMatch] = mutable.Set()
      //r ++= currMatches
      breakable {
        stoppedByConfigs.foreach(
          cfg => {
            val ms = resultPool.query(cfg.mid)
            if (ms.nonEmpty) {
              // remove all, !overlap means as long as there's stop-by-match in the sentence, the matches will be filtered
              if (!cfg.overlap) {
                toRemove ++= currMatches
                resultPool.handleStoppedMatch(cfg.mid, toRemove.toSet)
                break
              }
              else {
                val toStop = currMatches.filter(cm => ms.exists(checkMatchIntersect(cm, _)))
                toRemove ++= toStop
                if (toStop.nonEmpty) resultPool.handleStoppedMatch(cfg.mid, toStop.toSet)
              }
            }
          }
        )
      }
      currMatches -- toRemove
    }
  }

  def updateMatches(mid:MId, currMatches:Set[AtomSeqMatch], resultPool:TMatchResultPool):List[MId] = {
    val stoppedList = getListsStopBy(mid)

    if (stoppedList.isEmpty) List()
    else {
      val overlap = stoppedList.get._1
      val lists = stoppedList.get._2
      if (overlap) {
        val matchers2Update = ListBuffer[MId]()
        lists.foreach(
          slid => {
            val matches = resultPool.query(slid)
            val isAffected = matches.exists(
              m => {
                currMatches.exists(_.range.intersect(m.range).nonEmpty)
              }
            )
            if (isAffected) matchers2Update += slid
          }
        )
        matchers2Update.toList
      }
      else {
        val nonEmptyLists = lists.filter(resultPool.query(_).nonEmpty)
        nonEmptyLists
      }
    }
  }
}

object StoppedByMatcherManager {
  val EmptyStoppedByList = List[StoppedByConfig]()
  val EmptyMIdList = EmptyStoppedByList.map(_.mid)

  case class StoppedByConfig(mid:MId, overlap:Boolean)

  def checkMatchIntersect(m1:AtomSeqMatch, m2:AtomSeqMatch):Boolean = m1.range.intersect(m2.range).nonEmpty

}
