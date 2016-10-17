package org.ditw.mateng

import org.ditw.mateng.matchers.TMatcher
import org.ditw.mateng.matchers.TMatcher

/**
  * Created by jiaji on 2016-08-15.
  */
sealed trait MatchFromMagnet {
  type Result = AtomSeqMatch
  def apply():Result
}

object MatchFromMagnet {
  implicit def fromRange(tp:(TMatchResultPool, Range, TMatcher)) = new MatchFromMagnet {
    def apply():Result = {
      val resultPool = tp._1
      val range = tp._2
      val matcher = tp._3
      new AtomSeqMatch(resultPool, range, matcher, AtomSeqMatch.EmptySubMatches)
    }
  }

  implicit def fromIndex(tp:(TMatchResultPool, Int, TMatcher)) = new MatchFromMagnet {
    def apply():Result = {
      val resultPool = tp._1
      val index = tp._2
      val matcher = tp._3
      new AtomSeqMatch(resultPool, index to index, matcher, AtomSeqMatch.EmptySubMatches)
    }
  }

  implicit def fromIndexWithSubMatches(tp:(TMatchResultPool, TMatcher, List[AtomSeqMatch])) = new MatchFromMagnet {
    def apply():Result = {
      val resultPool = tp._1
      val matcher = tp._2
      val subMatches = tp._3
      val start = subMatches.map(_.range.start).min
      val end = subMatches.map(_.range.end).max
      new AtomSeqMatch(resultPool, start to end, matcher, subMatches)
    }
  }

}
