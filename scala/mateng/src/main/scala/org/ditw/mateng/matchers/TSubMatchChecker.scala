package org.ditw.mateng.matchers

import org.ditw.mateng.ConfValueStringParser.Parsed
import org.ditw.mateng.ErrorHandling.{MatchCheckerErrorNoSubMatches, MatchCheckerInputDiffer}
import org.ditw.mateng.matchers.SubMatchCheckerLib.TDynamicChecker
import org.ditw.mateng.matchers.TMatcher.{MId, TMatcherDep}
import org.ditw.mateng._
import org.ditw.mateng.{AtomSeqMatch, TAtom, TAtomMatcher, TMatchResultPool}
import org.ditw.mateng.matchers.TMatcher.TMatcherDep

import scala.collection.mutable.ListBuffer

/**
  * Created by jiaji on 2016-02-10.
  */
trait TSubMatchChecker extends TMatcherDep {
  def check(matchSeq:Seq[AtomSeqMatch], resultPool:TMatchResultPool):Boolean
}

object TSubMatchChecker {
  import org.ditw.mateng.utils.HelperFuncs._
  private[TSubMatchChecker] val EmptyMatcherDepSet = Set[MId]()
  private[TSubMatchChecker] trait TCheckerNoMatcherDep extends TSubMatchChecker {
    def depMatcherIds:Set[MId] = EmptyMatcherDepSet
  }

  val NoCheck:TSubMatchChecker = new TCheckerNoMatcherDep {
    def check(matchSeq:Seq[AtomSeqMatch], resultPool:TMatchResultPool) = true
  }

  val TailChecker:TSubMatchChecker = new TCheckerNoMatcherDep {
    def check(matchSeq:Seq[AtomSeqMatch], resultPool:TMatchResultPool) = matchSeq.last.range.end == resultPool.input.atoms.size-1
  }

  val HeadChecker:TSubMatchChecker = new TCheckerNoMatcherDep {
    def check(matchSeq:Seq[AtomSeqMatch], resultPool:TMatchResultPool) = matchSeq.head.range.start == 0
  }

  private[TSubMatchChecker] class _ListNGramChecker extends TCheckerNoMatcherDep {
    def check(matchSeq:Seq[AtomSeqMatch], resultPool:TMatchResultPool):Boolean = {
      val inbtwRanges = calcInBetweenRanges(matchSeq.map(_.range))
      !inbtwRanges.exists(_.nonEmpty)
    }
  }

  val ListNGramChecker:TSubMatchChecker = new _ListNGramChecker

  private[TSubMatchChecker] abstract class _InBetweenAtomChecker(val matchers:Set[TAtomMatcher]) extends TCheckerNoMatcherDep {
    private def getInBetweenAtomSeqs(matchSeq:Seq[AtomSeqMatch]):List[Seq[TAtom]] = {
      val c = matchSeq.size
      if (c <= 0) throw MatchCheckerErrorNoSubMatches
      val inputAtoms = matchSeq.head.resultPool.input.atoms
      val ranges = calcInBetweenRanges(matchSeq.map(_.range))
      ranges.map(r => if (r.isEmpty) Seq() else r.get.map(inputAtoms(_)))
    }
    protected def checkOneAtomSeq(atoms:Seq[TAtom]):Boolean
    def check(matchSeq:Seq[AtomSeqMatch], resultPool:TMatchResultPool):Boolean = {
      val atomSeqs = getInBetweenAtomSeqs(matchSeq)
      !atomSeqs.exists(!checkOneAtomSeq(_))
    }
  }

  private[TSubMatchChecker] class _NotInBetweenAtomChecker(orMatchers:Set[TAtomMatcher]) extends _InBetweenAtomChecker(orMatchers) {
    protected def checkOneAtomSeq(atoms:Seq[TAtom]):Boolean = {
      !atoms.exists(atom => {
        orMatchers.exists(_.check(atom))
      })
    }
  }

  def noAtomsInBetween(atomMatchers:TAtomMatcher*):TSubMatchChecker = new _NotInBetweenAtomChecker(atomMatchers.toSet)

  private[TSubMatchChecker] abstract class _InBetweenNamedMatcherChecker(val matcherIds:Set[MId]) extends TSubMatchChecker {
    def depMatcherIds:Set[MId] = matcherIds
    protected def checkOneRange(range:Option[Range], matchesToCheck:Set[AtomSeqMatch]):Boolean
    def check(matchSeq:Seq[AtomSeqMatch], resultPool:TMatchResultPool):Boolean = {
      val ranges = calcInBetweenRanges(matchSeq.map(_.range))
      val allMatches:Set[AtomSeqMatch] = matcherIds.flatMap(resultPool.query)
      !ranges.exists(!checkOneRange(_, allMatches))
    }
  }

  private def testRangeIntersect(r1:Range, r2:Range):Boolean = {
    val it = r1.intersect(r2)
    it.nonEmpty
  }
  private[TSubMatchChecker] class _InBetweenNamedMatcherChecker_Not(matcherIds:Set[MId]) extends _InBetweenNamedMatcherChecker(matcherIds) {
    protected def checkOneRange(range:Option[Range], matchesToCheck:Set[AtomSeqMatch]):Boolean = range.isEmpty || !matchesToCheck.exists(m => testRangeIntersect(m.range, range.get))
  }

  private[TSubMatchChecker] class _InBetweenNamedMatcherChecker_All(matcherIds:Set[MId]) extends _InBetweenNamedMatcherChecker(matcherIds) {
    protected def checkOneRange(range:Option[Range], matchesToCheck:Set[AtomSeqMatch]):Boolean = range.nonEmpty && matchesToCheck.exists(_.range.containsSlice(range.get))
  }

  private[matchers] def matchesInBetween_None(matcherIds:Iterable[MId]):TSubMatchChecker = new _InBetweenNamedMatcherChecker_Not(matcherIds.toSet)
  private[matchers] def matchesInBetween_All(matcherIds:Iterable[MId]):TSubMatchChecker = new _InBetweenNamedMatcherChecker_All(matcherIds.toSet)

  private[matchers] def getCheckerDepIds(checkers:Iterable[TMatcherDep]):Set[MId] = checkers.foldLeft(Set[MId]())(_ ++ _.depMatcherIds)
  private[matchers] class _andCheckers(checkers:Iterable[TSubMatchChecker]) extends TSubMatchChecker {
    def depMatcherIds:Set[MId] = getCheckerDepIds(checkers)
    def check(matchSeq:Seq[AtomSeqMatch], resultPool:TMatchResultPool):Boolean = !checkers.exists(!_.check(matchSeq, resultPool))
  }

}