package org.ditw.mateng.matchers

import org.ditw.mateng.AtomPropMatcherLib._
import org.ditw.mateng.TestHelper._
import MatcherManager.ContextChecker
import TMatcher._
import org.ditw.mateng.TInput
import org.scalatest.{FlatSpec, ShouldMatchers}

/**
  * Created by jiaji on 2016-02-14.
  */
class MatcherManagerTest extends FlatSpec with ShouldMatchers {
  import org.ditw.mateng.test.TestAtom._
  import org.ditw.mateng.test.TestInput._
  val input:TInput = fromAtomArrayEng(IndexedSeq(
    textAtom("Now"),
    FBI,
    textAtom("and"),
    Microsoft
  ))


  implicit val smclib = EmptySubMatchCheckerLib

  import SubMatchCheckerLib._
  val orgCompanyMatcherId = "org_company"
  val entityMatcher = fromAtomMatcher(E(EmptyRegexDict, Array("Company", "Organization")), EmptyCheckerIds, Option(orgCompanyMatcherId))
  val queryOrgCmpMatcher = queryPoolMatcher(orgCompanyMatcherId)
  val andMatcher = fromAtomMatcher(FExact("and"))
  val announceMatcher = fromAtomMatcher(FExact("announce"))
  val matcherId = Option("org_company_list")
  val matchers = matchersOrderedAllPositive(Seq(queryOrgCmpMatcher, andMatcher, queryOrgCmpMatcher), EmptyCheckerIds, matcherId)
  val notOrgMatcherId = Option("not_org")
  val notOrgMatcher = matchersOrderedAllPositive(Seq(entityMatcher, announceMatcher), List("Lng"), notOrgMatcherId)(StaticSubMatchCheckerLib)


  "t1" should "work" in {
    val mm = MatcherManager.create

    mm.add(entityMatcher)
    mm.add(matchers)

    val resultPool = mm.m(input, EmptySubMatchCheckerLib)
    val orgCompany = resultPool.query(orgCompanyMatcherId)
    orgCompany.size shouldBe(2)

    val orgCompanyList = resultPool.query(matcherId.get)
    orgCompanyList.size shouldBe(1)
  }


  "testQ2" should "work" in {
    val mm = MatcherManager.create

    val m1id = Option("m1id")
    val m2id = Option("m2id")
    val m1 = matchersOrderedAllPositive(Seq(queryOrgCmpMatcher, andMatcher, queryOrgCmpMatcher), EmptyCheckerIds, m1id)
    val m2 = fromAtomMatcher(FExact("and"), EmptyCheckerIds, m2id)
    val mq2id = Option("mq2id")
    val mq2 = queryAnd(Set(m1id.get), Set(m2id.get), EmptyCheckerIds, mq2id)

    mm.add(entityMatcher)
    mm.add(m1)
    mm.add(m2)
    mm.add(mq2)

    val resultPool = mm.m(input, EmptySubMatchCheckerLib)
    val q2m = resultPool.query(mq2id.get)

    q2m.size shouldBe(1)
  }


  //@Test(enabled = false)
  def testRemoveAlmostDup = {
    val mm = MatcherManager.create

    val m1id = Option("m1id")
    val m11id = Option("m11id")
    val m12id = Option("m12id")
    val m2id = Option("m2id")
    val m11 = matchersOrderedAllPositive(Seq(queryOrgCmpMatcher, andMatcher, queryOrgCmpMatcher), EmptyCheckerIds, m11id)
    val m12 = matchersOrderedAllPositive(Seq(queryOrgCmpMatcher, matchersOrderedAllPositive(Seq(andMatcher, queryOrgCmpMatcher))), EmptyCheckerIds, m12id)
    val m1 = matchersOR(m1id, Seq(queryPoolMatcher(m11id.get), queryPoolMatcher(m12id.get)))
    val m2 = fromAtomMatcher(FExact("and"), EmptyCheckerIds, m2id)
    val mq2id = Option("mq2id")
    val mq2 = queryAnd(Set(m11id.get), Set(m2id.get), EmptyCheckerIds, mq2id)

    mm.add(entityMatcher)
    mm.add(m1)
    mm.add(m11)
    mm.add(m12)
    mm.add(m2)
    mm.add(mq2)

    val resultPool = mm.m(input, EmptySubMatchCheckerLib)
    val q2m = resultPool.query(m1id.get)

    q2m.size shouldBe(1)
  }


  import StoppedByMatcherManager._
  val stoppedByMgr = new StoppedByMatcherManager(Map(orgCompanyMatcherId -> List(StoppedByConfig(notOrgMatcherId.get, true))))

  val input1:TInput = fromAtomArrayEng(IndexedSeq(
    textAtom("Now"),
    FBI,
    textAtom("and"),
    Microsoft,
    textAtom("announce")
  ))

  "testWithStoppedByMatchers" should "work" in {
    implicit val sbmm = stoppedByMgr
    val mm = MatcherManager.create

    mm.add(entityMatcher)
    mm.add(matchers)
    mm.add(notOrgMatcher)
    mm.setStoppedByManager(stoppedByMgr)

    val resultPool = mm.m(input1, EmptySubMatchCheckerLib)
    val orgCompany = resultPool.query(orgCompanyMatcherId)
    orgCompany.size shouldBe(1)
    val notOrgCompany = resultPool.query(notOrgMatcherId.get)
    notOrgCompany.size shouldBe(1)

  }

  val contextChecker:ContextChecker = (ctxt, mid) => {
    mid != notOrgMatcherId.get
  }
  val stoppedByMgr1 = new StoppedByMatcherManager(Map(orgCompanyMatcherId -> List(StoppedByConfig(notOrgMatcherId.get, true))), contextChecker)
  val input2:TInput = fromAtomArrayEng(IndexedSeq(
    textAtom("Now"),
    FBI,
    textAtom("and"),
    Microsoft,
    textAtom("announce")
  ))


  "testWithContextCheck" should "work" in {
    implicit val sbmm = stoppedByMgr1
    val mm = MatcherManager.create

    mm.add(entityMatcher)
    mm.add(matchers)
    mm.add(notOrgMatcher)
    mm.setStoppedByManager(stoppedByMgr1)

    val resultPool = mm.m(input2, EmptySubMatchCheckerLib)
    val orgCompany = resultPool.query(orgCompanyMatcherId)
    orgCompany.size shouldBe(2)
    val notOrgCompany = resultPool.query(notOrgMatcherId.get)
    notOrgCompany.size shouldBe(0)

  }
}
