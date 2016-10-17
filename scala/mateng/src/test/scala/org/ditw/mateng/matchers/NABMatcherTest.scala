package org.ditw.mateng.matchers

import org.ditw.mateng.AtomPropMatcherLib._
import org.ditw.mateng.TestHelper._
import SubMatchCheckerLib._
import TMatcher._
import org.ditw.mateng.TInput
import org.ditw.mateng.test.TestAtom._
import org.ditw.mateng.test.TestInput._
import org.scalatest._

/**
  * Created by jiaji on 2016-10-01.
  */
class NABMatcherTest extends FlatSpec with ShouldMatchers {

  val input:TInput = fromAtomArrayEng(IndexedSeq(
    textAtom("Now"),
    FBI,
    textAtom("and"),
    Microsoft,
    textAtom("announce")
  ))

  val input1:TInput = fromAtomArrayEng(IndexedSeq(
    textAtom("Now"),
    FBI,
    textAtom("and"),
    textAtom("x"),
    Microsoft,
    textAtom("announce")
  ))

  val input2:TInput = fromAtomArrayEng(IndexedSeq(
    textAtom("Now"),
    FBI,
    textAtom("x"),
    textAtom("and"),
    Microsoft,
    textAtom("announce")
  ))

  implicit val checkerLib = StaticSubMatchCheckerLib
  val orgCompanyMatcherId = "OrgCompany"
  val entityMatcher = fromAtomMatcher(E(EmptyRegexDict, Array("Company", "Organization")), EmptyCheckerIds, Option(orgCompanyMatcherId))
  val andMatcher = fromAtomMatcher(FExact("and"))

  "testNAB" should "work" in {
    val mm = MatcherManager.create

    val nabMatcherId = "nab"
    val nabMatcher = matchersNAB(andMatcher, entityMatcher, List(ListNGramId), false, Option(nabMatcherId))
    val anbMatcherId = "anb"
    val anbMatcher = matchersNAB(andMatcher, entityMatcher, List(ListNGramId), true, Option(anbMatcherId))
    mm.add(entityMatcher)
    mm.add(andMatcher)
    mm.add(nabMatcher)
    mm.add(anbMatcher)

    val resultPool = mm.m(input, StaticSubMatchCheckerLib)
    val orgCompanies = resultPool.query(orgCompanyMatcherId)
    orgCompanies.size shouldBe(2)

    val nabMatches = resultPool.query(nabMatcherId)
    nabMatches.size shouldBe(1)
    val anbMatches = resultPool.query(anbMatcherId)
    anbMatches.size shouldBe(1)
    val resultPool1 = mm.m(input1, StaticSubMatchCheckerLib)
    val nabMatches1 = resultPool1.query(nabMatcherId)
    nabMatches1.size shouldBe(2)
    val resultPool2 = mm.m(input2, StaticSubMatchCheckerLib)
    val anbMatches2 = resultPool2.query(anbMatcherId)
    anbMatches2.size shouldBe(2)


  }

  "testLookaround" should "work" in {
    val mm = MatcherManager.create

    val eabMatcherId = "eab"
    val eabMatcher = matchersLookaround(andMatcher, entityMatcher, List(ListNGramId), false, Option(eabMatcherId))
    val aebMatcherId = "aeb"
    val aebMatcher = matchersLookaround(andMatcher, entityMatcher, List(ListNGramId), true, Option(aebMatcherId))
    mm.add(entityMatcher)
    mm.add(andMatcher)
    mm.add(eabMatcher)
    mm.add(aebMatcher)

    val resultPool = mm.m(input, StaticSubMatchCheckerLib)
    val orgCompanies = resultPool.query(orgCompanyMatcherId)
    orgCompanies.size shouldBe(2)

    val eabMatches = resultPool.query(eabMatcherId)
    eabMatches.size shouldBe(1)
    val aebMatches = resultPool.query(aebMatcherId)
    aebMatches.size shouldBe(1)

    val resultPool1 = mm.m(input1, StaticSubMatchCheckerLib)
    val eabMatches1 = resultPool1.query(eabMatcherId)
    eabMatches1.size shouldBe(0)

    val resultPool2 = mm.m(input2, StaticSubMatchCheckerLib)
    val aebMatches2 = resultPool2.query(aebMatcherId)
    aebMatches2.size shouldBe(0)


  }
}
