package org.ditw.mateng.matchers

import org.ditw.mateng.ConfValueStringParser.Parsed
import org.ditw.mateng.TestHelper._
import SubMatchCheckerLib._
import org.ditw.mateng.test.TestAtom._
import org.ditw.mateng.TInput
import org.ditw.mateng.test.{TestAtom, TestInput}
import org.ditw.mateng.TInput
import org.scalatest.{FlatSpec, ShouldMatchers}

/**
  * Created by jiaji on 2016-02-29.
  */
class MatcherTmplTest extends FlatSpec with ShouldMatchers {
  "t1" should "work" in {
    import MatcherTmpl._
    import TSubMatchChecker._
    import TMatcher._
    import org.ditw.mateng.AtomPropMatcherLib._
    import org.ditw.mateng.test.TestInput._
    val lngChecker = "lngChecker"
    implicit val checkerLib = new SubMatchCheckerLib(Map(lngChecker -> Parsed("Lng", Array())), List()) //SubMatchCheckerLib.c(Map(lngChecker -> ListNGramChecker))
    val tmplId = "id1"
    val tlib = new MatcherTmplLib(
      List(new MatcherTmplDef(tmplId, "MTL_Repetition", lngChecker)), List()
    )
    val matcherId = "orgCompanyMatcher"
    val matcher = fromAtomMatcher(E(EmptyRegexDict, Array("Organization", "Company")), EmptyCheckerIds, Option(matcherId))
    val mm = MatcherManager.create
    mm.add(matcher)

    val id2 = "orgCompanySeq"
    val m2 = tlib.spawn(tmplId, Array(Array(matcherId)), Option(id2))
    mm.add(m2)

    val input:TInput = fromAtomArrayEng(IndexedSeq(
      textAtom("Now"),
      entityAtom("FBI", "Organization", "OrgEntity"),
      entityAtom("Microsoft", "Company", "OrgEntity"),
      textAtom("and"),
      entityAtom("IBM", "Company", "OrgEntity")
    ))

    val rp = mm.m(input, EmptySubMatchCheckerLib)
    val r = rp.query(id2)
    r.size shouldBe >(0)
  }
}
