package org.ditw.mateng

import org.ditw.mateng.test.TestAtom.Atom
import org.ditw.mateng.matchers.TMatcher
import org.scalatest.{FlatSpec, ShouldMatchers}


/**
  * Created by jiaji on 2016-02-09.
  */
class FromAtomMatcherTest extends FlatSpec with ShouldMatchers {
  import org.ditw.mateng.matchers.TMatcher._
  import TestHelper._
  import AtomPropMatcherLib._
  import org.ditw.mateng.test.TestInput._
  "t1" should "work" in {
    implicit val subMatchCheckerLib = EmptySubMatchCheckerLib
    val seqMatcher = fromAtomMatcher(F(EmptyRegexDict, "word"))
    val input = fromAtomArrayEng(IndexedSeq(
      Atom("Word", Map()),
      Atom("to", Map()),
      Atom("word", Map())
    ))

    val matches = seqMatcher.m(
      DummyResultPool(input)
    )

    matches.size shouldBe(2)

  }

}
