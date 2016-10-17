package org.ditw.mateng.matchers

import TMatcher._
import org.scalatest.{FlatSpec, ShouldMatchers}

/**
  * Created by jiaji on 2016-02-11.
  */
class CompositeMatcherTest extends FlatSpec with ShouldMatchers {
  import org.ditw.mateng.AtomPropMatcherLib._
  import org.ditw.mateng.TestHelper._
  val idWord = "atom-matcher-f-word"
  val idPhrase = "atom-matcher-f-phrase"
  val idSentence = "atom-matcher-f-sentence"
  val idGroup = Option("atom-matchers")

  implicit val smlib = EmptySubMatchCheckerLib
  import SubMatchCheckerLib._
  "t1" should "work" in {
    val matchers = Array(
      fromAtomMatcher(FExact("word"), EmptyCheckerIds, Option(idWord)),
      fromAtomMatcher(FExact("phrase"), EmptyCheckerIds, Option(idPhrase))
    )
    val compMatcher = matchersOrderedAllPositive(matchers, EmptyCheckerIds, idGroup)

    val testInput = inputFrom("word and Word phrase , Phrase")
    var result = compMatcher.matchFrom(DummyResultPool(testInput), 0)
    result.size shouldBe(4)

    val noMatchers = Array(
      fromAtomMatcher(FExact("vord")),
      fromAtomMatcher(FExact("frase"))
    )
    val noCompMatcher = matchersOrderedAllPositive(noMatchers, EmptyCheckerIds, idGroup)
    result = noCompMatcher.matchFrom(DummyResultPool(testInput), 0)
    result.size shouldBe(0)

    val noMatchers2 = Array(
      fromAtomMatcher(FExact("word")),
      fromAtomMatcher(FExact("frase"))
    )
    val noCompMatcher2 = matchersOrderedAllPositive(noMatchers2, EmptyCheckerIds, idGroup)
    result = noCompMatcher2.matchFrom(DummyResultPool(testInput), 0)
    result.size shouldBe(0)
  }
}
