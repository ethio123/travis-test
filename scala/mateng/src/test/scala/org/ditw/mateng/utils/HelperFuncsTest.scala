package org.ditw.mateng.utils

import org.ditw.mateng.ErrorHandling.MatcherErrorNoConsecutiveNegMatchers
import org.ditw.mateng.matchers.TSubMatchChecker
import org.scalatest.{FlatSpec, ShouldMatchers}
import org.scalatest.prop.TableDrivenPropertyChecks


/**
  * Created by jiaji on 2016-02-11.
  */
class HelperFuncsTest extends FlatSpec with ShouldMatchers with TableDrivenPropertyChecks {
  import scala.collection.mutable.Seq
  import HelperFuncs._

  val negMatcherIndexTransformTestData = Table(
    ("inSeq", "outSeq"),
    (IndexedSeq(0), IndexedSeq(0)),
    (IndexedSeq(1), IndexedSeq(1)),
    (IndexedSeq(1,3), IndexedSeq(1,2))
  )

  val negMatcherIndexTransformTestDataInv = Table(
    ("inSeq"),
    (IndexedSeq(0,1)),
    (IndexedSeq(0,2,3))
  )

  "testNegMatcherIndexTransform" should "work" in {
    forAll(negMatcherIndexTransformTestData) {
      (inSeq, outSeq) => {
        val r = negMatcherIndexTransform(inSeq)
        r shouldBe outSeq
      }
    }
  }

  "testNegMatcherIndexTransformInv" should "work" in {
    forAll(negMatcherIndexTransformTestDataInv) {
      (inSeq) => {
        intercept[MatEngError] {
          negMatcherIndexTransform(inSeq)
        }
      }
    }
  }

  "t1" should "work" in {
    var seqSet = Seq[Set[String]](
      Set("a", "b"),
      Set("1", "2")
    )

    var r = combine[String](seqSet)
    r shouldBe(Set[Seq[String]](
      Seq("a", "1"),
      Seq("a", "2"),
      Seq("b", "1"),
      Seq("b", "2")
      ))

    seqSet = Seq[Set[String]](
      Set("a", "b"),
      Set("1", "2"),
      Set("I", "II")
    )
    r = combine[String](seqSet)
    r shouldBe(Set[Seq[String]](
      Seq("a", "1", "I"),
      Seq("a", "2", "I"),
      Seq("b", "1", "I"),
      Seq("b", "2", "I"),
      Seq("a", "1", "II"),
      Seq("a", "2", "II"),
      Seq("b", "1", "II"),
      Seq("b", "2", "II")
    ))

  }

  val calcInBetweenRangesTestData = Table(
    ("inputRanges", "outputRanges"),
    (Seq(1 to 3, 5 to 6, 7 to 7), List(4 to 4)),
    (Seq(1 to 3, 5 to 6, 8 to 8), List(4 to 4, 7 to 7)),
    (Seq(1 to 3, 4 to 6, 7 to 7), List()),
    (Seq(1 to 3, 4 to 7), List()),
    (Seq(1 to 3), List())
  )
  "calcInBetweenRangesTest" should "work" in {
    import TSubMatchChecker._

    forAll(calcInBetweenRangesTestData) {
      (inputRanges, outputRanges) => {
        val r = calcInBetweenRanges(inputRanges)
        r.flatten shouldBe(outputRanges)
      }
    }
  }

  val allGapsTestData = Table(
    ("allRange", "inputRanges", "outputRanges"),
    (0 to 8, Seq(1 to 3, 5 to 6, 7 to 7), List(0 to 0, 4 to 4, None, 8 to 8)),
    (0 to 8, Seq(1 to 3, 5 to 6, 8 to 8), List(0 to 0, 4 to 4, 7 to 7, None)),
    (0 to 8, Seq(1 to 3, 4 to 6, 7 to 7), List(0 to 0, None, None, 8 to 8)),
    (0 to 8, Seq(0 to 3, 4 to 7), List(None, None, 8 to 8)),
    (0 to 8, Seq(1 to 3), List(0 to 0, 4 to 8))
  )

  "allGapsTest" should "work" in {
    import TSubMatchChecker._

    forAll(allGapsTestData) {
      (allRange, inputRanges, outputRanges) => {
        val r = allGaps(allRange, inputRanges)
        r.indices.foreach(
          idx => if (r(idx).nonEmpty) r(idx).get shouldBe outputRanges(idx) else r(idx) shouldBe(outputRanges(idx))
        )
      }
    }
  }

  "rangeContainmentTest" should "work" in {
    val r1 = 1 to 3
    val r2 = 0 to 4

    r2.containsSlice(r1) shouldBe(true)
    r2.containsSlice(2 to 5) shouldBe(false)
    r2.containsSlice(5 to 6) shouldBe(false)
  }

  "emptyCheckerSetTest" should "work" in {
    val emptyCheckers:Iterable[String=>Boolean] = List()
    val twoCheckers:Iterable[String=>Boolean] = List(
      (s => s.length > 2),
      (s => s.startsWith("ok"))
    )

    val ts1 = "ok1"
    val ts2 = "o"
    val ts3 = "pk1"
    val ts4 = "ok"

    twoCheckers.forall(x => x(ts1)) shouldBe true
    twoCheckers.forall(x => x(ts2)) shouldBe false
    twoCheckers.forall(x => x(ts3)) shouldBe false
    twoCheckers.forall(x => x(ts4)) shouldBe false

    emptyCheckers.forall(x => x(ts1)) shouldBe true
    emptyCheckers.forall(x => x(ts2)) shouldBe true
    emptyCheckers.forall(x => x(ts3)) shouldBe true
    emptyCheckers.forall(x => x(ts4)) shouldBe true

  }
}
