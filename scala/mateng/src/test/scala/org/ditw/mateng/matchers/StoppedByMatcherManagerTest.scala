package org.ditw.mateng.matchers

import org.scalatest.{FlatSpec, ShouldMatchers}
import org.scalatest.prop.TableDrivenPropertyChecks

/**
  * Created by jiaji on 2016-04-25.
  */
class StoppedByMatcherManagerTest extends FlatSpec with ShouldMatchers with TableDrivenPropertyChecks {
  import StoppedByMatcherManager._

  val testData = Table(
    ("stoppedByMap", "reverseMap"),

    (Iterable("l1" -> List(StoppedByConfig("sl1", true), StoppedByConfig("sl2", true))), Map("sl1" -> (true -> List("l1")), "sl2" -> (true -> List("l1")))),
    (
      Iterable("l1" -> List(StoppedByConfig("sl1", true), StoppedByConfig("sl2", true)), ("l1" -> List(StoppedByConfig("sl3", false)))),
      Map("sl1" -> (true -> List("l1")), "sl2" -> (true -> List("l1")), "sl3" -> (false -> List("l1")))
    )
  )

  "t1" should "work" in {
    forAll(testData) {
      (stoppedByMap, reverseMap) => {
        val m = new StoppedByMatcherManager(stoppedByMap)
        reverseMap.foreach(
          p => {
            val k = p._1
            val expected = p._2
            m.getListsStopBy(k).get shouldBe expected
          }
        )
      }
    }
  }

}
