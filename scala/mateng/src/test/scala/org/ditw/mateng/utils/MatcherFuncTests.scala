package org.ditw.mateng.utils

import org.ditw.mateng.matchers.MatcherTmpl.MatcherFuncDef
import org.scalatest._
import org.scalatest.prop.TableDrivenPropertyChecks

/**
  * Created by jiaji on 2016-10-10.
  */
class MatcherFuncTests extends FlatSpec with ShouldMatchers with TableDrivenPropertyChecks {

  val mfdef1 = "mf1($1|$2)"
  val mfdef2 = "mf1($2|$1)"
  val mfdef3 = "mf1($2|const|$1)"
  val td = Table(
    ("defi", "input", "result"),
    (mfdef1, Array(Array("a"), Array("b")), Array[Array[String]](Array("a"), Array("b"))),
    //(mfdef1, Array("a", "b/c"), Array[Array[String]](Array("a"), Array("b", "c"))),
    //(mfdef2, Array("a/b", "c/d"), Array[Array[String]](Array("c", "d"), Array("a", "b"))),
    //(mfdef1, Array("9\\/11/8\\/14", "9/11"), Array[Array[String]](Array("9/11", "8/14"), Array("9", "11"))),
    //(mfdef1, Array("9\\/11 / 8\\/14", "9 / 11"), Array[Array[String]](Array("9/11", "8/14"), Array("9", "11"))),
    (mfdef3, Array(Array("a", "b"), Array("c","d")), Array[Array[String]](Array("c", "d"), Array("const"), Array("a", "b"))),
    (mfdef2, Array(Array("a"), Array("b")), Array[Array[String]](Array("b"), Array("a")))
  )
  "t1" should "work" in {
    forAll(td) { (defi, input, result) =>
      val mf = new MatcherFuncDef("id1", defi)
      val params = mf.getParams(input)
      params shouldBe result
    }

  }
}
