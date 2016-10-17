package org.ditw.mateng

import org.ditw.mateng.test.TestAtom.Atom
import org.scalatest.{FlatSpec, ShouldMatchers}

/**
  * Created by jiaji on 2016-02-09.
  */
class TPropMatcherTmplTest extends FlatSpec with ShouldMatchers {
  import TPropMatcherTmpl._
  import TPropMatcherTmpl.PropMatchType._
  //import TestHelper.Atom
  "t1" should "work" in {
    val kp = KnownProp("id1", "p1", AtLeastOne)

    val pm = kp.spawn(List(Array("v1", "v2")), AtomPropMatcherLib.EmptyRegexDict)

    pm.isSuccess shouldBe(true)
    pm.get.check(Atom("txt", Map("p1" -> Set("v1", "v2")))) shouldBe(true)
  }


}
