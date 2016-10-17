package org.ditw.mateng

import org.ditw.mateng.test.TestAtom.Atom
import org.scalatest.{FlatSpec, ShouldMatchers}
import org.scalatest.prop.TableDrivenPropertyChecks

/**
  * Created by jiaji on 2016-02-06.
  */
class PropMatchTest extends FlatSpec with ShouldMatchers with TableDrivenPropertyChecks {
  //import TestHelper.Atom
  //import TestHelper.Atom._
  import TAtomMatcher._

  val propName1 = "p1"
  val normalTestAtom = Atom("txt", Map(propName1 -> Set("v1", "v2")))
  val normalTestData = Table(
    ("valueList", "caseSensi", "expectedCheckResult"),
    (Array("v1"), false, true),
    (Array("v1", "v3"), false, true),
    (Array("v2", "v3"), false, true),
    (Array("v3", "v4"), false, false),
    (Array("v1", "v2"), false, true),
    (Array("v1"), true, true),
    (Array("V1"), true, false),
    (Array("V1", "v3"), true, false),
    (Array("V2", "v3"), true, false),
    (Array("V1", "v2"), true, true),
    (Array("v1", "V2"), true, true),
    (Array("V1", "V2"), true, false),
    (Array("V3", "V4"), true, false)
  )

  "t1" should "work" in {

    forAll(normalTestData) {
      (valueList, caseSensi, expectedResult) => {
        val pm = propMatchExact(propName1, valueList, caseSensi)
        pm.check(normalTestAtom) shouldBe(expectedResult)

        val pmex = propMatchExact(propName1, valueList, caseSensi, true)
        pmex.check(normalTestAtom) shouldBe(!expectedResult)
      }
    }

  }

  val TwitterShortenedUrl = """https?\://t.co/\S+"""
  val PrivateIPAddress = """((10|192\.168|172\.(2\d|1[6-9]|3[01]))\.|127\.|0\.0\.0\.0).*"""
  val regexTestData = Table(
    ("regex", "attrValues", "expectedCheckResult"),
    (TwitterShortenedUrl, Set("http://t.co/asdk"), true),
    (TwitterShortenedUrl, Set("http://t.co/asdk", "http://fb.me/asdk"), true),
    (TwitterShortenedUrl, Set("http://fb.me/asdk"), false),
    (PrivateIPAddress, Set("0.0.0.0"), true),
    (PrivateIPAddress, Set("128.3.2.4"), false),
    (PrivateIPAddress, Set("192.168.1.23"), true)
  )

  "regexPropMatchTest" should "work" in {
    forAll(regexTestData) {
      (regex, attrValues, expectedResult) => {
        val pm = propMatchRegex(propName1, regex)
        val testAtom = Atom("txt", Map(propName1 -> attrValues))
        pm.check(testAtom) shouldBe(expectedResult)

        val pmex = propMatchRegex(propName1, regex, true)
        pmex.check(testAtom) shouldNot be(expectedResult)
      }
    }
  }
}
