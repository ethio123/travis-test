package org.ditw.mateng

import org.ditw.mateng.test.TestAtom.Atom
import org.scalatest.{FlatSpec, ShouldMatchers}
import org.scalatest.prop.TableDrivenPropertyChecks

/**
  * Created by jiaji on 2016-02-09.
  */
class PropMatcherLibTest extends FlatSpec with ShouldMatchers with TableDrivenPropertyChecks {
  import AtomPropMatcherLib._
  import TestHelper._

  val paramData = List(Array("data"))

  val testData = Table(
    ("matcherTmplId", "spawnParams", "atomToTest", "matchResult"),
    ("F", paramData, Atom("data", Map()), true),
    ("E", List(Array("Company")), Atom("Microsoft", Map("entityType" -> Set("Entity", "OrgEntity", "Company"))), true),
    ("E", List(Array("Company", "Organization")), Atom("FBI", Map("entityType" -> Set("Entity", "OrgEntity", "Organization"))), true),
    ("C", paramData, Atom("Data", Map()), false),
    ("C", List(Array("Data")), Atom("Data", Map()), true),
    ("L", paramData, Atom("txt", Map("lemma" -> Set("data"))), true),
    ("L", paramData, Atom("txt", Map("lemma" -> Set("data", "date"))), true),
    ("EA", List(Array("Organization"), Array("category"), Array("ThreatActor")), Atom("txt", Map("entityType" -> Set("Entity", "OrgEntity", "Organization"), "category" -> Set("ThreatActor"))), true),
    ("EAx", List(Array("Organization"), Array("category"), Array("ThreatActor")), Atom("txt", Map("entityType" -> Set("Entity", "OrgEntity", "Organization"), "category" -> Set("ThreatActor"))), false)
  )

  "t1" should "work" in {

    forAll(testData) {
      (matcherTmplId, spawnParams, atomToTest, matchResult) => {
        val pm = PropMatcherTmplMap.get(matcherTmplId).get.spawn(spawnParams, EmptyRegexDict)
        pm.get.check(atomToTest) shouldBe(matchResult)

      }
    }

  }

  val compositeTestData = Table(
    ("pmList", "atom2Test", "matchResult"),
    (
      List(("E", List(Array("Organization"))), ("EA", List(Array("Organization"), Array("category"), Array("ThreatActor")))),
      Atom("Lizard Squad", Map("entityType" -> Set("Entity", "OrgEntity", "Organization"), "category" -> Set("ThreatActor"))),
      true
      )
  )

  import TAtomMatcher._
  "testComposite" should "work" in {
    forAll(compositeTestData) {
      (pmList, atom2Test, matchResult) => {
        val l = pmList.map(p => PropMatcherTmplMap.get(p._1).get.spawn(p._2, EmptyRegexDict))
        l.exists(_.isFailure) shouldBe(false)
        val m = composite(l.map(_.get))
        m.check(atom2Test) shouldBe(matchResult)
      }
    }
  }

}
