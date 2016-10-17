package org.ditw.mateng.extracts

import org.ditw.mateng.test.TestAtom._
import org.ditw.mateng.{AtomPropMatcherLib, TInput, TMatchResultPool}
import org.ditw.mateng.matchers.TMatcher._
import org.ditw.mateng.{TInput, TMatchResultPool}
import org.ditw.mateng.matchers.SubMatchCheckerLib
import org.scalatest.{FlatSpec, ShouldMatchers}

/**
  * Created by jiaji on 2016-02-12.
  */
class ExtractTest extends FlatSpec with ShouldMatchers {
  import org.ditw.mateng.AtomPropMatcherLib._
  import org.ditw.mateng.TestHelper._
  import Extract._
  import org.ditw.mateng.test.TestAtom._
  import org.ditw.mateng.test.TestInput._

  import SubMatchCheckerLib._
  import org.ditw.mateng.AtomPropMatcherLib._

  implicit val subMatchCheckerLib = EmptySubMatchCheckerLib

  "t1" should "work" in {
    val entityMatcher = fromAtomMatcher(E(EmptyRegexDict, Array("Company", "Organization")))
    val andMatcher = fromAtomMatcher(F(EmptyRegexDict, "and"))
    val matcherId:MId = "mid"
    val matchers = matchersOrderedAllPositive(Seq(entityMatcher, andMatcher, entityMatcher), EmptyCheckerIds, Option(matcherId))

    val input:TInput = fromAtomArrayEng(IndexedSeq(
      textAtom("Now"),
      entityAtom("FBI", "Organization", "OrgEntity"),
      textAtom("and"),
      entityAtom("Microsoft", "Company", "OrgEntity")
    ))

    var extractDef = extractEntities("_entities", matcherId, "Company")

    var matches = matchers.m(DummyResultPool(input))
    var extracts = matches.flatMap(m => extractDef.process(m).instances)
    extracts.size shouldBe(1)

    extractDef = extractEntities("_entities", matcherId, "Company", "Organization")
    extracts = matches.flatMap(m => extractDef.process(m).instances)
    extracts.size shouldBe(2)


    extractDef = extractWhole("_entities", Option(matcherId))
    extracts = matches.flatMap(m => extractDef.process(m).instances)
    extracts.size shouldBe(1)

  }

  private val _extractDef =
    """
      |{
      | "extractDefSets": [
      |  {
      |   "domain": "CyberAttack",
      |   "relEntExtracts": {
      |     "name": "_related_entities",
      |     "atomMatcherDefs": [ "E(Country)" ]
      |   },
      |   "extractBlocks": [
      |     "_testblock_entities:_blocking_entities"
      |   ],
      |   "extractDefs": [
      |    {
      |     "extractName": "_entities",
      |     "matcherId": "CyberAttack.org-cmp",
      |     "atomMatcherDef": "E(Company)"
      |    },
      |    {
      |     "extractName": "_entities",
      |     "matcherId": "CyberAttack.org-list",
      |     "atomMatcherDef": "E(Company | Organization)"
      |    },
      |    {
      |     "extractName": "_testblock_entities",
      |     "matcherId": "CyberAttack.org-list",
      |     "atomMatcherDef": "E(Company | Organization)"
      |    },
      |    {
      |     "extractName": "_blocking_entities",
      |     "matcherId": "CyberAttack.org-list",
      |     "atomMatcherDef": "E(Company | Organization)"
      |    },
      |    {
      |     "extractName": "_indicator",
      |     "matcherId": "CyberAttack.org-list"
      |    }
      |   ]
      |  }
      | ]
      |}
    """.stripMargin

  "testBlocking" should "work" in {
    val input:TInput = fromAtomArrayEng(IndexedSeq(
      textAtom("Now"),
      entityAtom("Anonymous", "Organization", "OrgEntity"),
      textAtom("launch"),
      textAtom("attack"),
      textAtom("against"),
      entityAtom("FBI", "Organization", "OrgEntity"),
      textAtom("and"),
      entityAtom("Microsoft", "Company", "OrgEntity")
    ))

    val orgCompanyMatcher = fromAtomMatcher(E(EmptyRegexDict, Array("Company", "Organization")), EmptyCheckerIds, Option("CyberAttack.org-cmp"))
    val orgListMatcher = matchersOrderedAllPositive(Seq(orgCompanyMatcher, fromAtomMatcher(FExact("and")), orgCompanyMatcher), EmptyCheckerIds, Option("CyberAttack.org-list"))
    val resultPools = DummyResultPool(input)
    val matches = orgListMatcher.m(resultPools)

    val taggedExtractDefSet = extractDefs.getExtractDefSet("CyberAttack")
    matches.foreach(
      ma => {
        val ex = taggedExtractDefSet.run(ma, EmptyRelatedEntityCheckerIds)
        ex.mkString(" ")
      }
    )
  }

  "testRelEnt" should "work" in {
    val input:TInput = fromAtomArrayEng(IndexedSeq(
      entityAtom("US", "Country", "GeoEntity"),
      entityAtom("Anonymous", "Organization", "OrgEntity"),
      textAtom("launch"),
      textAtom("attack"),
      textAtom("against"),
      entityAtom("FBI", "Organization", "OrgEntity"),
      textAtom("and"),
      entityAtom("Microsoft", "Company", "OrgEntity"),
      textAtom("x"),
      entityAtom("US", "Country", "GeoEntity")
    ))

    val orgCompanyMatcher = fromAtomMatcher(E(EmptyRegexDict, Array("Company", "Organization")), EmptyCheckerIds, Option("CyberAttack.org-cmp"))
    val orgListMatcher = matchersOrderedAllPositive(Seq(orgCompanyMatcher, fromAtomMatcher(FExact("and")), orgCompanyMatcher), EmptyCheckerIds, Option("CyberAttack.org-list"))
    val resultPools = TMatchResultPool.create(input, StaticSubMatchCheckerLib)
    val matches = orgListMatcher.m(resultPools)

    val taggedExtractDefSet = extractDefs.getExtractDefSet("CyberAttack")
    matches.foreach(
      ma => {
        val ex = taggedExtractDefSet.run(ma, List("Lng"))
        ex.mkString(" ")
      }
    )
  }

//  {
//    "extractName": "_entities",
//    "matcherId": "mid3",
//    "atomMatcherDef": "EA(category, ThreatActor|Government)"
//  },

  val extractDefs = Extract.fromJson(_extractDef)

  "t2" should "work" in {

    val input:TInput = fromAtomArrayEng(IndexedSeq(
      textAtom("Now"),
      entityAtom("Anonymous", "Organization", "OrgEntity"),
      textAtom("launch"),
      textAtom("attack"),
      textAtom("against"),
      entityAtom("FBI", "Organization", "OrgEntity"),
      textAtom("and"),
      entityAtom("Microsoft", "Company", "OrgEntity")
    ))

    val orgCompanyMatcher = fromAtomMatcher(E(EmptyRegexDict, Array("Company", "Organization")), EmptyCheckerIds, Option("CyberAttack.org-cmp"))
    val orgListMatcher = matchersOrderedAllPositive(Seq(orgCompanyMatcher, fromAtomMatcher(FExact("and")), orgCompanyMatcher), EmptyCheckerIds, Option("CyberAttack.org-list"))
    val matches = orgListMatcher.m(DummyResultPool(input))

    val taggedExtractDefSet = extractDefs.getExtractDefSet("CyberAttack")
    matches.foreach(
      ma => {
        val ex = taggedExtractDefSet.run(ma, EmptyRelatedEntityCheckerIds)
        ex.mkString(" ")
      }
    )

  }
}
