package org.ditw.mateng.matchers

import org.ditw.mateng.ConfValueStringParser.Parsed
import StoppedByMatcherManager.StoppedByConfig
import SubMatchCheckerLib._
import org.ditw.mateng.test.TestAtom._
import org.ditw.mateng.{AtomPropMatcherLib, TInput, TestHelper}
import org.ditw.mateng.{TAtom, TInput}
import org.scalatest.{FlatSpec, ShouldMatchers}
import org.scalatest.prop.TableDrivenPropertyChecks

/**
  * Created by jiaji on 2016-02-18.
  */
class RepeatableMatcherTest extends FlatSpec with ShouldMatchers with TableDrivenPropertyChecks {
  import TMatcher._
  import org.ditw.mateng.AtomPropMatcherLib._
  import TSubMatchChecker._
  import org.ditw.mateng.TestHelper._

  import org.ditw.mateng.test.TestInput._

  val checkerId = "cid"
  val listConnectorId = "list-connector"
  implicit val checkerLib = new SubMatchCheckerLib(
    Map(checkerId -> Parsed("All", Array(Array(listConnectorId)))), List()
  )

  def createInput(atoms:Seq[TAtom]):TInput = {
    fromAtomArrayEng(IndexedSeq(
      textAtom("Now"),
      Anonymous
    ) ++ atoms)
  }

  val listConnector = matchersOR(
    listConnectorId,
    Seq(
      fromAtomMatcher(FExact(",")),
      fromAtomMatcher(FExact("and"))
    )
  )

  val OrgMId = "Org"
  val OrgMatcher = fromAtomMatcher(OrganizationEntity, EmptyCheckerIds, Option(OrgMId))(checkerLib)
  val CmpMId = "Company"
  val CmpMatcher = fromAtomMatcher(CompanyEntity, EmptyCheckerIds, Option(CmpMId))(checkerLib)
  val entityList = repeatMatcher(
    Seq(
      queryPoolMatcher(Set(OrgMId, CmpMId))
    ),
    Option("cmp-org-list"),
    List(checkerId)
  )

  val testData = Table(
    ("testAtoms", "matchRanges"),
    (
      Seq(FBI, _comma, Microsoft),
      Set(1 to 1, 2 to 2, 4 to 4, 2 to 4)
      ),
    (
      Seq(FBI, _and, Microsoft),
      Set(1 to 1, 2 to 2, 4 to 4, 2 to 4)
      ),
    (
      Seq(_comma, FBI, _and, Microsoft),
      Set(1 to 5, 1 to 1, 3 to 3, 5 to 5, 1 to 3, 3 to 5)
      ),
    (
      Seq(_comma, FBI, _and, _comma, Microsoft),
      Set(1 to 3, 1 to 1, 3 to 3, 6 to 6)
      ),
    (
      Seq(FBI, Microsoft),
      Set(1 to 1, 2 to 2, 3 to 3)
      )
  )

  "t1" should "work" in {
    val mm = MatcherManager.create
    mm.add(listConnector)
    mm.add(CmpMatcher)
    mm.add(OrgMatcher)
    mm.add(entityList)
    forAll(testData) {
      (testAtoms, resultRanges) => {
        val input = createInput(testAtoms)
        val resultPool = mm.m(input, checkerLib)
        val entities = resultPool.query(entityList.id.get)
        entities.size shouldBe(resultRanges.size)
        resultRanges.foreach(r => entities.exists(_.range == r) shouldBe true)
      }
    }
  }

  val _announce = fromAtomMatcher(FExact("announce"))
  val notOrgMId = Option("not-org")
  val notOrgMatcher = matchersOrderedAllPositive(Seq(queryPoolMatcher(Set(OrgMId, CmpMId)), _announce), List(ListNGramId), notOrgMId)(StaticSubMatchCheckerLib)
  val stoppedByMgr = new StoppedByMatcherManager(Map(
    OrgMId -> List(StoppedByConfig(notOrgMId.get, true)),
    CmpMId -> List(StoppedByConfig(notOrgMId.get, true))
  ))

  def createInput2(atoms:Seq[TAtom]):TInput = fromAtomArrayEng(IndexedSeq(
      textAtom("Now"),
      Anonymous
    ) ++ atoms ++ Seq(textAtom("announce")))


  val testData2 = Table(
    ("testAtoms", "matchRanges"),
    (
      Seq(FBI, _comma, Microsoft),
      Set(1 to 1, 2 to 2)
      ),
    (
      Seq(FBI, _and, Microsoft),
      Set(1 to 1, 2 to 2)
      ),
    (
      Seq(_comma, FBI, _and, Microsoft),
      Set(1 to 3, 1 to 1, 3 to 3)
      ),
    (
      Seq(_comma, FBI, _and, _comma, Microsoft),
      Set(1 to 3, 1 to 1, 3 to 3)
      ),
    (
      Seq(FBI, Microsoft),
      Set(1 to 1, 2 to 2)
      )
  )


  "t2" should "work" in {
    implicit val sbmm = stoppedByMgr
    val mm = MatcherManager.create
    mm.add(listConnector)
    mm.add(CmpMatcher)
    mm.add(OrgMatcher)
    mm.add(entityList)
    mm.add(notOrgMatcher)
    mm.setStoppedByManager(stoppedByMgr)
    forAll(testData2) {
      (testAtoms, resultRanges) => {
        val input = createInput2(testAtoms)
        val resultPool = mm.m(input, checkerLib)
        val entities = resultPool.query(entityList.id.get)
        entities.size shouldBe resultRanges.size
        resultRanges.foreach(r => entities.exists(_.range == r) shouldBe true)
      }
    }
  }

}
