package org.ditw.mateng.matchers

import org.ditw.mateng.ErrorHandling.{SubMatchCheckerErrorUnknownCheckerId, SubMatchCheckerLibErrorUnknownTemplate}
import org.ditw.mateng.matchers.TMatcher.{MId, TMatcherDep}
import org.ditw.mateng.{ConfValueStringParser, TInput}
import org.ditw.mateng.ConfValueStringParser.Parsed
import org.ditw.mateng.ConfValueStringParser.Parsed
import org.ditw.mateng.TInput
import org.ditw.mateng.matchers.TMatcher.TMatcherDep

import scala.collection.mutable.ListBuffer

/**
  * Created by jiaji on 2016-03-12.
  */
import SubMatchCheckerLib._
import TSubMatchChecker._

class SubMatchCheckerLib(staticDefs:Iterable[(String, Parsed)], dynDefs:Iterable[(String, TDynamicChecker)]) {
  /*
  def resolve(implicit paramTransform: String => String, gen:PartialFunction[Parsed, TSubMatchChecker]):SubMatchCheckerLib = {
    defs.foreach(
      d => {
        val id = d._1
        val defi = d._2
        val parsed = ConfValueStringParser.parse(defi)
        val transformedParams = parsed.paras.map(_.map(paramTransform))

        _map += id -> gen(Parsed(parsed.id, transformedParams))
      }
    )
    this
  }
  */
  val builtInCreator = new PartialFunction[Parsed, TSubMatchChecker] {
    def isDefinedAt(p:Parsed):Boolean = isStaticChecker(p)
    def apply(p:Parsed):TSubMatchChecker = p.id match {
      case "None" => matchesInBetween_None(p.paras(0))
      case "All" => matchesInBetween_All(p.paras(0))
      case NoCheckId => NoCheck
      case HeadCheckId => HeadChecker
      case TailCheckId => TailChecker
      case ListNGramId => ListNGramChecker
      case _ => throw SubMatchCheckerErrorUnknownCheckerId(p.id)
    }
  }


  val (staticCheckerMap:Map[String, TSubMatchChecker], dynamicCheckerMap:Map[String, TDynamicChecker]) = {
    val staticCheckers = ListBuffer[(String, TSubMatchChecker)]()
    val dynamicCheckers = ListBuffer[(String, TDynamicChecker)]()
    staticDefs.foreach(
      d => {
        val id = d._1
        val parsed = d._2
        //val parsed = ConfValueStringParser.parse(defi)
        //val transformedParams = parsed.paras.map(_.map(paramTransform))

        if (builtInCreator.isDefinedAt(parsed)) {
          staticCheckers += id -> builtInCreator(parsed)
        }
        else if(parsed.id == "And") {
          dynamicCheckers += id -> And(parsed.paras(0), this)
        }
        else {
          throw SubMatchCheckerLibErrorUnknownTemplate(parsed.id)
        }
      }
    )
    (BuildInCheckers ++ staticCheckers.toMap, (dynDefs ++ dynamicCheckers).toMap)
  }

  def getDepMatcherIds(checkerId:String):Set[MId] =
    if (staticCheckerMap.contains(checkerId)) staticCheckerMap.get(checkerId).get.depMatcherIds else Set()

  //private val _map:mutable.Map[String, TSubMatchChecker] = mutable.Map()
  //def getStatic(id:String):TSubMatchChecker = staticCheckerMap.get(id).get

  def getCheckers(ids:Iterable[String]):(Iterable[TSubMatchChecker], Iterable[TDynamicChecker]) = {
    val staticIds = ids.filter(staticCheckerMap.contains)
    val dynamicIds = ids.filter(dynamicCheckerMap.contains)
    val staticMatchers = staticIds.map(staticCheckerMap.get(_).get)
    (staticMatchers, dynamicIds.map(dynamicCheckerMap.get(_).get))
  }

  def forInput(input:TInput):Map[String, TSubMatchChecker] = staticCheckerMap ++
    dynamicCheckerMap.map(p => (p._1, p._2.gen(input))).filter(p => (p._2.isDefined)).map(p => (p._1, p._2.get))
}

object SubMatchCheckerLib {
  /*
  def c(m:Map[String,TSubMatchChecker]):SubMatchCheckerLib = {
    val r = new SubMatchCheckerLib
    r ++= m
  }
  */
  trait TDynamicChecker extends TMatcherDep {
    override def depMatcherIds:Set[MId] = TMatcher.EmptyIdSet
    def gen(input:TInput):Option[TSubMatchChecker]
  }

  val NoCheckId = "NoCheck"
  val HeadCheckId = "HeadCheck"
  val TailCheckId = "TailCheck"
  val ListNGramId = "Lng"

  import TSubMatchChecker._
  val BuildInCheckers:Map[String, TSubMatchChecker] = Map(
    NoCheckId -> NoCheck,
    HeadCheckId -> HeadChecker,
    TailCheckId -> TailChecker,
    ListNGramId -> ListNGramChecker
  )

  private val _TemplateNames:Set[String] = Set(NoCheckId, HeadCheckId, TailCheckId, ListNGramId, "None", "All")
  def isStaticChecker(p:Parsed):Boolean = _TemplateNames.contains(p.id)
  def isStaticChecker(id:String):Boolean = _TemplateNames.contains(id)

  private[SubMatchCheckerLib] class _andCheckersFromLib(checkerIds:Iterable[String], checkerLib:SubMatchCheckerLib) extends TDynamicChecker {
    lazy private val checkers = checkerLib.getCheckers(checkerIds)
    lazy private val staticCheckers = checkers._1
    lazy private val dynamicCheckers = checkers._2
    lazy private val isStatic = dynamicCheckers.isEmpty
    def gen(input:TInput):Option[TSubMatchChecker] = {
      val checkers = if (isStatic) staticCheckers else staticCheckers ++ dynamicCheckers.flatMap(_.gen(input))
      if (checkers.size == 1) Option(checkers.toList(0)) else Option(new _andCheckers(checkers))
    }
    override def depMatcherIds:Set[MId] = getCheckerDepIds(staticCheckers) ++ getCheckerDepIds(dynamicCheckers)
  }
  private[matchers] def And(checkerIds:Iterable[String], checkerLib:SubMatchCheckerLib):TDynamicChecker = new _andCheckersFromLib(checkerIds, checkerLib)

}