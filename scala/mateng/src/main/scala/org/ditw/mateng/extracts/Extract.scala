package org.ditw.mateng.extracts

import org.ditw.mateng.utils.MatEngError
import org.ditw.mateng.{AtomSeqMatch, ConfValueStringParser, TAtomMatcher, TMatchResultPool}
import org.ditw.mateng.matchers.{SubMatchCheckerLib, TMatcher, TSubMatchChecker}
import org.ditw.mateng.matchers.TMatcher.MId
import org.ditw.mateng.matchers.{SubMatchCheckerLib, TMatcher}
import org.ditw.mateng.{AtomSeqMatch, ConfValueStringParser, TAtomMatcher}
import org.json4s.JsonAST.{JArray, JField, JObject, JString}
import org.json4s.{CustomSerializer, NoTypeHints}
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization

import scala.collection.mutable.ListBuffer

/**
  * Created by jiaji on 2016-02-12.
  */
class Extract private (
  val name:String,
  val instances:List[AtomSeqMatch]
) {
  override def toString = "%s=[%s]".format(name, instances.mkString(", "))
}

object Extract {

  import org.ditw.mateng.AtomPropMatcherLib.{E, Er, TmplE, TmplEr}
  import org.ditw.mateng.ErrorHandling._
  abstract class ExtractDefBase(val extractName:String, protected val matcherId:Option[MId]) {
    protected def getInstances(input:AtomSeqMatch):List[AtomSeqMatch]
    def process(input:AtomSeqMatch):Extract = {
      val instances = getInstances(input)

      new Extract(extractName, instances)
    }
    def id = "%s.%s".format(matcherId, extractName)
    override def toString = "[%s -> %s]".format(matcherId, extractName)
    var domain:ExtractDomain = null
    def init(d:ExtractDomain):Unit = domain = d
    //def fullMatcherId = if (domain == null) matcherId else "%s.%s".format(domain.domain, matcherId)
  }

  private val EmptyRegexDict = Map[String,String]()

  private def AtomMatcherFromTmplId(tmplId:String, param:Array[String]):TAtomMatcher = {
    tmplId match {
      case TmplE => E(EmptyRegexDict, param)
      case TmplEr => Er(EmptyRegexDict, param)
      case _ => throw MatEngError.NotImplemented
    }
  }


  private[Extract] class _PropExtractDef(extractName:String, matcherId:MId, val matcherTmplId:String, val tmplParams:List[Array[String]]) extends ExtractDefBase(extractName, Option(matcherId)) {
    //private val (matcherTmplId, tmplParams) = AtomPropMatcherParser.parseTmplId(atomMatcherDef)
    def getInstances(input:AtomSeqMatch):List[AtomSeqMatch] = {

      val propMatcher = AtomMatcherFromTmplId(matcherTmplId, tmplParams.head)
      val qualifiedSubMatches:List[AtomSeqMatch] = input.allSubMatches(_.matcher.idEquals(matcherId))
      val singleAtomSubMatches = qualifiedSubMatches.flatMap(_.allSubMatches(_.range.length == 1))
      // only return sub-matches that doesn't have any sub-matches, i.e. leaf matches
      singleAtomSubMatches.filter(sm => sm.subMatches.isEmpty && propMatcher.check(sm.atoms(0)))
    }
  }

  private[Extract] class _EntityTypeExtractDef(extractName:String, matcherId:MId, val entityTypes:Array[String])
    extends _PropExtractDef(extractName, matcherId, TmplE, List(entityTypes))

  def extractEntities(extractName:String, matcherId:MId, entityTypes:String*):ExtractDefBase =
    new _EntityTypeExtractDef(extractName, matcherId, entityTypes.toArray)


  def extractByAtomMatcherDef(extractName:String, matcherId:MId, atomMatcherDef:String):ExtractDefBase = {
    val parsed = ConfValueStringParser.parse(atomMatcherDef)
    if (parsed.id == TmplE) new _EntityTypeExtractDef(extractName, matcherId, parsed.paras(0))
    else new _PropExtractDef(extractName, matcherId, parsed.id, parsed.paras.toList)
  }

  import TMatcher._

  private def idMatch(input:AtomSeqMatch, mid:MId) = input.matcher != null && input.matcher.id.nonEmpty && input.matcher.id.get == mid
  private[Extract] class _ExtractWholeDef(extractName:String, matcherId:Option[MId]) extends ExtractDefBase(extractName, matcherId) {
    /*
    def getInstances(input:AtomSeqMatch):Seq[AtomSeqMatch] = {
      input.allSubMatches(subMatch => subMatch.matcher != null && !subMatch.matcher.id.isEmpty && subMatch.matcher.id.get == matcherId)
    }
    */
    def getInstances(input:AtomSeqMatch):List[AtomSeqMatch] = {
      if (matcherId.isEmpty) List(input)
      else {
        val subMatches = input.allSubMatches(idMatch(_, matcherId.get))
        //if (idMatch(input, matcherId.get)) input :: subMatches
        subMatches
      }
    }
  }

  def extractWhole(extractName:String, matcherId:Option[String]):ExtractDefBase = new _ExtractWholeDef(extractName, matcherId)

  class RelatedEntityExtracts(val name:String, val atomMatcherDefs:List[String])

  val EmptyRelatedEntityCheckerIds = List()

  private val EmptyRelEntMatchers = List[TMatcher]()
  class ExtractDomain(val domain:String, val nonEventExtracts:Set[String], val relEntExtracts:Option[RelatedEntityExtracts], val extractBlocks:List[String], extractDefs:List[ExtractDefBase]) {

    private val _blockMap:Map[String,Set[String]] = extractBlocks.map{ eb =>
      val parts = eb.split("\\:")
      if (parts.length != 2) throw ExtractBlockErrorIllFormattedDef(eb)
      else {
        parts(0) -> parts(1).split("\\,").toSet
      }
    }.toMap
    def hasBlocker(exAttrName:String):Boolean = _blockMap.contains(exAttrName)
    private val _map:Map[String, List[ExtractDefBase]] = {
      extractDefs.foreach(_.init(this))
      extractDefs.groupBy(_.extractName).map(p => (p._1, p._2))
    }

    private def getRelEntMatchers(implicit subMatchCheckerLib:SubMatchCheckerLib):List[TMatcher] = {
      if (relEntExtracts.isEmpty) EmptyRelEntMatchers
      else {
        val exName = relEntExtracts.get.name
        relEntExtracts.get.atomMatcherDefs.map { d =>
          val parsed = ConfValueStringParser.parse(d)
          fromAtomMatcher(AtomMatcherFromTmplId(parsed.id, parsed.paras(0)))
        }
      }
    }

    private def extractRelEnt(m:AtomSeqMatch, subMatchCheckerIds:Iterable[String]):Option[Extract] = {
      if (relEntExtracts.isEmpty) None
      else {
        implicit val submatchCheckerLib = m.resultPool.subMatchCheckerLib
        val relEntMatchers = getRelEntMatchers
        val allMatches = relEntMatchers.map(_.m(m.resultPool)).reduce(_ ++ _)
        if (allMatches.nonEmpty) {
          val checked = if (subMatchCheckerIds.nonEmpty) {
            val smCheckers = subMatchCheckerIds.map(m.resultPool.getSubMatchChecker)
            allMatches.filter{ am =>
              val toCheck = Seq(am, m).sortBy(_.range.start)
              smCheckers.exists(_.check(toCheck, m.resultPool))
            }
          }
          else allMatches
          Option(new Extract(relEntExtracts.get.name, checked.toList))
        }
        else None
      }
    }



    //def getExtractDef(id:String):List[ExtractDefBase] = _map.get(id).get
    def run(m:AtomSeqMatch, relEntSubMatchCheckerIds:Iterable[String]):Map[String,Extract] = {
      val relEntExt = extractRelEnt(m, relEntSubMatchCheckerIds)
      val raw0 = _map.map(p => {
        val extracts = p._2.map(_.process(m))
        val mergedExtract = extracts.foldLeft(List[AtomSeqMatch]())(_ ++ _.instances).distinct
        p._1 -> new Extract(p._1, mergedExtract)
        //new Extract(p._1, mergedExtract)
      })
      val raw = if (relEntExt.nonEmpty) raw0 + (relEntExt.get.name -> relEntExt.get) else raw0
      val toCheckBlock = raw.filter(exp => _blockMap.contains(exp._1))
      val noNeedToCheck = raw.filter(ex => !_blockMap.contains(ex._1))
      val rawAll = raw.map(_._2)
      val checkResult:Iterable[Extract] = toCheckBlock.flatMap(tc => checkUnblockedExtract(tc._2, rawAll))
      /*
      toCheckBlock.foreach{ b =>
        val blockingExtractNames = _blockMap(b._1)
        val blockingExtractInsts = raw.filter(exp => blockingExtractNames.contains(exp._1)).flatMap(_._2.instances).toSet
        val unblockedInsts = b._2.instances.filter(!blockingExtractInsts.contains(_))
        if (unblockedInsts.nonEmpty) checkResult += new Extract(b._1, unblockedInsts)
      }
      */
      noNeedToCheck ++ checkResult.map(r => r.name -> r)
    }

    def checkUnblockedExtract(ex:Extract, all:Iterable[Extract]):Option[Extract] = {
      val blockingExtractNames = _blockMap(ex.name)
      val blockingExtractInsts = all.filter(ex => blockingExtractNames.contains(ex.name)).flatMap(_.instances).toSet
      // check ranges only since AtomSeqMatch equality checking checks matcher Id too
      val blockingInstRanges = blockingExtractInsts.map(_.range)
      val unblockedInsts = ex.instances.filter(i => !blockingInstRanges.contains(i.range))
      if (unblockedInsts.isEmpty) None
      else Option(new Extract(ex.name, unblockedInsts))
    }
  }

  class ExtractDefLib(val extractDefSets:List[ExtractDomain]) {
    private val _map:Map[String, ExtractDomain] = extractDefSets.map(eds => (eds.domain, eds)).toMap
    def getExtractDefSet(tag:String):ExtractDomain = _map.get(tag).get
  }

  class ExtractDefSerializer extends CustomSerializer[ExtractDefBase](
    format => (
      {
        case JObject(List(
          JField("extractName", JString(extractName)),
          JField("matcherId", JString(matcherId)),
          JField("atomMatcherDef", JString(atomMatcherDef))
          //JField("tmplParams", tmplParams)
        )) => {
          /*
          val param:Array[Array[String]] = tmplParams match {
            case JString(str) => Array(Array(str))
            case JArray(strLst) => strLst match {
              case strs:List[JString] => Array(strs.map(_.extract[String]).toArray)
              case y => throw NotImplemented
            }
            case x => throw NotImplemented
          }
          new _PropExtractDef(extractName, matcherId, matcherTmplId, param)
          */
          //throw NotImplemented
          extractByAtomMatcherDef(extractName, matcherId, atomMatcherDef)
        }
        case JObject(List(
          JField("extractName", JString(extractName)),
          JField("matcherId", JString(matcherId))
        )) => new _ExtractWholeDef(extractName, Option(matcherId))
        case JObject(List(JField("extractName", JString(extractName)))) => new _ExtractWholeDef(extractName, None)
        case err => throw ExtractErrorJSONFormat(err)
      },
      {
        case _ => throw MatEngError.NotImplemented
      }
      )
  )

  implicit val _formats = Serialization.formats(NoTypeHints) + new ExtractDefSerializer
  def fromJson(json:String):ExtractDefLib = parse(json).extract[ExtractDefLib]
}