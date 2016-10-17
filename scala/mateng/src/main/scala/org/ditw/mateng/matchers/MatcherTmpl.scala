package org.ditw.mateng.matchers

import org.ditw.mateng.matchers.SubMatchCheckerLib._
import org.ditw.mateng.matchers.TMatcher._
import org.ditw.mateng.utils.MatEngError
import org.ditw.mateng.{AtomPropMatcherLib, ConfValueStringParser}
import org.ditw.mateng.matchers.TSubMatchChecker._
import org.ditw.mateng.ConfValueStringParser

/**
  * Created by jiaji on 2016-02-29.
  */
import org.ditw.mateng.utils.MatEngError._
object MatcherTmpl {
  //import scala.collection.mutable
  import TMatcher._
  import org.ditw.mateng.ErrorHandling._
  import scala.collection.mutable

  class MatcherTmplDef(val id:String, val t:String, val checkerIds:String) {
    def getCheckerIds = checkerIds.split("\\|")
  }

  private def paramIndexMap(param: Array[String]):Map[Int,Int] = {
    val r = mutable.Map[Int,Int]()

    param.indices.foreach{ x =>
      val paramIndex = ConfValueStringParser.parseParam(param(x))
      if (paramIndex.nonEmpty) r += paramIndex.get -> x
    }

    if (r.exists(p => p._1 > r.size || p._1 <= 0)) throw Todo("exception: param index out of range [1..size]")
    r.toMap
  }

  private def replaceParams(in: Array[Array[String]], paramIndexMap:Map[Int,Int], defiParams: Array[String]) = {
    /*
    val result = param.map(_.clone)
    in.indices.foreach(
      idx => {
        val coord = paramIndexMap.get(idx+1).get
        result(coord._1)(coord._2) = in(idx)
      }
    )
    result
    */
    // each parameter in 'in' will further be expanded into an array of parameters, separated by '/'
    val result = defiParams.map(p => Array(p))
    in.indices.foreach{ idx =>
      //val params = in(idx).split(ConfValueStringParser.SecondSeparatorStr).map(_.trim.replace(ConfValueStringParser.EscapedSecondSeparator, ConfValueStringParser.SecondSeparatorChar))
      val resultIndex = paramIndexMap(idx+1)
      result(resultIndex) = in(idx)
    }
    result
  }

  class MatcherFuncDef(val id:String, val defi:String) {
    // note: all matcher functions do not use 2-d arguments, i.e. the definition looks like MatchFuncName(a1|a2)
    //   when the functions are applied, 2-d parameters are allowed by using '/' as parameter separator, e.g. MatchFuncName(p11/p12|p2)
    private lazy val _parsed = ConfValueStringParser.parse(defi)
    private lazy val _params = _parsed.paras.map(_(0))

    private lazy val _paramIndexMap:Map[Int,Int] = paramIndexMap(_params)
    def paramCount = _paramIndexMap.size
    def getTemplateId = _parsed.id

    def getParams(in:Array[Array[String]]):Array[Array[String]] = {
      if (in.length != paramCount) {
        throw Todo("exception: param count differ")
      }
      replaceParams(in, _paramIndexMap, _params)
    }
  }

  type DomainIdFinder = MId => MId
  def getDomainId(domainIdFinder: DomainIdFinder, id:MId) = domainIdFinder(id)
  def getDomainIds(domainIdFinder: DomainIdFinder, ids:Array[MId]):Array[MId] = ids.map(domainIdFinder)

  private def paramCountError(expCount:Int):String = s"Exact $expCount params required"
  private def paramMinCountError(min:Int):String = s"At least $min params required"
  private def SeqMatchers(funcId:String, params:Array[Array[String]], checkerIds:Array[String], id:Option[MId])
                         (implicit matchCheckerLib:SubMatchCheckerLib):TMatcher = {
    if (params.length < 2) throw MatcherTemplateErrorSpawnMatcher(funcId, paramMinCountError(2))
    val seqMatchers = params.map { p => queryPoolMatcher(p.toSet) }
    matchersOrderedAllPositive(seqMatchers, checkerIds, id)
  }

  val NABParamCount = 2
  private def NABMatchers(funcId:String, rawParams:Array[Array[String]], checkerIds:Array[String], flipOrder:Boolean, id:Option[MId])
                         (implicit matchCheckerLib:SubMatchCheckerLib):TMatcher = {
    if (rawParams.exists(_.length != 1)) throw MatcherTemplateErrorSpawnMatcher(funcId, s"cannot use OR('/') operator in NAB matchers")
    val params = rawParams.map(_(0)) // take only one
    if (params.length != NABParamCount) throw MatcherTemplateErrorSpawnMatcher(funcId, s"exact 2 params required")
    val (notMatcherId, matcherId) = if (!flipOrder) (params(0), params(1)) else (params(1), params(0))
    matchersNAB(queryPoolMatcher(notMatcherId), queryPoolMatcher(matcherId), checkerIds, flipOrder, id)
  }

  private def NABMatchers(matcher1:TMatcher, matcher2:TMatcher, checkerIds:Array[String], flipOrder:Boolean, id:Option[MId])
                         (implicit matchCheckerLib:SubMatchCheckerLib):TMatcher = {
    val (notMatcher, matcher) = if (!flipOrder) (matcher1, matcher2) else (matcher2, matcher1)
    matchersNAB(notMatcher, matcher, checkerIds, flipOrder, id)
  }

  sealed class MatcherTmplLib(val tmplDefs:List[MatcherTmplDef], val funcDefs:List[MatcherFuncDef]) {
    private val _templateMap = tmplDefs.map(td => (td.id, td)).toMap
    private val _funcMap = funcDefs.map(fd => (fd.id, fd)).toMap
    def funcDef2TmplDef(funcDefId:String):MatcherTmplDef = {
      val fd = _funcMap.get(funcDefId).get
      _templateMap.get(fd.getTemplateId).get
    }
    private def getTemplate(funcOrTemplId:String):MatcherTmplDef = {
      if (_funcMap.contains(funcOrTemplId)) funcDef2TmplDef(funcOrTemplId)
      else _templateMap.get(funcOrTemplId).get
    }

    import org.ditw.mateng.AtomPropMatcherLib._
    def spawn(tmplFuncId:String, _param:Array[Array[String]], id:Option[MId] = None, domainFinder:Option[DomainIdFinder] = None)
             (implicit matchCheckerLib:SubMatchCheckerLib):TMatcher = {
      val templateDef = getTemplate(tmplFuncId)
      val param = if (_funcMap.contains(tmplFuncId)) _funcMap.get(tmplFuncId).get.getParams(_param) else _param
      val domainParam = if (domainFinder.nonEmpty) param.map(getDomainIds(domainFinder.get, _)) else param
      val (t, checkerIds) = (templateDef.t, templateDef.getCheckerIds)
      t match {
        case "MTL_QueryFromPool" => {
          val queryParams = domainParam(0).toSet
          if (checkerIds.length < 1) queryPoolMatcher(queryParams)
          else {
            //if (checkerIds.length == 1) queryPoolMatcher(domainParam.toSet, checkerIds(0))
            //else matchersOR(id, checkerIds.map(queryPoolMatcher(domainParam.toSet, _)))
            queryPoolMatcher(queryParams, checkerIds)
          }
        }
        case "MTL_QueriesFromPool" => {
          if (domainParam.length != 2) throw Todo("MTL_QueriesFromPool only supports 2 params for now")
          val s1 = domainParam(0).toSet
          val s2 = domainParam(1).toSet
          queryAnd(s1, s2, checkerIds, id)
          //if (checkerIds.length < 1) queryAnd(s1, s2, NoCheckId, id)
          //else if (checkerIds.length == 1) queryAnd(s1, s2, checkerIds(0), id)
          //else matchersOR(id, checkerIds.map(queryAnd(s1, s2, _)))
        }
        case "MTL_Repetition" => {
          repeatMatcher(domainParam.map(dp => queryPoolMatcher(dp.toSet)), id, checkerIds)
          //if (checkerIds.length == 1) repeatMatcher(Seq(queryPoolMatcher(domainParam.toSet)), id, checkerIds(0))
          //else matchersOR(id, checkerIds.map(repeatMatcher(Seq(queryPoolMatcher(domainParam.toSet)), None, _)))
        }
        case "MTL_NotPreceededBy" => NABMatchers(tmplFuncId, domainParam, checkerIds, false, id)
        case "MTL_NotFollowedBy" => NABMatchers(tmplFuncId, domainParam, checkerIds, true, id)
        case "MTL_NotPreceededOrFollowedBy" => {
          if (domainParam.length != 3) throw MatcherTemplateErrorSpawnMatcher(tmplFuncId, paramCountError(3))
          val nabParam = domainParam.slice(0, NABParamCount)
          val nab = NABMatchers("NaBNc.NaB", nabParam, checkerIds, false, None)
          NABMatchers(nab, queryPoolMatcher(domainParam(2).toSet), checkerIds, true, id)
        }
        case "MTL_NotOverlapWith" => {
          if (domainParam.length != 2) throw MatcherTemplateErrorSpawnMatcher(tmplFuncId, paramCountError(2))
          val paramSets = domainParam.map(_.toSet)
          val matcher = queryPoolMatcher(paramSets(0))
          val notMatcher = queryPoolMatcher(paramSets(1))
          matchersNonOverlap(matcher, notMatcher, id)
        }
        case "MTL_PreceededBy" => {
          if (domainParam.length != 2) throw MatcherTemplateErrorSpawnMatcher(tmplFuncId, paramCountError(2))
          val paramSets = domainParam.map(_.toSet)
          val expected = queryPoolMatcher(paramSets(0))
          val matcher = queryPoolMatcher(paramSets(1))
          matchersLookaround(expected, matcher, checkerIds, false, id)
        }
        case "MTL_FollowedBy" => {
          if (domainParam.length != 2) throw MatcherTemplateErrorSpawnMatcher(tmplFuncId, paramCountError(2))
          val paramSets = domainParam.map(_.toSet)
          val expected = queryPoolMatcher(paramSets(1))
          val matcher = queryPoolMatcher(paramSets(0))
          matchersLookaround(expected, matcher, checkerIds, true, id)
        }
        case "MTL_HeadTailNotInbetween" => {
          if (domainParam.length < 3) throw MatcherTemplateErrorSpawnMatcher(tmplFuncId, "At least 3 params required")
          val paramSets:Array[Set[String]] = domainParam.map(_.toSet)
          val ms = paramSets.map(ps => queryPoolMatcher(ps))
          if (domainParam.length == 3) matchersOrdered(ms, IndexedSeq(1), checkerIds, id)
          else {
            // merge neg matchers in between
            val negMatcher = matchersOR(ms.slice(1, ms.length-1))
            val matcherSeq = Seq(ms.head, negMatcher, ms.last)
            matchersOrdered(matcherSeq, IndexedSeq(1), checkerIds, id)
          }
        }
        case "MTL_Sequence" => {
          SeqMatchers(tmplFuncId, domainParam, checkerIds, id)
        }

          //if (domainParam.length != 3) throw MatcherTemplateErrorSpawnMatcher(tmplFuncId, "exact 3 params required")
          //if (checkerIds.length == 1) matchersOrderedAllPositive(domainParam.map(queryPoolMatcher), matchCheckerLib, checkerIds(0), id)
          //else matchersOR(id, checkerIds.map(matchersOrderedAllPositive(domainParam.map(queryPoolMatcher), matchCheckerLib, _)))
        case "MTL_AtomSequence" => {
          //if (param.length != 1) throw MatcherTemplateErrorSpawnMatcher(tmplFuncId, paramCountError(1))
          val seqMatchers = param(0).map(varLengthStringMatcher(_, checkerIds)) //param.map(p => { varLengthStringMatcher(p(0), checkerIds)
            //val words = p.split("\\s+")
            //if (checkerIds.length == 1) varLengthStringMatcher(p(0)) //matchersOrderedAllPositive(words.map(w => fromAtomMatcher(F(w))), matchCheckerLib, checkerIds(0))
            //else  //matchersOR(id, checkerIds.map(varLengthStringMatcher(_))) //matchersOrderedAllPositive(words.map(w => fromAtomMatcher(F(w))), matchCheckerLib, _)))
         // })
          matchersOR(id, seqMatchers)
        }
        case "MTL_AnyAtom" => {
          val count = param(0)(0).toInt
          anyAtomMatcher(count, checkerIds, id)
          //if (checkerIds.length == 1) anyAtomMatcher(count, checkerIds(0), id)
          //else matchersOR(id, checkerIds.map(anyAtomMatcher(count, _)))
        }
        case x => throw NotImplemented(s"Unknown matcher template: $x")
      }
    }

    def contains(tmplFuncId:String):Boolean = _templateMap.contains(tmplFuncId) || _funcMap.contains(tmplFuncId)
  }
}