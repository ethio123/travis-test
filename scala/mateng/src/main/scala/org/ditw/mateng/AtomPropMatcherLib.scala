package org.ditw.mateng

import org.json4s.{NoTypeHints, CustomSerializer}
import org.json4s.JsonAST.{JString, JField, JObject}
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization

import scala.util.{Success, Failure, Try}

/**
  * Created by jiaji on 2016-02-08.
  */
object AtomPropMatcherLib {
  import TPropMatcherTmpl._
  import ErrorHandling._
  import TPropMatcherTmpl.PropMatchType._

  /*
  object BuiltInMatcherTmpl extends Enumeration {
    type BuiltInMatcherTmpl = Value
    val F, L = Value
  }

  import BuiltInMatcherTmpl._
  */
  import TPropMatcherTmpl._

  /// json def
  /// {
  ///  "id": "...",
  ///  "propName": "...",
  ///  "matchType": "...",
  ///  "caseSensi": "...",
  ///  "exclude": "..."
  /// }
  private[AtomPropMatcherLib] class PropMatcherTmplJson(
    val id:String,
    val propName:Option[String],
    val matchType:Option[String],
    val caseSensi:Option[Boolean],
    val exclude:Option[Boolean]
  )
  import PropMatchType._

  implicit def fromPropMatcherTmplJson(j:PropMatcherTmplJson):TPropMatcherTmpl = {
    val matchType = if (j.matchType.isEmpty) AtLeastOne else PropMatchType.withName(j.matchType.get)
    val caseSensi = if (j.caseSensi.isEmpty) false else j.caseSensi.get
    val exclude = if (j.exclude.isEmpty) false else j.exclude.get

    if (matchType != Regex) {
      if (j.propName.nonEmpty) KnownProp(j.id, j.propName.get, matchType, caseSensi, exclude)
      // known property
      else UnknownProp(j.id, matchType, caseSensi, exclude)
    }
    else {
      if (j.propName.nonEmpty) KnownPropRegex(j.id, j.propName.get, exclude)
      // known property
      else UnknownPropRegex(j.id, exclude)
    }
  }


  /*
  class PropMatcherTmplSerializer extends CustomSerializer[TPropMatcherTmpl](
    format => (
      {
        case JObject(fieldList) => {
          fieldList match {
            case fl:List[JField] => {

            }
          }
        }
        case JObject(List(JField("id", JString(id)), JField("propName", JString(propName)))) => KnownProp(id, propName)
        case JObject(List(JField("id", JString(id)), JField("propName", JString(propName)))) => KnownProp(id, propName)
        case JObject(List(JField("id", JString(id)), JField("matchType", JString(matchType)))) => {
          val mt = PropMatchType.withName(matchType)
          UnknownProp(id, PropMatchType.withName(matchType))
        }
        case JObject(List(JField("id", JString(id)))) => UnknownProp(id)
        case x => throw AtomErrorToDo(x.toString)
      },
      {
        case _ => throw NotImplemented
      }
    )
  )
  */

  //def pid(tmpl:BuiltInMatcherTmpl):String = tmpl.toString
  implicit val _formats = Serialization.formats(NoTypeHints)
  def loadPropMatcherTmplPool(json:String):Map[String,TPropMatcherTmpl] = {
    val tmplJsons = parse(json).extract[List[PropMatcherTmplJson]]
    tmplJsons.map(j => (j.id, fromPropMatcherTmplJson(j))).toMap
  }

  private val _TmplJson =
    """
      |[
      | {
      |  "id": "F",
      |  "propName": "texts"
      | },
      | {
      |  "id": "FR",
      |  "propName": "texts",
      |  "matchType": "Regex"
      | },
      | {
      |  "id": "E",
      |  "propName": "entityType"
      | },
      | {
      |  "id": "Er",
      |  "propName": "RLPType"
      | },
      | {
      |  "id": "PoS",
      |  "propName": "PoS-tag"
      | },
      | {
      |  "id": "C",
      |  "propName": "texts",
      |  "caseSensi": true
      | },
      | {
      |  "id": "L",
      |  "propName": "lemma"
      | },
      | {
      |  "id": "EA"
      | },
      | {
      |  "id": "EAx",
      |  "exclude": true
      | },
      | {
      |  "id": "EAR",
      |  "matchType": "Regex"
      | }
      |]
    """.stripMargin

  val PropMatcherTmplMap:Map[String,TPropMatcherTmpl] = loadPropMatcherTmplPool(_TmplJson)

  def spawn(defi:String, regexDict:Map[String,String]):TAtomMatcher = {
    val parsed = ConfValueStringParser.parse(defi)
    spawn(parsed.id, parsed.paras, regexDict)
  }
  def spawn(tmplId:String, tmplParams:Array[Array[String]], regexDict:Map[String,String]):TAtomMatcher = {
    // todo: error handling
    PropMatcherTmplMap.get(tmplId).get.spawn(tmplParams.toList, regexDict).get
  }

  val EmptyRegexDict = Map[String,String]()
  private def getTmpl(tmplId:String):TPropMatcherTmpl = PropMatcherTmplMap.get(tmplId).get
  val TmplFR = "FR"
  def FR(regexDict:Map[String,String], word:String*):TAtomMatcher = getTmpl(TmplFR).spawn(List(word.toArray), regexDict).get
  val TmplF = "F"
  def F(regexDict:Map[String,String], word:String*):TAtomMatcher = getTmpl(TmplF).spawn(List(word.toArray), regexDict).get
  def FExact(word:String) = F(EmptyRegexDict, word)
  val TmplE = "E"
  def E(regexDict:Map[String,String], entityTypes:Array[String]):TAtomMatcher = getTmpl(TmplE).spawn(List(entityTypes), regexDict).get
  val TmplEr = "Er"
  def Er(regexDict:Map[String,String], entityTypes:Array[String]):TAtomMatcher = getTmpl(TmplEr).spawn(List(entityTypes), regexDict).get
  val TmplL = "L"
  def L(regexDict:Map[String,String], lemma:String*):TAtomMatcher = getTmpl(TmplL).spawn(List(lemma.toArray), regexDict).get
  val TmplEA = "EA"
  def EA(regexDict:Map[String,String], attrName:String, attrValue:String):TAtomMatcher = getTmpl(TmplEA).spawn(List(attrName, attrValue), regexDict).get
  val TmplEAx = "EAx"
  def EAx(regexDict:Map[String,String], attrName:String, attrValue:String):TAtomMatcher = getTmpl(TmplEAx).spawn(List(attrName, attrValue), regexDict).get
  val OrganizationEntity = E(EmptyRegexDict, Array("Organization"))
  val CompanyEntity = E(EmptyRegexDict, Array("Company"))

  def contains(tmplId:String):Boolean = PropMatcherTmplMap.contains(tmplId)
}
