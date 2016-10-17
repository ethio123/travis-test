package org.ditw.mateng

import org.ditw.mateng.TAtomMatcher.PropMatchBase
import org.ditw.mateng.utils.MatEngError

import scala.util.{Failure, Success, Try}

/**
  * Created by jiaji on 2016-02-09.
  */
trait TPropMatcherTmpl {
  val id:String
  //val matchType:PropMatchType,
  //val caseSensi:Boolean,
  val exclude:Boolean
  def spawn(params:List[AnyRef], regexDict:Map[String,String]):Try[TAtomMatcher]
}

object TPropMatcherTmpl {
  object PropMatchType extends Enumeration {
    type PropMatchType = Value
    val Equals, AtLeastOne, All, Regex = Value
  }

  import PropMatchType._
  import TAtomMatcher._
  import ErrorHandling._

  private def try2GetParamOf[T](c:Class[T], p:AnyRef):Try[T] = {
    if (c.isAssignableFrom(p.getClass)) Success(p.asInstanceOf[T])
    else Failure(AtomPropMatcherLibErrorParamType(c.toString, p.getClass.toString))
    /*
    p match {
      case p:T => Success(p)
      case x =>
    }
    */
  }
  private def try2GetString(p:AnyRef):Try[String] = try2GetParamOf[String](classOf[String], p)
  private def try2GetStringArray(p:AnyRef):Try[Array[String]] = try2GetParamOf[Array[String]](classOf[Array[String]], p)

  private def _paramCount(params:List[AnyRef], expected:Int):Option[MatEngError] =
    if (params.size != expected) {
      Option(AtomPropMatcherLibErrorParamCount(KnownPropMatcherParamCount, params.size))
    } else None

  private val KnownPropMatcherParamCount = 1
  private[TPropMatcherTmpl] class _KnownPropMatcher(val propName:String, val id:String, val matchType:PropMatchType, val caseSensi:Boolean, val exclude:Boolean)
    extends TPropMatcherTmpl {
    override def spawn(params:List[AnyRef], regexDict:Map[String,String]):Try[TAtomMatcher] = {
      val cr = _paramCount(params, KnownPropMatcherParamCount)
      if (cr.isDefined) Failure(cr.get)
      else try2GetStringArray(params(0)).flatMap(strArr => Success(
        if (propName == texts) textMatcher(strArr, caseSensi) else propMatchExact(propName, strArr, caseSensi, exclude)
      ))
    }
  }

  private[TPropMatcherTmpl] class _KnownPropRegexMatcher(val propName:String, val id:String, val exclude:Boolean)
    extends TPropMatcherTmpl {
    override def spawn(params:List[AnyRef], regexDict:Map[String,String]):Try[TAtomMatcher] = {
      val cr = _paramCount(params, KnownPropMatcherParamCount)
      if (cr.isDefined) Failure(cr.get)
      else try2GetStringArray(params.head).flatMap{ regexId =>
        val regex = regexDict(regexId(0))
        if (propName == texts) Success(textRegexMatcher(regex, exclude))
        else Success(propMatchRegex(propName, regex, exclude))
      }
    }
  }

  private val UnknownPropMatcherParamCount = 3
  private[TPropMatcherTmpl] class _UnknownPropEntityMatcher(val id:String, val matchType:PropMatchType, val caseSensi:Boolean, val exclude:Boolean)
    extends TPropMatcherTmpl {
    override def spawn(params:List[AnyRef], regexDict:Map[String,String]):Try[TAtomMatcher] = {
      val cr = _paramCount(params, UnknownPropMatcherParamCount)
      if (cr.isDefined) Failure(cr.get)
      else {
        val entityType = try2GetStringArray(params.head).get(0)
        val propName = try2GetStringArray(params(1)).get(0)
        try2GetStringArray(params(2)).flatMap{strArr =>
          val matchers = Seq(AtomPropMatcherLib.E(regexDict, Array(entityType)), propMatchExact(propName, strArr, caseSensi, exclude))
          Success(composite(matchers))
        }
      }
    }
  }

  private[TPropMatcherTmpl] class _UnknownPropEntityRegexMatcher(val id:String, val exclude:Boolean)
    extends TPropMatcherTmpl {
    override def spawn(params:List[AnyRef], regexDict:Map[String,String]):Try[TAtomMatcher] = {
      val cr = _paramCount(params, UnknownPropMatcherParamCount)
      if (cr.isDefined) Failure(cr.get)
      else {
        val entityType = try2GetStringArray(params.head).get(0)
        val propName = try2GetStringArray(params(1)).get(0)
        try2GetStringArray(params(2)).flatMap{ regex =>
          val matchers = Seq(AtomPropMatcherLib.E(regexDict, Array(entityType)), propMatchRegex(propName, regexDict(regex(0)), exclude))
          Success(composite(matchers))
        }
      }
    }
  }

  def KnownProp(id:String, propName:String, matchType:PropMatchType = AtLeastOne, caseSensi:Boolean = false, exclude:Boolean = false) =
    new _KnownPropMatcher(propName, id, matchType, caseSensi, exclude)
  def UnknownProp(id:String, matchType:PropMatchType = AtLeastOne, caseSensi:Boolean = false, exclude:Boolean = false) =
    new _UnknownPropEntityMatcher(id, matchType, caseSensi, exclude)
  def KnownPropRegex(id:String, propName:String, exclude:Boolean = false) =
    new _KnownPropRegexMatcher(propName, id, exclude)
  def UnknownPropRegex(id:String, exclude:Boolean = false) =
    new _UnknownPropEntityRegexMatcher(id, exclude)

}