package org.ditw.mateng

import org.json4s.JsonAST._
import org.json4s.{CustomSerializer, NoTypeHints}
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization

/**
  * Created by jiaji on 2016-02-06.
  */
trait TAtomMatcher {
  def check(atom:TAtom):Boolean
}

object TAtomMatcher {

  private[TAtomMatcher] class StringMatchHelper(values:Array[String], val caseSensi:Boolean = false) {
    private val _values = if (caseSensi) values else values.map(_.toLowerCase)
    def matches(s:String):Boolean = {
      val t = if (caseSensi) s else s.toLowerCase
      _values.contains(t)
    }
  }

  val Any = new TAtomMatcher {
    def check(atom:TAtom) = true
  }

  val texts = "texts"

  private val toStringTmpl = "Atom(%s %s %s)"

  private[TAtomMatcher] class _TextMatcher(val values:Array[String], val caseSensi:Boolean = false, exclude:Boolean = false) extends TAtomMatcher {
    private val _helper = new StringMatchHelper(values, caseSensi)
    def check(atom:TAtom) = {
      val r = _helper.matches(atom.text)
      if (exclude) !r else r
    }
    override def toString = toStringTmpl.format("Txt", "=", values.mkString("/"))
  }

  private[TAtomMatcher] class _TextRegexMatcher(val regex:String, exclude:Boolean = false) extends TAtomMatcher {
    private val _pattern = regex.r.pattern
    def check(atom:TAtom) = {
      val hasMatch = _pattern.matcher(atom.text).matches
      if (exclude) !hasMatch else hasMatch
    }
    override def toString = s"Regex: $regex"
  }

  def textMatcher(values:Array[String], caseSensi:Boolean = false, exclude:Boolean = false) = new _TextMatcher(values, caseSensi, exclude)
  def textRegexMatcher(regex:String, exclude:Boolean = false) = new _TextRegexMatcher(regex, exclude)

  abstract class PropMatchBase(val name:String, val exclude:Boolean) extends TAtomMatcher {
    //def isExclude:Boolean = !exclude.isEmpty && exclude.get
    def check(atom:TAtom):Boolean = {
      val propValues = atom.propValues(name)
      val r = if (propValues.isSuccess) propValues.get.exists(_checkOne) else false
      if (!exclude) r
      else !r
    }
    def _checkOne(v:String):Boolean
  }


  private[TAtomMatcher] class _PropMatchExact(name:String, val values:Array[String], val caseSensi:Boolean, exclude:Boolean = false) extends PropMatchBase(name, exclude) {
    private val _helper = new StringMatchHelper(values, caseSensi)
    override def _checkOne(v:String):Boolean = _helper.matches(v)
    override def toString = toStringTmpl.format(name, "=", values.mkString("/"))
  }


  private[TAtomMatcher] class _PropMatchRegex(name:String, val regex:String, exclude:Boolean = false) extends PropMatchBase(name, exclude) {
    private val _p = regex.r.pattern
    override def _checkOne(v:String):Boolean = _p.matcher(v).matches()
    override def toString = toStringTmpl.format(name, "regex", regex)
  }

  def propMatchExact(name:String, values:Array[String], caseSensi:Boolean = false, exclude:Boolean = false) = new _PropMatchExact(name, values, caseSensi, exclude)
  def propMatchRegex(name:String, regex:String, exclude:Boolean = false) = new _PropMatchRegex(name, regex, exclude)

  private[TAtomMatcher] class _CompositeMatcher(val subMatchers:Seq[TAtomMatcher]) extends TAtomMatcher {
    def check(atom:TAtom) = subMatchers.forall(_.check(atom))
  }

  def composite(subMatchers:Seq[TAtomMatcher]):TAtomMatcher = new _CompositeMatcher(subMatchers)

  /*
  private[TAtomMatcher] class _AtomMatcher(val texts:Option[List[String]], val properties:List[PropMatchBase], val caseSensi:Option[Boolean]) extends TAtomMatcher {
    def isCaseSensitive = !caseSensi.isEmpty && caseSensi.get
    def check(atom:TAtom)= false
  }

  class PropertyMatchSerializer extends CustomSerializer[PropMatchBase](
    format => ({
      case JObject(List(JField("name", JString(propName)), JField("value", JString(value)))) => propMatchExact(propName, Array(value))
      case JObject(List(JField("name", JString(propName)), JField("value", JString(value)), JField("caseSensi", JBool(caseSensi)))) => propMatchExact(propName, Array(value), caseSensi)
      case JObject(List(JField("name", JString(propName)), JField("values", JArray(arr)))) => propMatchExact(propName, arr.map(_.asInstanceOf[JString].s).toArray)
      case x => throw new IllegalArgumentException("Unknown JSON format: " + x)
      },
      {
        case _ => throw new IllegalArgumentException("Unsupported operation!")
      }
    )
  )

  implicit val _format = Serialization.formats(NoTypeHints) + new PropertyMatchSerializer
  def atomMatcher(jsonDef:String):TAtomMatcher = parse(jsonDef).extract[_AtomMatcher]
  */
}