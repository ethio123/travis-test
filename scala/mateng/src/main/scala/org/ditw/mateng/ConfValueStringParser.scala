package org.ditw.mateng

import java.util.regex.Pattern

import ErrorHandling.AtomPropMatcherErrorDefinition

import scala.collection.mutable.ListBuffer

/**
  * Created by jiaji on 2016-02-16.
  */
object ConfValueStringParser {

  private def idxValid(idx:Int):Boolean = idx >= 0
  def parseTmplId(matcherDef:String):(String,Option[String]) = {
    val idx1:Int = matcherDef.indexOf('(')
    val idx2:Int = matcherDef.lastIndexOf(')')

    if (!idxValid(idx1) && !idxValid(idx2)) (matcherDef.trim, None)
    else if (idxValid(idx1) && idxValid(idx2) && idx1 < idx2) (matcherDef.substring(0, idx1), Option(matcherDef.substring(idx1+1, idx2)))
    else throw AtomPropMatcherErrorDefinition(matcherDef)
  }

  val LookaroundRegexPatternTemplate = """(?<!\\)\%s"""
  val EscapedCharTemplate = """\%s"""
  val FirstSeparatorChar = "|"
  val FirstSeparator = LookaroundRegexPatternTemplate.format(FirstSeparatorChar).r.pattern
  val EscapedFirstSeparator = EscapedCharTemplate.format(FirstSeparatorChar)

  val SecondSeparatorChar = "/"
  val SecondSeparatorStr = LookaroundRegexPatternTemplate.format(SecondSeparatorChar)
  val SecondSeparator = SecondSeparatorStr.r.pattern
  val EscapedSecondSeparator = EscapedCharTemplate.format(SecondSeparatorChar)

  val EscapedEscape = """\\"""

  //val MatchFuncParamSeparator = """(?<!\\)/"""
  //val MatchFuncParamEscapedSeparator = """\/"""

  def parseParams(paramStr:String):Array[Array[String]] = {
    val paramArr:Array[String] = FirstSeparator.split(paramStr).map(_.trim.replace(EscapedFirstSeparator, FirstSeparatorChar))
    val r = paramArr.map(p => SecondSeparator.split(p).map(_.trim.replace(EscapedSecondSeparator, SecondSeparatorChar).replace(EscapedEscape, "\\")))
    r
  }

  val ParamSign = "$"
  def parseParam(in:String):Option[Int] = {
    if (in.trim.startsWith(ParamSign)) {
      val rem = in.trim.substring(ParamSign.length)
      if (rem.nonEmpty && rem.forall(_.isDigit)) Option(rem.toInt)
      else None
    }
    else None
  }

  private val EmptyParams = Array[Array[String]]()

  case class Parsed(val id:String, val paras:Array[Array[String]]) {
    def unparse = {
      if (paras.nonEmpty && paras.forall(_.nonEmpty)) {
        val paraStr = paras.map(_.mkString(SecondSeparatorChar)).mkString(FirstSeparatorChar)
        s"$id($paraStr)"
      }
      else id
    }
  }

  def parse(conf:String):Parsed = {
    val (id, paras) = parseTmplId(conf)
    new Parsed(id, if (paras.isDefined) parseParams(paras.get) else EmptyParams)
  }
}
