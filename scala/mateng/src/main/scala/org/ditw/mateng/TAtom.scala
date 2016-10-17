package org.ditw.mateng

import scala.util.{Success, Failure, Try}

/**
  * Created by jiaji on 2016-02-04.
  */
trait TAtom {
  def text:String
  def propValue(propName:String):Try[String]
  def propValues(propName:String):Try[Set[String]]
}

object TAtom {
}