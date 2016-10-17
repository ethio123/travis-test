package org.ditw.mateng

import org.ditw.mateng.matchers.TMatcher.MId

/**
  * Created by jiaji on 2016-08-26.
  */
trait TContext {
  def getFlag(flag:String):Boolean
  def queryMatch(matcherId:MId):Boolean
  def getThreshold(key:String):Option[Double]
}
