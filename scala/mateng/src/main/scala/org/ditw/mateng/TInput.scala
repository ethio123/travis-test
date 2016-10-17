package org.ditw.mateng

import org.ditw.mateng.matchers.TMatcher.MId
import org.ditw.mateng.matchers.TSubMatchChecker

/**
  * Created by jiaji on 2016-02-11.
  */
trait TInput {
  val lang:String
  def atoms:IndexedSeq[TAtom]
  //def subMatchChecker(checkerId:String):TSubMatchChecker
  val context:TContext
}

object TInput {
  val EmptyContext = new TContext {
    override def queryMatch(matcherId: MId): Boolean = false
    override def getFlag(flag: String): Boolean = false
    override def getThreshold(key:String):Option[Double] = None
  }
}
