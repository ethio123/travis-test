package org.ditw.mateng.matchers

/**
  * Created by jiaji on 2016-02-25.
  */
trait TAtomDistanceCalc {
  def calc(atomRange1:Range, atomRange2:Range):TAtomDistance
}
