package org.ditw.mateng

import org.ditw.mateng.AtomPropMatcherLib._
import org.ditw.mateng.ConfValueStringParser.Parsed
import org.ditw.mateng.TAtomMatcher.PropMatchBase
import org.ditw.mateng.matchers.MatcherManager._
import org.ditw.mateng.matchers.{StoppedByMatcherManager, SubMatchCheckerLib}
import org.ditw.mateng.matchers.SubMatchCheckerLib
import org.ditw.mateng.test.TestAtom.Atom
import org.ditw.mateng.test.TestInput

import scala.util.{Failure, Success, Try}

/**
  * Created by jiaji on 2016-02-08.
  */
object TestHelper {


  import org.ditw.mateng.test.TestInput._
  def inputFrom(sentence:String):TInput = {
    val tokens = sentence.split("\\s+")
    fromAtomArrayEng(tokens.map(Atom(_, Map())).toIndexedSeq)
  }

  val EmptySubMatchCheckerLib = new SubMatchCheckerLib(List(), List())
  val StaticSubMatchCheckerLib = new SubMatchCheckerLib(List("Lng" -> Parsed("Lng", Array())), List())

  def DummyResultPool(input:TInput) = TMatchResultPool.create(input, EmptySubMatchCheckerLib)
}
