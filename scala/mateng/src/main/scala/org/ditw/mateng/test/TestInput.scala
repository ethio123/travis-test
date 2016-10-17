package org.ditw.mateng.test

import org.ditw.mateng.matchers.TMatcher.MId
import org.ditw.mateng.{ErrorHandling, TAtom, TContext, TInput}
import org.ditw.mateng.{TAtom, TContext, TInput}
import org.ditw.mateng.utils.MatEngError

/**
  * Created by jiaji on 2016-02-14.
  */
object TestInput {

  private[TestInput] class _Input(val lang:String, val atoms:IndexedSeq[TAtom], val context:TContext = TInput.EmptyContext) extends TInput {
    def subMatchChecker(checkerId:String) = throw MatEngError.NotImplemented
  }
  def fromAtomArray(lang:String, atoms:IndexedSeq[TAtom]):TInput = new _Input(lang, atoms)
  def fromAtomArrayEng(atoms:IndexedSeq[TAtom]):TInput = new _Input("eng", atoms)
}
