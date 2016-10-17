package org.ditw.mateng

import org.ditw.mateng.test.TestAtom.Atom
import org.ditw.mateng.utils.MatEngError
import org.scalatest.{FlatSpec, ShouldMatchers}
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.prop.Tables.Table

import scala.util.Failure

/**
  * Created by jiaji on 2016-02-05.
  */
class TAtomTest extends FlatSpec with ShouldMatchers with TableDrivenPropertyChecks {

  import ErrorHandling._
  //import TestHelper.Atom

  def newMap(text:String, vset:Set[String]) = Map[String,Set[String]](text -> vset)



  val atomCreationTable = Table(
    ("text", "propMap"),
    ("txt1", newMap("prop1", Set("v1", "v2"))),
    ("txt2", newMap("prop2", Set[String]()))
  )

  "applyTest" should "work" in {
    forAll(atomCreationTable) {
      (text, propMap) => {
        val atom = Atom(text, propMap)

        atom.text shouldBe(text)
        propMap.keySet.foreach(
          k => {
            propMap.get(k).get shouldBe(atom.propValues(k).get)
          }
        )
      }
    }
  }

  "propValueTest2" should "work" in {
    forAll(atomCreationTable) {
      (text, propMap) => {
        val atom = Atom(text, propMap)

        atom.text shouldBe(text)
        propMap.keySet.foreach(
          k => {
            val v = atom.propValue(k)
            v.isFailure shouldBe(true)
            v match {
              case Failure(x) => {
                x match {
                  case e:MatEngError => println(e.describe)
                  case xx => fail("Expects AtomErrorMultvalueProp, but get %s instead".format(xx.getClass))
                }
              }
              case _ => fail("Success result not expected")
            }
          }
        )
      }
    }
  }
}
