package org.ditw.mateng.test

import org.ditw.mateng.ErrorHandling.{AtomErrorMultvalueProp, AtomErrorNoProp}
import org.ditw.mateng.ErrorHandling
import org.ditw.mateng.TAtom

import scala.util.{Failure, Success, Try}

/**
  * Created by jiaji on 2016-02-14.
  */
object TestAtom {
  object Atom {
    val EmptyPropValueSet = Set[String]()
    import org.ditw.mateng.ErrorHandling._
    private[Atom] class _Atom(val _text:String, val _propMap:Map[String,Set[String]]) extends TAtom {
      def text = _text
      def propValue(propName:String):Try[String] = {
        if (_propMap.contains(propName)) {
          val values = _propMap.get(propName).get
          if (values.size != 1) {
            Failure(AtomErrorMultvalueProp(propName, values))
          }
          else Success(values.toList(0))
        }
        else Failure(AtomErrorNoProp(propName))
      }

      def propValues(propName:String):Try[Set[String]] =
        if (_propMap.contains(propName)) Success(_propMap.get(propName).get) else Failure(AtomErrorNoProp(propName))
    }

    def apply(text:String, propMap:Map[String,Set[String]]):TAtom = new _Atom(text, propMap)

  }

  def entityAtom(text:String, entityTypes:String*):TAtom = Atom(text, Map("entityType" -> entityTypes.toSet))

  val EmptyPropMap = Map[String,Set[String]]()
  def textAtom(text:String):TAtom = Atom(text, EmptyPropMap)

  val FBI = entityAtom("FBI", "Organization", "OrgEntity")
  val CIA = entityAtom("CIA", "Organization", "OrgEntity")
  val Anonymous = //entityAtom("Anonymous", "Organization", "OrgEntity")
    Atom("Anonymous", Map("entityType" -> Set("Organization", "OrgEntity"), "category" -> Set("ThreatActor")))
  val Microsoft = entityAtom("Microsoft", "Company", "OrgEntity")
  val _and = textAtom("and")
  val _comma = textAtom(",")
}
