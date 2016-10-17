package org.ditw.mateng

import utils.MatEngError
import utils.MatEngError.MatEngErrorBase
import org.ditw.mateng.matchers.TMatcher

/**
  * Created by jiaji on 2016-02-05.
  */

object ErrorHandling {

  val MatchCheckerErrorNoSubMatches = new MatEngErrorBase("No sub-matches found. Checking cannot be performed!")

  final case class NotImplemented(msg:String)
    extends MatEngErrorBase(s"Not Implemented: $msg")

  final case class ExtractBlockErrorIllFormattedDef(extractBlockDef:String)
    extends MatEngErrorBase("Unrecognizable extract block definition: %s".format(extractBlockDef))

  final case class MatcherErrorNoConsecutiveNegMatchers(negIndices:IndexedSeq[Int])
    extends MatEngErrorBase("Consecutive negative sub-matches (indices: %s) not allowed (doesn't make sense).".format(negIndices))

  final case class MatcherManagerErrorMatcherIdAlreadyExist(existing:TMatcher, toAdd:TMatcher)
    extends MatEngErrorBase("Matcher with id(%s) already exist: [%s], cannot add another [%s] with the same id".format(existing.id, existing, toAdd))

  final case class ExtractErrorJSONFormat(jsonObj:AnyRef)
    extends MatEngErrorBase("Unable to deserialize: %s".format(jsonObj))

  final case class SubMatchCheckerLibErrorUnknownTemplate(templateId:String)
    extends MatEngErrorBase("Unknown template: %s".format(templateId))

  final case class SubMatchCheckerErrorUnknownCheckerId(checkerId:String)
    extends MatEngErrorBase("Unknown checker Id: %s".format(checkerId))

  final case class MatcherTemplateErrorSpawnMatcher(tmplId:String, msg:String)
    extends MatEngErrorBase(s"Template [$tmplId] spawning error: $msg")

  final case class AtomErrorNoProp(propName:String)
    extends MatEngErrorBase("Property [%s] not present.".format(propName))

  final case class MatchCheckerInputDiffer(m1:AtomSeqMatch, m2:AtomSeqMatch)
    extends MatEngErrorBase("Matches are from different input: [%s] vs. [%s]".format(m1, m2))

  final case class AtomErrorMultvalueProp(propName:String,values:Set[String])
    extends MatEngErrorBase("Property [%s] is not a single-valued property (values:[%s], call propValues instead.".format(propName, values))

  final case class AtomPropMatcherLibErrorParamCount(expected:Int, actual:Int)
    extends MatEngErrorBase("Parameter count does not match: expected [%d], actual [%d].".format(expected, actual))

  final case class AtomPropMatcherLibErrorParamType(expected:String, actual:String)
    extends MatEngErrorBase("Parameter type does not match: expected [%s], actual [%s].".format(expected, actual))

  final case class AtomPropMatcherErrorDefinition(matcherDef:String)
    extends MatEngErrorBase("Unrecognized property matcher definition: [%s]".format(matcherDef))
}
