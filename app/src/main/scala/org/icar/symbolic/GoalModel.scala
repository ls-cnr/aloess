package org.icar.symbolic

import java.time.{Duration, LocalDateTime}

abstract class GoalModel
case class GoalTree(root : GoalNode, meta:List[MetaGoal]) extends GoalModel {

}
case class FlatGoalList[GoalType](goals : List[GoalType]) extends GoalModel


abstract class GoalNode(val id : String)
case class AndGoalDecomposition(name : String,subgoals : List[GoalNode]) extends GoalNode(name)
case class OrGoalDecomposition(name : String,subgoals : List[GoalNode]) extends GoalNode(name)

case class GoalSpec(name : String,trigger: LogicFormula with PropositionNature, formula:LogicFormula with PropositionNature) extends GoalNode(name)
case class FOLGoalSpec(name : String,trigger: LogicFormula with FOLNature, formula:LogicFormula with FOLNature) extends GoalNode(name)
case class LTLGoalSpec(name : String,trigger: LogicFormula with FOLNature, formula:LogicFormula with LTLNature) extends GoalNode(name)
case class MTLGoalSpec(name : String,trigger: LogicFormula with FOLNature, formula:LogicFormula with MTLNature) extends GoalNode(name)


abstract class MetaGoal
abstract class GoalPattern extends MetaGoal
case class ExplicitStartTime(target : String, start : LocalDateTime) extends GoalPattern
case class ExplicitStartEvent(target : String, event : LogicFormula with FOLNature, cap_name : String = "") extends GoalPattern
case class ExplicitEndTime(target : String, start : LocalDateTime) extends GoalPattern
case class ExplicitEndEvent(target : String, event : LogicFormula with FOLNature, cap_name : String = "") extends GoalPattern
case class RepeatUntil(target : String,condition : LogicFormula with FOLNature, cap_name : String = "") extends GoalPattern
case class AllowMultiInstance(target : String) extends GoalPattern
case class AllowParallel(target : String) extends GoalPattern

abstract class TemporalConstraint extends MetaGoal
case class GoalDuration(target : String, duration : Duration) extends TemporalConstraint

abstract class FailureRate extends MetaGoal
case class ProbabilisticFailureRate(target : String,prob : Double) extends FailureRate