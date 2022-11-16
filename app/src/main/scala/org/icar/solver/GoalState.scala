package org.icar.solver

import org.icar.subsymbolic.RawLogicFormula
import org.icar.symbolic.{AndGoalDecomposition, GoalNode, GoalSpec, GoalTree, LTLGoalSpec, MTLGoalSpec, OrGoalDecomposition}


case class GoalModelMap(root:String, map : Map[String,GoalState]) {

  def degree : Double = map(root).sat_degree

  def get_root_state : GoalState = map(root)

  def stringGraphviz(goal_model : GoalTree) : String = {
    val body = stringGraphviz_generic_node(goal_model,goal_model.root)
    s"digraph GoalState {\n $body }\n"
  }

  def stringGraphviz_generic_node(goal_model : GoalTree, node : GoalNode) : String = {
    node match {
      case AndGoalDecomposition(name,subgoals) => stringGraphviz_and_node(goal_model : GoalTree,name,subgoals)
      case OrGoalDecomposition(name,subgoals) => stringGraphviz_or_node(goal_model : GoalTree,name,subgoals)
      case GoalSpec(name,_,_) => stringGraphviz_leaf_node(goal_model : GoalTree,name)
      case LTLGoalSpec(name,_,_) => stringGraphviz_leaf_node(goal_model : GoalTree,name)
      case MTLGoalSpec(name,_,_) => stringGraphviz_leaf_node(goal_model : GoalTree,name)
    }
  }

  def stringGraphviz_and_node(goal_model : GoalTree, name: String, subgoals: List[GoalNode]): String = {
    val goal_state = map(name)
    var color = "black"
    var label = s"\"$name\""
    goal_state.sat_state match {
      case FullSatisfaction() => color = "green"
      case PartialSatisfaction() => color = "blue"; label = s"\"$name(${goal_state.sat_degree})\""
      case Violation() => color = "red"
      case _ =>
    }

    var string = ""
    for (s <- subgoals) {
      string += stringGraphviz_generic_node(goal_model, s)
      string += s"\"$name\"[color=$color,label=$label];\n"
      string += s"\"$name\"->\"${s.id}\";\n"
    }
  string
  }

  def stringGraphviz_or_node(goal_model : GoalTree, name: String, subgoals: List[GoalNode]): String = {
    val goal_state = map(name)
    var color = "black"
    var label = s"\"$name\""
    goal_state.sat_state match {
      case FullSatisfaction() => color = "green"
      case PartialSatisfaction() => color = "blue"; label = s"\"$name(${goal_state.sat_degree})\""
      case Violation() => color = "red"
      case _ =>
    }
    var string = ""
    for (s <- subgoals) {
      string += stringGraphviz_generic_node(goal_model, s)
      string += s"\"$name\"[shape=diamond,color=$color,label=$label];\n"
      string += s"\"$name\"->\"$s.id\";\n"
    }
    string
  }

  def stringGraphviz_leaf_node(goal_model : GoalTree, name: String): String = {
    val goal_state = map(name)
    var color = "black"
    var label = s"\"$name\""
    goal_state.sat_state match {
      case FullSatisfaction() => color = "green"
      case PartialSatisfaction() => color = "blue"; label = s"\"$name(${goal_state.sat_degree})\""
      case Violation() => color = "red"
      case _ =>
    }
    s"\"$name\"[color=$color,label=$label];\n"
  }


}

case class GoalState(internal_state: AchievementState, sat_state : GoalSatisfaction, sat_degree : Double, switch_to_committ:Boolean, switch_to_satisf:Boolean) {
}



abstract class AchievementState
case class DependsOnSubgoals() extends AchievementState
case class Ready(cond : RawLogicFormula) extends AchievementState
case class Committed(cond : RawLogicFormula) extends AchievementState
case class Completed() extends AchievementState

abstract class GoalSatisfaction
case class FullSatisfaction() extends GoalSatisfaction
case class Violation() extends GoalSatisfaction
case class PartialSatisfaction() extends GoalSatisfaction //degree:Double