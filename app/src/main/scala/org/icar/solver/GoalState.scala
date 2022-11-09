package org.icar.solver

import org.icar.subsymbolic.RawLogicFormula
import org.icar.symbolic.{AndGoalDecomposition, GoalNode, GoalSpec, GoalTree, LTLGoalSpec, MTLGoalSpec, OrGoalDecomposition}


case class GoalModelMap(root:String, map : Map[String,GoalState]) {

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
    val color = if (goal_state.satisf == FullSatisfaction()) "green" else "black"
    var string = ""
    for (s <- subgoals) {
      string += stringGraphviz_generic_node(goal_model, s)
      string += s"\"$name\"[color=$color];\n"
      string += s"\"$name\"->\"${s.id}\";\n"
    }
  string
  }

  def stringGraphviz_or_node(goal_model : GoalTree, name: String, subgoals: List[GoalNode]): String = {
    val goal_state = map(name)
    val color = if (goal_state.satisf == FullSatisfaction()) "green" else "black"
    var string = ""
    for (s <- subgoals) {
      string += stringGraphviz_generic_node(goal_model, s)
      string += s"\"$name\"[shape=diamond,color=$color];\n"
      string += s"\"$name\"->\"$s.id\";\n"
    }
    string
  }

  def stringGraphviz_leaf_node(goal_model : GoalTree, name: String): String = {
    val goal_state = map(name)
    val color = if (goal_state.satisf == FullSatisfaction()) "green" else "black"
    s"\"$name\"[color=$color];\n"
  }


}

case class GoalState(achievement: AchievementState, satisf : GoalSatisfaction, switch_to_committ:Boolean, switch_to_satisf:Boolean) {
}



abstract class AchievementState
case class DependsOnSubgoals() extends AchievementState
case class Ready(cond : RawLogicFormula) extends AchievementState
case class Committed(cond : RawLogicFormula) extends AchievementState
case class Completed() extends AchievementState

abstract class GoalSatisfaction
case class FullSatisfaction() extends GoalSatisfaction
case class Violation() extends GoalSatisfaction
case class PartialSatisfaction(degree:Double) extends GoalSatisfaction