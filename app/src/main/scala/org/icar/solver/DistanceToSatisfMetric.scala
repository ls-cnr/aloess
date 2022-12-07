package org.icar.solver

import org.icar.subsymbolic._
import org.icar.symbolic.{AndGoalDecomposition, FOLGoalSpec, GoalNode, GoalSpec, GoalTree, LTLGoalSpec, MTLGoalSpec, OrGoalDecomposition}

abstract class DistanceToSatisfMetric(val goal_node : GoalTree) {
  def success_value : Double
  def failure_value : Double
  def distance(goal_state : Map[String, GoalState], state : RawState) : Double
  def resistance_generic_node(goal: GoalNode, goal_map : Map[String, GoalState], state : RawState): Double
  def resistance_and_node(name:String, subgoals:List[GoalNode],goal_map : Map[String, GoalState], state : RawState) : Double
  def resistance_or_node(name:String, subgoals:List[GoalNode],goal_map : Map[String, GoalState], state : RawState): Double
  def resistance_leaf_node(name:String,goal_map : Map[String, GoalState], state : RawState): Double
  def resistance_formula(formula: RawLogicFormula, state : RawState) : Double
}
