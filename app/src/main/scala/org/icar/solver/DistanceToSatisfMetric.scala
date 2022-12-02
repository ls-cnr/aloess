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


class EffortToSatisf(override val goal_node : GoalTree) extends DistanceToSatisfMetric(goal_node) {
  override def distance(goal_state: Map[String, GoalState], state : RawState): Double = resistance_generic_node(goal_node.root,goal_state,state)

  val rmax : Double = 10
  val rmin : Double = 1/rmax

  def success_value : Double = 0
  def failure_value : Double = 1

  override def resistance_generic_node(goal: GoalNode, goal_map : Map[String, GoalState], state : RawState): Double = {
    goal match {
      case AndGoalDecomposition(name, subgoals) =>resistance_and_node(name, subgoals, goal_map,state)
      case OrGoalDecomposition(name, subgoals) =>resistance_or_node(name, subgoals, goal_map,state)
      case GoalSpec(name, _, _) =>resistance_leaf_node(name, goal_map,state)
      case FOLGoalSpec(name, _, _) =>resistance_leaf_node(name, goal_map,state)
      case LTLGoalSpec(name, _, _) =>resistance_leaf_node(name, goal_map,state)
      case MTLGoalSpec(name, _, _) =>resistance_leaf_node(name, goal_map,state)
    }
  }

  override def resistance_and_node(name:String, subgoals:List[GoalNode],goal_map : Map[String, GoalState], state : RawState) : Double = {
    var sum: Double = 0
    for (g<-subgoals) sum += resistance_generic_node(g,goal_map,state)
    sum/subgoals.size
  }
  override def resistance_or_node(name:String, subgoals:List[GoalNode],goal_map : Map[String, GoalState], state : RawState): Double = {
    var smaller: Double = 0

    for (g<-subgoals) smaller = Math.min(smaller,resistance_generic_node(g,goal_map,state))
    smaller
  }
  override def resistance_leaf_node(name:String,goal_map : Map[String, GoalState], state : RawState): Double = {
    val goal_state = goal_map(name)
    goal_state.internal_state match {
      case Ready(cond) =>
        val value = resistance_formula(cond,state)
        val norm = normalize_ready(value)
        norm
      case Committed(cond) =>
        val value = resistance_formula(cond,state)
        val norm = normalize_commit(value)
        norm
      case Completed() =>
        if (goal_state.sat_state==FullSatisfaction())
          success_value
        else
          failure_value
      case _ => failure_value
    }
  }

  override def resistance_formula(formula: RawLogicFormula, state : RawState) : Double = {
    formula match {
      case RawProposition(index) =>if (state.satisfies(index)) rmin else rmax
      case RawTT() =>rmax
      case RawFF() =>rmin
      case RawConj(left, right) =>resistance_formula(left,state)+resistance_formula(right,state)
      case RawDisj(left, right) => parallel(resistance_formula(left,state),resistance_formula(right,state))
      case RawNeg(op) =>1/resistance_formula(op,state)
      case RawImpl(left, right) =>resistance_formula(RawDisj(left,RawNeg(right)),state)
      case RawIff(left, right) =>resistance_formula(RawConj(RawDisj(left,RawNeg(right)),RawDisj(right,RawNeg(left))),state)
      case RawNext(op) =>resistance_formula(op,state)
      case RawUntil(left, right) =>parallel(resistance_formula(left,state),resistance_formula(right,state))
      case RawRelease(left, right) =>parallel(resistance_formula(left,state),resistance_formula(right,state))
      case RawFinally(op) =>resistance_formula(op,state)
      case RawGlobally(op) =>resistance_formula(op,state)
    }
  }

  private def parallel(left: Double, right: Double): Double = {
    (left*right)/(left+right)
  }

  private def normalize(x:Double) : Double = {
    val num : Double = x-rmin
    val den : Double = rmax-rmin
    val r = num/den
    r
  }

  private def normalize_ready(x:Double) : Double = {
    val norm = normalize(x)
    0.5 + (norm/2)
  }
  private def normalize_commit(x:Double) : Double = {
    val norm = normalize(x)
    norm/2
  }

}