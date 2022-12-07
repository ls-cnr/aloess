package org.icar.solver

import org.icar.partial_sat.EffortToSatisf
import org.icar.subsymbolic.builder.{ActionBuilder, RETEBuilder, SubLogicBuilder}
import org.icar.symbolic._

abstract class AbstractSolver(onto:DomainOntology, abstract_repo : List[AbstractCapability], goal_model : GoalTree) {
  val sub_logic = new SubLogicBuilder(onto)
  val rete_builder = new RETEBuilder(sub_logic,onto.axioms)
  val sub_actions = new ActionBuilder(sub_logic,abstract_repo)
  val effort_to_sat = new EffortToSatisf(goal_model)
  val goal_state_builder = new GoalMapBuilder(sub_logic,goal_model,effort_to_sat)
  val goal_map_merger = new GoalMapMerger(goal_model.root, effort_to_sat)

  var goal_model_state : GoalModelMap = GoalModelMap(goal_model.root.id,Map.empty)     // => goal state dovrebbe essere una specie di "memory" relativa al goal model

  init

  def run(start:StateOfWorld,termination:TerminationCondition): SolverOutcome

  private def init : Unit = {
    goal_model_state = goal_state_builder.create_goalmodel_map
  }

}
