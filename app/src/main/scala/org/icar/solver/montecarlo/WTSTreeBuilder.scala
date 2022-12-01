package org.icar.solver.montecarlo

import org.icar.solver.GoalMapBuilder
import org.icar.subsymbolic.{RawAction, RawEvolution, RawState}
import org.icar.subsymbolic.builder.{ActionBuilder, RETEBuilder}
import org.icar.subsymbolic.rete.RETE

class WTSTreeBuilder(wi: RawState, rete_builder : RETEBuilder, action_builder : ActionBuilder, val goal_builder : GoalMapBuilder) {

  var generator_node_id : Int = 0
  var rete: RETE = rete_builder.rete

  val root : WTSTreeNode = init

  private def init : WTSTreeNode = {
    val initial_memory = rete.reset_memory(wi)
    val template_goalmap = goal_builder.create_goalmodel_map
    val initial_goalmap = goal_builder.update_goalmap(initial_memory.stable_state,template_goalmap)
    val updated_applicable_actions = for (a<-action_builder.actions if a.pre.satisfied_in(initial_memory.stable_state)) yield a
    new WTSTreeNode(get_new_node_id,null,initial_memory,initial_goalmap,updated_applicable_actions.toArray/*,new_r2s*/)
  }

  def get_node_child(node : WTSTreeNode, index:Int) : WTSTreeNode = {
    if (node.children(index).isDefined)
      node.children(index).get
    else {
      node.children(index) = Some(expand_node_by_action(node,index))
      node.children(index).get
    }
  }

  private def expand_node_by_action(node : WTSTreeNode, index: Int) : WTSTreeNode = {
    val action = node.legal_actions(index)

    val trajectory: RawEvolution = action.effects(0)    //we work with undeterministic actions

    val new_state = rete.evolution(node.state,trajectory.evo)
    val updated_map = goal_builder.update_goalmap(new_state.stable_state,node.supervisor)

    val updated_actions = node.legal_actions.filter( _!=action )
    val updated_applicable_actions : Array[RawAction] = for (a<-updated_actions if a.pre.satisfied_in(new_state.stable_state)) yield a
    val node_id = get_new_node_id
    //val new_r2s = my_tree.r2s

    new WTSTreeNode(node_id,node,new_state,updated_map,updated_applicable_actions/*,new_r2s*/)
  }

  private def get_new_node_id : Int = {
    val id = generator_node_id
    generator_node_id += 1
    id
  }
}
