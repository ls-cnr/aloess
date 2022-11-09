package org.icar.solver.best_first

import org.icar.solver.DomainMetric
import org.icar.subsymbolic.RawState
import org.icar.subsymbolic.rete.Memory

/**
 * this class ensure ids for nodes are assigned with a global scope
 * every wts has the same id for the same name
 */
class GlobalNodeBuilder(metric : DomainMetric) {
  var node_ids : Map[RawState,Int] = Map.empty
  var scores : Map[Int, Double] = Map.empty
  def get_or_create_node(memory:Memory) : WTSNode = {
    if (!node_ids.contains(memory.stable_state)) {
      val score = metric.evaluate_state(memory.stable_state)
      val node = WTSNode(node_ids.size,memory,score)
      node_ids = node_ids + (memory.stable_state -> node.id)
      scores = scores + (node.id -> score)
      node
    } else {
      val id = node_ids(memory.stable_state)
      val score = scores(id)
      WTSNode(id,memory,score)
    }
  }
}
