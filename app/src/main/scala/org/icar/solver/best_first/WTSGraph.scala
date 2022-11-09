package org.icar.solver.best_first

import org.icar.solver.{DomainMetric, FullSatisfaction, GoalMapMerger, GoalModelMap, GoalState, PartialSatisfaction, Violation}
import org.icar.symbolic.GoalNode

import scala.collection.immutable.TreeSet

case class WTSGraph(
                     start : WTSNode,
                     nodes : List[WTSNode],
                     transitions : List[WTSTransition],

                     graph_label : GraphLabelling,
                     node_label : Map[Int,NodeLabelling],
                     tx_label : Map[Int,TxLabelling],

                     frontier : TreeSet[WTSNode],
                     visited : List[WTSNode]) {


  def stringGraphviz: String = {
    var string = "digraph WTS {\n"
    for (n <- nodes) {
      string += "\""+n.id+"\"[label=\""+n.memory.stable_state+"\"]\n"
    }

    for (t <- transitions) {
      string += "\""+t.origin+"\""
      string += "->"
      string += "\""+t.destination+"\""
      string += "[label=\""+tx_label(t.id).action_id+":"+tx_label(t.id).scenario_id+"\"];\n"
    }

    string + "}\n"
  }

  def is_full_solution : Boolean = {
    val root_goal_name = graph_label.wts_goals.root
    graph_label.wts_goals.map(root_goal_name).satisf == FullSatisfaction()
  }
  def most_promising_node : Option[WTSNode] = {
    if (frontier.nonEmpty)
      Some(frontier.head)
    else
      None
  }
  def is_interested_to(node: WTSNode): Boolean = {
    val not_complete = !is_full_solution
    var node_in_frontier = false
    for (f <- frontier) if (f.id==node.id) node_in_frontier=true
    not_complete && node_in_frontier
  }


  /**
   * apply all the expansions to the same wts thus to generate its updated version
   * each expansion adds
   * 1) a new frontier node
   * 2) a new transition from the focus node to the new node
   * 3) labels for node and trransition
   * note: the whole graph labeling is changed consequently to
   * 1) update the frontier data structure
   * 2) check if the wts represents a full solution
   */
  def expand_wts(focusnode:WTSNode, exp_list: List[DecodeExpansion], goal_map_merger : GoalMapMerger, metric : DomainMetric) : WTSGraph = {
    var nodes : List[WTSNode] = this.nodes
    var transitions : List[WTSTransition] = this.transitions
    var node_labels : Map[Int,NodeLabelling] = this.node_label
    var tx_labels : Map[Int,TxLabelling] = this.tx_label
    var frontier : TreeSet[WTSNode] = this.frontier
    var visited : List[WTSNode] = this.visited

    var wts_goal_map : Map[String,GoalState] = this.graph_label.wts_goals.map

    for (exp <- exp_list) {
      nodes = exp.node :: nodes
      transitions = exp.tx :: transitions
      node_labels = node_labels + (exp.node.id -> exp.nodelabel)
      val node_goal_state = exp.nodelabel.node_goals.map

      wts_goal_map = goal_map_merger.merge_maps(wts_goal_map,node_goal_state)

      tx_labels = tx_labels + (exp.tx.id -> exp.txlabel)
      frontier = frontier - focusnode + exp.node
      visited = focusnode :: visited
    }

    val quality_sol: Double = calculate_quality_of_solution(wts_goal_map,metric)
    val graph_label = GraphLabelling(GoalModelMap( this.graph_label.wts_goals.root ,wts_goal_map),quality_sol)
    WTSGraph(start,nodes,transitions,graph_label,node_labels,tx_labels,frontier,visited)
  }

  def calculate_quality_of_solution(goal_map: Map[String, GoalState], metric : DomainMetric): Double = {
    val root_state = goal_map(this.graph_label.wts_goals.root)
    root_state.satisf match {
      case FullSatisfaction() => metric.max
      case PartialSatisfaction(degree) => degree
      case Violation() => metric.min
      case _ => metric.min
    }
  }

}

object WTSGraph {

  def start_graph(start_node:WTSNode, init_goal_map : GoalModelMap) : WTSGraph = {
    val label = NodeLabelling(init_goal_map, List.empty)
    WTSGraph(
      start_node,
      List(start_node),
      List.empty,
      GraphLabelling(init_goal_map,0),
      Map(0->label),
      Map.empty,
      TreeSet(start_node),List.empty
    )
  }
}

