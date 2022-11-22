package org.icar.solver.best_first

import org.icar.solver._

case class WTSGraph(
                     start : WTSNode,
                     nodes : List[WTSNode],
                     transitions : List[WTSTransition],

                     graph_label : GraphLabelling,
                     node_label : Map[Int,NodeLabelling],
                     tx_label : Map[Int,TxLabelling],

                     frontier : List[WTSNode],
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
    graph_label.wts_goals.map(root_goal_name).sat_state == FullSatisfaction()
    //val all_leaf_node_have_not_triggers
  }
  def most_promising_node(opt_metric : Option[DomainMetric]) : Option[WTSNode] = {
    if (opt_metric.isDefined)
      most_promising_node_according_score
    else
      most_promising_node_according_sat_degree
  }
  def most_promising_node_according_score : Option[WTSNode] = {
    var result : Option[WTSNode] = None
    var score : Double = 0
    for (f <- frontier)
      if (f.score > score) {
        score = f.score
        result = Some(f)
      }
    result
  }
  def most_promising_node_according_sat_degree : Option[WTSNode] = {
    var result : Option[WTSNode] = None
    var degree : Double = 0
    for (f <- frontier) {
      val label = node_label(f.id)
      val root_state = label.node_goals.get_root_state
      if (root_state.sat_state.isInstanceOf[PartialSatisfaction]) {
        val node_degree = root_state.sat_degree
        if (node_degree > degree) {
          result = Some(f)
          degree = node_degree
        }
      }
    }
    result
  }
  def is_interested_to(node: WTSNode): Boolean = {
    val not_complete = !is_full_solution
    val node_in_frontier = frontier.contains(node)
    //for (f <- frontier) if (f.id==node.id) node_in_frontier=true
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
  def expand_wts(focusnode:WTSNode, exp_list: List[DecodeExpansion], goal_map_merger : GoalMapMerger, opt_metric : Option[DomainMetric]) : WTSGraph = {
    var up_nodes : List[WTSNode] = this.nodes
    var up_transitions : List[WTSTransition] = this.transitions
    var up_node_labels : Map[Int,NodeLabelling] = this.node_label
    var up_tx_labels : Map[Int,TxLabelling] = this.tx_label
    var up_frontier : List[WTSNode] = this.frontier.filter(x => x.id != focusnode.id)
    var up_visited : List[WTSNode] = focusnode :: this.visited

    var up_wts_goal_map : Map[String,GoalState] = this.graph_label.wts_goals.map

    for (exp <- exp_list) {
      if (!up_nodes.contains(exp.node)) {
        up_nodes = exp.node :: up_nodes
        up_node_labels = up_node_labels + (exp.node.id -> exp.nodelabel)
      }
      up_transitions = exp.tx :: up_transitions
      up_tx_labels = up_tx_labels + (exp.tx.id -> exp.txlabel)

      val node_goal_state = exp.nodelabel.node_goals.map
      up_wts_goal_map = goal_map_merger.merge_maps(up_wts_goal_map,node_goal_state)

      if (!up_visited.contains(exp.node))
        up_frontier = exp.node :: up_frontier
    }

    val up_quality_sol: Double = calculate_quality_of_solution(up_wts_goal_map)
    val up_graph_label = GraphLabelling(GoalModelMap( this.graph_label.wts_goals.root ,up_wts_goal_map),up_quality_sol)

    WTSGraph(start,up_nodes,up_transitions,up_graph_label,up_node_labels,up_tx_labels,up_frontier,up_visited)
  }

  def wts_without_node_in_frontier(focusnode:WTSNode) : WTSGraph = {
    val up_frontier : List[WTSNode] = this.frontier.filter(x => x.id != focusnode.id)
    val up_visited : List[WTSNode] = focusnode :: this.visited
    WTSGraph(start,nodes,transitions,graph_label,node_label,tx_label,up_frontier,up_visited)
  }

  def calculate_quality_of_solution(goal_map: Map[String, GoalState]): Double = {
    val root_state = goal_map(this.graph_label.wts_goals.root)
    root_state.sat_degree
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
      List(start_node),
      List.empty
    )
  }
}

