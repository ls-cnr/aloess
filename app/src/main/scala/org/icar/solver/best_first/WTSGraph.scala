package org.icar.solver.best_first

import org.icar.solver.{FullSatisfaction, GoalModelMap}
import org.icar.subsymbolic.RawLogicFormula
import org.icar.subsymbolic.rete.Memory
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
    val root = graph_label.root_goal
    graph_label.wts_goals.map(root.id).satisf == FullSatisfaction()
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

}

object WTSGraph {
  def start_graph(start_node:WTSNode, root_goal : GoalNode, init_goal_map : GoalModelMap) : WTSGraph = {
    val label = NodeLabelling(init_goal_map, true, List.empty)
    WTSGraph(
      start_node,
      List(start_node),
      List.empty,
      GraphLabelling(root_goal,init_goal_map,0),
      Map(0->label),
      Map.empty,
      TreeSet(start_node),List.empty
    )
  }
}


case class WTSNode(id:Int, memory:Memory, score:Double) extends Ordered[WTSNode] {
  //score is the quality of the node (higher is better)
  override def compare(that: WTSNode): Int = that.score compare this.score

//  override def equals(obj: Any): Boolean = {
//    obj match {
//      case WTSNode(node_id,_,_) => if (id ==node_id) true else false
//      case _ => false
//    }
//  }
}

case class WTSTransition(id:Int, origin : Int, destination : Int)



case class GraphLabelling(
                         root_goal : GoalNode,
                         wts_goals : GoalModelMap,
                         //full_solution : Boolean,
                         quality_of_solution : Double     // global quality of the (partial) solution
                       )
case class NodeLabelling(
                          node_goals : GoalModelMap,
                          is_frontier : Boolean, // the node is yet to be expanded
                          //                          is_terminal: Boolean, // a node is terminal it has been focused but it has not outgoing arcs
                          invariants : List[RawLogicFormula], // conditions that must hold in any new node
                        )
case class TxLabelling(action_id: Int,scenario_id : String)







object runTreeSetOnNode extends App {
  var frontier : TreeSet[WTSNode] =TreeSet.empty
  val node1 = WTSNode(1,null,10)
  val node2 = WTSNode(2,null,5)
  val node3 = WTSNode(3,null,25)
  val node4 = WTSNode(4,null,15)
  frontier = frontier+node1
  frontier = frontier+node2
  frontier = frontier+node3
  frontier = frontier+node4
  println(frontier)
}