package org.icar.solver.best_first

import org.icar.solver.GoalModelMap
import org.icar.subsymbolic.RawLogicFormula
import org.icar.subsymbolic.rete.Memory

import scala.collection.immutable.TreeSet

case class WTSNode(id:Int, memory:Memory, score:Double) extends Ordered[WTSNode] {
  //score is the quality of the node (higher is better)
  // please, note: node equality must be done by explicitly comparing their ids
  override def compare(that: WTSNode): Int = that.score compare this.score

  override def toString: String = s"node $id score $score [${memory.stable_state}]"
}

case class WTSTransition(id:Int, name : String, origin : Int, destination : Int)



case class GraphLabelling(
                           //root_goal : GoalNode,    //todo remove this
                           wts_goals : GoalModelMap,
                           quality_of_solution : Double     // global quality of the (partial) solution
                         )
case class NodeLabelling(
                          node_goals : GoalModelMap,
                          //is_frontier : Boolean, //todo remove this
                          invariants : List[RawLogicFormula], // conditions that must hold in any new node
                        )
case class TxLabelling(action_id: Int,scenario_id : String)







object runTreeSetOnNode extends App {
  var frontier: TreeSet[WTSNode] = TreeSet.empty
  val node1 = WTSNode(1, null, 10)
  val node2 = WTSNode(2, null, 5)
  val node3 = WTSNode(3, null, 25)
  val node4 = WTSNode(4, null, 15)
  frontier = frontier + node1
  frontier = frontier + node2
  frontier = frontier + node3
  frontier = frontier + node4
  println(frontier)
}

