package org.icar.solver.montecarlo

import org.icar.solver.{FullSatisfaction, FullSolutions, GoalModelMap}
import org.icar.subsymbolic.{RawAction, RawEvolution, RawState}
import org.icar.subsymbolic.rete.Memory

//todo: rename state --> memory, supervisor --> node_goals
//todo: BestFirstSolver: mettere una unica RETE globale a livello di WTSGraph, come qui, potrebbe ridurre l'occupazione di memoria
class WTSTreeNode(val id:Int, val parent:WTSTreeNode, val state : Memory, val supervisor : GoalModelMap, val legal_actions : Array[RawAction]/*,val r2s : Double*/) {
  val children : Array[Option[WTSTreeNode]] = Array.fill(legal_actions.length)(None)
  var visit : Int = 0     // number of visit received by the search algorithm
  var win : Double = 0    // score obtained by the various visits
  var improve_goal_satisfaction : Boolean = false

  def is_root : Boolean = (parent==null)
  def is_exit : Boolean = supervisor.get_root_state.is_fully_sat
  def is_nonterminal: Boolean = !legal_actions.isEmpty && !is_exit
  def is_notfullyexpanded: Boolean = children.contains(None)
  def untried_actions: Array[Int] = {
    val untried = for (index <- 0 until legal_actions.length if !children(index).isDefined) yield index
    untried.toArray
  }
  def get_actionID_to(node: WTSTreeNode): Int = {
    var index = -1
    for (i <- 0 until children.size if children(i).isDefined) {
      val child = children(i).get
      if (child == node)
        index = i
    }
    if (index != -1)
      legal_actions(index).id
    else
      -1
  }



  def back_sol_stringGraphviz() : String = {
    var string = "digraph solution {\n"
    string += back_to_graphviz_body(parent,this)
    string += "}\n"

    string
  }

  private def back_to_graphviz_body(back : WTSTreeNode,node : WTSTreeNode) : String = {
    var string = s"N${node.id};\n"
    if (back != null) {
      string += s"N${back.id} -> N${node.id}; \n"
      string += back_to_graphviz_body(back.parent,back)
    }
    string
  }


  def stringGraphviz() : String = {
    var string = "digraph WTSTree {\n"
    string += to_graphviz_body()
    string += "}\n"

    string
  }

  private def to_graphviz_body() : String = {
    var string = "N"+id
    val label = "\""+win+"/"+visit+"("+supervisor.get_root_state.sat_degree+")\""
    if (is_exit)
      string += s"[label=$label,style=bold,color=green];\n"
    else if (improve_goal_satisfaction==true)
      string += s"[label=$label,style=bold,color=blue];\n"
    else
      string += s"[label=$label,color=black];\n"

    for (index<-0 until children.length if children(index).isDefined) {
      val mychild : WTSTreeNode = children(index).get
      val child_state: RawState = mychild.state.stable_state

      val act = legal_actions(index)
      string += s"N$id -> N${mychild.id} [label=\"${act.id}\"]; \n"

      string += mychild.to_graphviz_body()
    }

    string
  }

}
