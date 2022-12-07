package org.icar.solver.montecarlo

import org.icar.symbolic._

class WTSTree2Solution(action_register : Map[Int,CapabilityEntry]) {
  var task_id=1

  val end_event = EndEvent(0, "unique_start")
  val start_event = StartEvent(1, "unique_end")

  var wfitems: Set[AbstractWorkflowItem] = Set(start_event,end_event)
  var wfflow: List[AbstractSequenceFlow] = List.empty

  def build_abstract(leaf_node : WTSTreeNode) : AbstractWorkflow = {
    wfitems = Set(start_event,end_event)
    wfflow = List.empty
   visit_node(leaf_node, end_event)

    AbstractWorkflow(leaf_node.id.toString,wfitems.toList,wfflow)
  }

  private def visit_node(node: WTSTreeNode, connect_to : AbstractWorkflowItem) : Unit = {
    val pre_node = node.parent
    if (pre_node != null) {
      val action_id = pre_node.get_actionID_to(node)
      val cap_entry :CapabilityEntry = action_register(action_id)
      val item = addTask(cap_entry)
      addSequenceFlow(item,connect_to)
      visit_node(pre_node, item)
    } else {
      addSequenceFlow(start_event,connect_to)
    }
  }

  private def addTask(grounding : CapabilityEntry) : CapabilityTask = {
    val task = CapabilityTask(task_id,grounding.cap,grounding.pars, List.empty)
    task_id += 1
    wfitems = wfitems+task
    task
  }

  private def addSequenceFlow(from:AbstractWorkflowItem,to:AbstractWorkflowItem,scenario:String="",condition:LogicFormula with FOLNature =True()) : Unit = {
    if (!wfflow.contains(AbstractSequenceFlow(from,to,scenario,condition))) {
      wfflow = AbstractSequenceFlow(from,to,scenario,condition) :: wfflow
    }
  }

}
