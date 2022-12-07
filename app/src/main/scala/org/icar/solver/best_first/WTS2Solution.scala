package org.icar.solver.best_first

import org.icar.symbolic._

class WTS2Solution(wts:WTSGraph, action_register : Map[Int,CapabilityEntry], I : StateOfWorld, patterns:List[MetaGoal]=List.empty) {
  val startEventID = 0

  var wfitems: Set[AbstractWorkflowItem] = Set(StartEvent(startEventID, s"start_${startEventID}"))
  var wfflow: List[AbstractSequenceFlow] = List.empty
  var node_to_item_map:Map[WTSNode,AbstractWorkflowItem] = Map.empty

  var task_id=1; var split_id=1; var join_id=1; var end_id = 1

  def build_abstract : AbstractWorkflow = {
    wfitems = Set(StartEvent(startEventID, s"start_${startEventID}"))
    wfflow = List.empty
    node_to_item_map = Map.empty

    visit_node(wts.start, StartEvent(startEventID, s"start_${startEventID}"))
    for (pattern <- patterns) {
      apply_pattern(pattern)
    }
    AbstractWorkflow(wts.name,wfitems.toList,wfflow)
  }

  private def visit_node(wts_node: WTSNode, connect_from : AbstractWorkflowItem, scenario:String="") : Unit = {
    if (!node_to_item_map.contains(wts_node)) {
      val outgoing = wts.transitions.filter(_.origin == wts_node.id)
      if (outgoing.size == 0) {
        // last element
        val end_element = addEnd()
        node_to_item_map += (wts_node -> end_element)
        addSequenceFlow(connect_from,end_element,scenario)

      } else if (outgoing.size == 1) {
        // standard task
        val tx = outgoing.head
        val item = visit_transition(tx)
        val ref_item = node_to_item_map(wts_node)
        addSequenceFlow(connect_from, ref_item,scenario)

      } else {
        // task with many outcomes
        val item = visit_XOR(outgoing.toSet)
        val ref_item = node_to_item_map(wts_node)
        addSequenceFlow(connect_from, ref_item,scenario)
      }

    } else {
      // loop
      val item = addMergeGateway

      addSequenceFlow(connect_from,item,scenario)
      val arrival_item = node_to_item_map(wts_node)

      node_to_item_map -= (wts_node)
      node_to_item_map += (wts_node -> item)
      addSequenceFlow(item,arrival_item)
    }
  }

  private def visit_transition(arc: WTSTransition) : AbstractWorkflowItem = {
    val label = wts.tx_label(arc.id)
    val src_cap_entry = action_register(label.action_id)
    val item = addTask(src_cap_entry)
    val src_node :WTSNode = wts.get_node_by_ID(arc.origin).get
    node_to_item_map += (src_node -> item)
    val dst_node :WTSNode = wts.get_node_by_ID(arc.destination).get
    visit_node(dst_node,item)
    item
  }

  private def visit_XOR(arcs: Set[WTSTransition]) : AbstractWorkflowItem = {
    val source : Int = arcs.head.origin
    val src_cap_entry = action_register(source)
    val outports = for (t<-arcs.toList) yield wts.tx_label(t.id).scenario_id

    val task_item = addTask(src_cap_entry)
    val src_node :WTSNode = wts.get_node_by_ID(source).get
    node_to_item_map += (src_node -> task_item)

    val split_item = addSplitGateway(outports)
    addSequenceFlow(task_item,split_item)

    for (tx <- arcs) {
      val dst_node :WTSNode = wts.get_node_by_ID(tx.destination).get
      val scenario = wts.tx_label(tx.id).scenario_id
      visit_node(dst_node,split_item,scenario)
    }

    task_item
  }

  private def addTask(grounding : CapabilityEntry) : CapabilityTask = {
    val task = CapabilityTask(task_id,grounding.cap,grounding.pars, List.empty)
    task_id += 1
    wfitems = wfitems+task
    task
  }

  private def addEnd() : EndEvent = {
    val end = EndEvent(end_id, s"endEvent_${end_id}")
    end_id += 1
    wfitems = wfitems+end
    end
  }

  private def addSplitGateway(outport:List[String]) : SplitGateway = {
    val gw = SplitGateway(split_id,outport)
    split_id += 1
    wfitems = wfitems+gw
    gw
  }

  private def addMergeGateway : JoinGateway = {
    val gw = JoinGateway(join_id)
    join_id += 1
    wfitems = wfitems+gw
    gw
  }

  private def addSequenceFlow(from:AbstractWorkflowItem,to:AbstractWorkflowItem,scenario:String="",condition:LogicFormula with FOLNature =True()) : Unit = {
    if (!wfflow.contains(AbstractSequenceFlow(from,to,scenario,condition))) {
      wfflow = AbstractSequenceFlow(from,to,scenario,condition) :: wfflow
    }
  }


  private def apply_pattern(pattern: MetaGoal) : Unit = {
    pattern match {
      case p:RepeatUntil =>
        apply_structured_loop(p.target, p.condition)
    }
  }

  private def apply_structured_loop(goal_id: String, condition: LogicFormula with FOLNature): Unit = {
    val applicability = check_structured_loop_applicability(goal_id)

    if (applicability) {
      for (node<-node_to_item_map.keys) {
        val node_goal = wts.node_label(node.id).node_goals
        val target_goal_state = node_goal.map(goal_id)
        if (target_goal_state.switch_to_committ) {
          val item_node = node_to_item_map(node)

          // put join
          val pattern_join = addMergeGateway

          // put flow from node_prec to join
          val flowfrom_precs = wfflow.filter(_.to==item_node)
          for (i<-flowfrom_precs) {
            addSequenceFlow(i.from, pattern_join)
          }
          // separate node from node_prec
          wfflow = wfflow.filter(_.to!=item_node)

          // put flow from join to node
          addSequenceFlow(pattern_join,item_node)

          //visit all path until exit
          val arriving_nodes = visit_path_until_exit(node,goal_id)

          //for each arriving_node
          for (arr_node <- arriving_nodes) {
            val item_arriving_node = node_to_item_map(arr_node)

            //put split
            val pattern_split = addSplitGateway(List(condition.toString,"otherwise"))

            //put flow from split to join with specified condition
            addSequenceFlow(pattern_split, pattern_join, condition.toString)

            //put flow from join to arriving_node.prec
            val flowto_prec = wfflow.filter(_.to==item_arriving_node)
            for (f<-flowto_prec) {
              addSequenceFlow(f.from, pattern_split)
            }
            //remove flow from arriving_node to arriving_node.succ
            wfflow = wfflow.filter(_.to!=item_arriving_node)

            //put flow from arriving_node to split
            addSequenceFlow(pattern_split, item_arriving_node, "otherwise")

          }
        }
      }
    }
  }
  private def check_structured_loop_applicability(goal_name: String):Boolean = {
    //check at least 1 trigger && at least 1 exit
    var check_trigger = false
    var check_exit = false
    for (node<-node_to_item_map.keys) {
      val node_goal = wts.node_label(node.id).node_goals
      val target_goal_state = node_goal.map(goal_name)
      if (target_goal_state.switch_to_committ)
        check_trigger = true
      if (target_goal_state.switch_to_satisf)
        check_exit = true
    }

    check_trigger && check_exit
  }

  private def visit_path_until_exit(node: WTSNode, goal_name: String): List[WTSNode] = {
    val node_goal = wts.node_label(node.id).node_goals
    val target_goal_state = node_goal.map(goal_name)
    if (target_goal_state.switch_to_satisf)
      List(node)
    else {
      val item = node_to_item_map(node)
      if (item.isInstanceOf[EndEvent])
        List()
      else {
        val outgoing_flows = wfflow.filter(_.from==item)
        var list : List[WTSNode] = List.empty
        for (f <- outgoing_flows) {
          val dest = f.to
          val dest_state = inverse_map(dest)
          list = visit_path_until_exit(dest_state,goal_name) ::: list
        }
        list
      }
    }
  }

  private def inverse_map(item: AbstractWorkflowItem):WTSNode = {
    node_to_item_map.filter(_._2 == item).head._1
  }

}
