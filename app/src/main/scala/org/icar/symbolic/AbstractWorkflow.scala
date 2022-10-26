package org.icar.symbolic

case class AbstractWorkflow(items : List[AbstractWorkflowItem], flows : List[AbstractWorkflowFlows]) {
  def outgoing_flows(target : AbstractWorkflowItem) : List[SequenceFlow] = {
    var filtered: List[SequenceFlow] = List.empty
    for (f <- flows) {
      if (f.isInstanceOf[SequenceFlow]) {
        val s = f.asInstanceOf[SequenceFlow]
        if (s.from == target)
          filtered = s :: filtered
      }
    }
    filtered
  }

  def incoming_flows(target : AbstractWorkflowItem) : List[SequenceFlow] = {
    var filtered: List[SequenceFlow] = List.empty
    for (f <- flows) {
      if (f.isInstanceOf[SequenceFlow]) {
        val s = f.asInstanceOf[SequenceFlow]
        if (s.to == target)
          filtered = s :: filtered
      }
    }
    filtered
  }
}


abstract class AbstractWorkflowItem
case class StartEvent(id: Int, name: String) extends AbstractWorkflowItem
case class EndEvent(id: Int, name: String) extends AbstractWorkflowItem
case class GenericEvent(id: Int, name: String) extends AbstractWorkflowItem

case class CapabilityTask(id: Int, cap: AbstractCapability, assignments: Map[String, ConstantTerm], boundaries : List[GenericEvent]) extends AbstractWorkflowItem
case class SubprocessTask(id: Int, subprocess: AbstractWorkflowItem, boundaries : List[GenericEvent]) extends AbstractWorkflowItem

case class SplitGateway(id: Int, ports: List[String]) extends AbstractWorkflowItem
case class JoinGateway(id: Int) extends AbstractWorkflowItem

abstract class AbstractWorkflowFlows
case class SequenceFlow(from: AbstractWorkflowItem, to: AbstractWorkflowItem, port: String = "", condition: LogicFormula with FOLNature  = True()) extends AbstractWorkflowFlows
