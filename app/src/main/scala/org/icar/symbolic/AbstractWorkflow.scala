package org.icar.symbolic

case class AbstractWorkflow(name : String , items : List[AbstractWorkflowItem], flows : List[AbstractWorkflowFlows]) {
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


  def stringGraphviz() : String = {
    var string = s"digraph $name {\n"+"rankdir=LR\n"+"{rank=min; \"start\"}\n"+"{rank=max; \"end\"}\n"

    for (n <- items)
      string += "\""+print_item(n)+"\""+print_item_decoration(n)

    for (f <- flows if f.isInstanceOf[SequenceFlow]) {
      val flow = f.asInstanceOf[SequenceFlow]

      string += "\""+print_item(flow.from)+"\""
      string += "->"
      string += "\""+print_item(flow.to)+"\""
      string += "[label=\""+flow.port+"\"];\n"
    }
    string + "}\n"
  }


  private def print_item(n: AbstractWorkflowItem): String = {
    n match {
      case StartEvent(_,_) => "start"
      case EndEvent(_,_) => "end"
      case CapabilityTask(id, cap,assignments,boundaries) =>
        val entry = CapabilityEntry(cap,assignments)
        s"${id}_${entry.toString}"
      case JoinGateway(id) => s"J$id"
      case SplitGateway(id, outport) => s"S$id"
    }
  }
  private def print_item_decoration(n: AbstractWorkflowItem): String = {
    n match {
      case StartEvent(_,_) => "[shape=doublecircle,color=black];\n"
      case EndEvent(_,_) => "[shape=doublecircle,color=green];\n"
      case CapabilityTask(_, _,_,_) => "[shape=box,color=black];\n"
      case JoinGateway(_) => "[shape=diamond,color=black];\n"
      case SplitGateway(_, _) => "[shape=diamond,color=black];\n"
    }
  }

}


abstract class AbstractWorkflowItem
case class StartEvent(id: Int, name: String) extends AbstractWorkflowItem
case class EndEvent(id: Int, name: String) extends AbstractWorkflowItem
case class GenericEvent(id: Int, name: String) extends AbstractWorkflowItem

case class CapabilityTask(id: Int, cap: AbstractCapability, assignments: List[CapabilityParameterEntry], boundaries : List[GenericEvent]) extends AbstractWorkflowItem
case class SubprocessTask(id: Int, subprocess: AbstractWorkflowItem, boundaries : List[GenericEvent]) extends AbstractWorkflowItem

case class SplitGateway(id: Int, ports: List[String]) extends AbstractWorkflowItem
case class JoinGateway(id: Int) extends AbstractWorkflowItem

abstract class AbstractWorkflowFlows
case class SequenceFlow(from: AbstractWorkflowItem, to: AbstractWorkflowItem, port: String = "", condition: LogicFormula with FOLNature  = True()) extends AbstractWorkflowFlows
