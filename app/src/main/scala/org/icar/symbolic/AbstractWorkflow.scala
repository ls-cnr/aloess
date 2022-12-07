package org.icar.symbolic

case class AbstractWorkflow(name : String , items : List[AbstractWorkflowItem], flows : List[AbstractWorkflowFlows]) {

  def getSolutionTasks : List[CapabilityTask] = getWorkflowItems[CapabilityTask]()

  def getStartEvents : List[AbstractWorkflowItem] = getWorkflowItems[StartEvent]()

  def getEndEvents : List[AbstractWorkflowItem] = getWorkflowItems[EndEvent]()

  def getJoinGateways : List[JoinGateway] = getWorkflowItems[JoinGateway]()

  def getSplitGateways : List[SplitGateway] = getWorkflowItems[SplitGateway]()

  private def getWorkflowItems[T <: AbstractWorkflowItem : Manifest]() : List[T] = {
    def aux(wfItems: List[AbstractWorkflowItem]): List[T] = wfItems match {
      case (head: T) :: tail => head.asInstanceOf[T] :: aux(tail)
      case _ :: tail => aux(tail)
      case Nil => List()
    }

    aux(items)
  }

  def get_flows : List[AbstractSequenceFlow] = {
    for (f<-flows if f.isInstanceOf[AbstractSequenceFlow]) yield f.asInstanceOf[AbstractSequenceFlow]
  }

  def outgoing_flows(target : AbstractWorkflowItem) : List[AbstractSequenceFlow] = {
    var filtered: List[AbstractSequenceFlow] = List.empty
    for (f <- flows) {
      if (f.isInstanceOf[AbstractSequenceFlow]) {
        val s = f.asInstanceOf[AbstractSequenceFlow]
        if (s.from == target)
          filtered = s :: filtered
      }
    }
    filtered
  }

  def incoming_flows(target : AbstractWorkflowItem) : List[AbstractSequenceFlow] = {
    var filtered: List[AbstractSequenceFlow] = List.empty
    for (f <- flows) {
      if (f.isInstanceOf[AbstractSequenceFlow]) {
        val s = f.asInstanceOf[AbstractSequenceFlow]
        if (s.to == target)
          filtered = s :: filtered
      }
    }
    filtered
  }

  def successors(task: AbstractWorkflowItem): List[AbstractWorkflowItem] = {
    val out = outgoing_flows(task)
    val succs = for (o <- out) yield o.to
    succs
  }

  def predecessors(task: AbstractWorkflowItem): List[AbstractWorkflowItem] = {
    val in = incoming_flows(task)
    val preds = for (i <- in) yield i.from
    preds
  }

  def stringGraphviz() : String = {
    var string = s"digraph $name {\n"+"rankdir=LR\n"+"{rank=min; \"start\"}\n"+"{rank=max; \"end\"}\n"

    for (n <- items)
      string += "\""+print_item(n)+"\""+print_item_decoration(n)

    for (f <- flows if f.isInstanceOf[AbstractSequenceFlow]) {
      val flow = f.asInstanceOf[AbstractSequenceFlow]

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


abstract class AbstractWorkflowItem {
  def getStringID(): String
}
case class StartEvent(id: Int, name: String) extends AbstractWorkflowItem {
  override def getStringID(): String = s"startEv_${id}"
}
case class EndEvent(id: Int, name: String) extends AbstractWorkflowItem {
  override def getStringID(): String = s"endEv_${id}"
}
case class GenericEvent(id: Int, name: String) extends AbstractWorkflowItem {
  override def getStringID(): String = s"Ev_${id}"
}

case class CapabilityTask(id: Int, cap: AbstractCapability, assignments: List[CapabilityParameterEntry], boundaries : List[GenericEvent]) extends AbstractWorkflowItem {
  override def getStringID(): String = s"cp_${id}"
}
case class SubprocessTask(id: Int, subprocess: AbstractWorkflowItem, boundaries : List[GenericEvent]) extends AbstractWorkflowItem {
  override def getStringID(): String = s"sp_${id}"
}

case class SplitGateway(id: Int, ports: List[String]) extends AbstractWorkflowItem {
  override def getStringID(): String = s"split_${id}"
}
case class JoinGateway(id: Int) extends AbstractWorkflowItem {
  override def getStringID(): String = s"join_${id}"
}

abstract class AbstractWorkflowFlows
case class AbstractSequenceFlow(from: AbstractWorkflowItem, to: AbstractWorkflowItem, port: String = "", condition: LogicFormula with FOLNature  = True()) extends AbstractWorkflowFlows

