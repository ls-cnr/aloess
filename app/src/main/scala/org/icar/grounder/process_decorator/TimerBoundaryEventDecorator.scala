package org.icar.grounder.process_decorator

import org.icar.business_process.{ConcreteWorkflow, ConcreteWFFlowDef, ConcreteWFItemDef, WFEvent, EventType, FlowableTimerEventDefinition, SequenceFlow, FlowableServiceTask}

class TimerBoundaryEventDecorator(readaptationClassName : String) extends ProcessDecorator {

  val adaptationRequestTask = FlowableServiceTask("timer_adaptationTask", "timer_adaptationTask", readaptationClassName, None)
  val dummyTimeCondition = "PT5S"

  override def apply(wf: ConcreteWorkflow): ConcreteWorkflow = {
    wf
  }

  /**
   * Decorate all ServiceTask with a BoundaryEvent
   *
   * @param items the grounded workflow items
   * @param itemID
   * @return
   */
  def decorateItems(items: List[ConcreteWFItemDef]): List[ConcreteWFItemDef] = {
    def decorateItemsAux(items: List[ConcreteWFItemDef], itemID: Int): List[ConcreteWFItemDef] = items match {
      case (head: FlowableServiceTask) :: tail => {
        val ev = WFEvent(s"boundaryTimer_${head.id}",
          s"boundaryTimer_${head.label}",
          EventType.Boundary.toString,
          FlowableTimerEventDefinition(head.id, dummyTimeCondition))
        head :: ev :: decorateItemsAux(tail, itemID + 1)
      }
      case (head: ConcreteWFItemDef) :: tail => head :: decorateItemsAux(tail, itemID + 1)
      case Nil => List()
    }

    decorateItemsAux(items, 0)
  }

  /**
   *
   * @param items
   * @param itemID
   * @return
   */
  def decorateSequenceFlows(items: List[ConcreteWFItemDef]): List[ConcreteWFFlowDef] = {
    def decorateSequenceFlowsAux(items: List[ConcreteWFItemDef], itemID: Int): List[ConcreteWFFlowDef] = items match {
      case (ev: WFEvent) :: tail if ev.eventtype == EventType.Boundary.toString => {
        val seqFlow = SequenceFlow(s"flow_boundaryTimer_${ev.id}", ev, adaptationRequestTask, None)
        seqFlow :: decorateSequenceFlowsAux(tail, itemID + 1)
      }
      case (_: ConcreteWFItemDef) :: tail => decorateSequenceFlowsAux(tail, itemID + 1)
      case Nil => List()
    }

    decorateSequenceFlowsAux(items, 0)
  }

}
