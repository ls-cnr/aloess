package org.icar.grounder.process_decorator

import org.icar.business_process.{ConcreteWorkflow, ConcreteWFFlowDef, ConcreteWFItemDef, WFEvent, EventType, FlowableErrorEventDefinition, SequenceFlow, FlowableServiceTask}

/**
 * Decorate all service task with a boundary event error that goes to a service task. This last refers to a java/scala
 * class that call MUSA for re-adapting the workflow (following a failure during the execution of a task).
 */
class BoundaryErrorEventDecorator(boundaryErrorEventErrCode : String, readaptationClassName : String) extends ProcessDecorator {

  //val boundaryErrorEventErrCode = "REQUIRE_ORCHESTRATION"
  //val readaptationClassName = "nettunit.handler.adaptation.MUSAOrchestrationHandler"
  val adaptationRequestTask = FlowableServiceTask("adaptationTask", "adaptationTask", readaptationClassName, None)
  val adaptationEndEvent = WFEvent("failureEndEvent", "failureEndEvent", EventType.End.toString, null)
  val adaptationTaskSeqFlow = SequenceFlow(s"adaptationTaskFlowEnd", adaptationRequestTask, adaptationEndEvent, None)



  override def apply(wf: ConcreteWorkflow): ConcreteWorkflow = {
    //First, filter the items so to avoid, for example, that the re-organization is decorated
    val filteredItems = filterItemsToDecorate(wf.items.toList)
    val filteredItemsWithFailure = containsFailureItems(filteredItems)
    val decoratedItems = filteredItemsWithFailure match {
      case true => decorateItems(filteredItems) //Add only the boundary events to filtered items
      case false => decorateItems(filteredItems) ++ List(adaptationRequestTask, adaptationEndEvent) //Add boundary
      //events and the end event and the re-organization task
    }

    val seqFlows = containsFailureFlows(wf.flows.toList) match {
      case true => decorateSequenceFlows(decoratedItems) ++ wf.flows.toList
      case false => decorateSequenceFlows(decoratedItems) ++ wf.flows.toList ++ List(adaptationTaskSeqFlow)
    }

    // return the decorated workflow
    wf.copy(wf.datatypes, decoratedItems.toArray, seqFlows.toArray, wf.data)
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
        val ev = WFEvent(s"boundaryError_${head.id}",
          s"boundaryError_${head.label}",
          EventType.Boundary.toString,
          FlowableErrorEventDefinition(head.id, boundaryErrorEventErrCode))
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
        val seqFlow = SequenceFlow(s"flow_boundaryError_${ev.id}", ev, adaptationRequestTask, None)
        seqFlow :: decorateSequenceFlowsAux(tail, itemID + 1)
      }
      case (_: ConcreteWFItemDef) :: tail => decorateSequenceFlowsAux(tail, itemID + 1)
      case Nil => List()
    }

    decorateSequenceFlowsAux(items, 0)
  }

  /**
   * Filter the element to decorate. Ignore:
   * - adaptation task
   * - sequence flows to adaptation task
   * - boundary/timer events
   *
   * @param items
   * @return
   */
  private def filterItemsToDecorate(items: List[ConcreteWFItemDef]): List[ConcreteWFItemDef] = {
    def aux(items: List[ConcreteWFItemDef]): List[ConcreteWFItemDef] = items match {
      case (head: FlowableServiceTask) :: tail if head.id == adaptationRequestTask.id => aux(tail)
      case (head: WFEvent) :: tail if head.id == adaptationEndEvent.id => aux(tail)
      case head :: tail => head :: aux(tail)
      case Nil => List()
    }

    aux(items)
  }

  /**
   *
   * @param flows
   * @return true if the list of flows contains a sequence flow going towards the adaptation task
   */
  private def containsFailureFlows(flows: List[ConcreteWFFlowDef]): Boolean = {
    def aux(flows: List[ConcreteWFFlowDef]): Boolean = flows match {
      case (head: SequenceFlow) :: tail if head.id == adaptationTaskSeqFlow.id => true || aux(tail)
      case _ :: tail => false || aux(tail)
      case Nil => false
    }

    aux(flows)
  }

  /**
   *
   * @param flows
   * @return true if the list of items contains the adaptation task
   */
  private def containsFailureItems(items: List[ConcreteWFItemDef]): Boolean = {
    def aux(items: List[ConcreteWFItemDef]): Boolean = items match {
      case (head: FlowableServiceTask) :: tail if head.id == adaptationRequestTask.id => true || aux(tail)
      case (head: WFEvent) :: tail if head.id == adaptationEndEvent.id => true || aux(tail)
      case _ :: tail => false || aux(tail)
      case Nil => false
    }

    aux(items)
  }

}
