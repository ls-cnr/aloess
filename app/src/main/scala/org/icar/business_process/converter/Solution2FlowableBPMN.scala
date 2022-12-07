package org.icar.business_process.converter

import org.icar.business_process._

import scala.xml.Elem

class Solution2FlowableBPMN(process: ConcreteWorkflow) {

  def getBPMN(processName: String, processID: String): Elem = {
    <definitions xmlns:flowable="http://flowable.org/bpmn" xmlns="http://www.omg.org/spec/BPMN/20100524/MODEL"
                 xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema"
                 xmlns:activiti="http://activiti.org/bpmn" xmlns:bpmndi="http://www.omg.org/spec/BPMN/20100524/DI"
                 xmlns:omgdc="http://www.omg.org/spec/DD/20100524/DC" xmlns:omgdi="http://www.omg.org/spec/DD/20100524/DI"
                 typeLanguage="http://www.w3.org/2001/XMLSchema" expressionLanguage="http://www.w3.org/1999/XPath"
                 targetNamespace="http://www.activiti.org/test">
      <process id={processID} name={processName} isExecutable="true">
        {process.items.map(writeItem)}{process.flows.map(writeFlow)}
      </process>
    </definitions>
  }

  private def writeItem(item: ConcreteWFItemDef): Elem = item match {
    case t: WFTask => userTask(t)
    case st: FlowableServiceTask => serviceTask(st)
    case gt: WFGateway => gateway(gt)
    case ev: WFEvent => event(ev)
  }
  private def writeFlow(s: ConcreteWFFlowDef): Elem = s match {
    case sf: SequenceFlow => writeSequenceFlows(sf)
    //TODO other flow types here
  }



  /*Service task------------------------------------------*/
  def serviceTask(task: FlowableServiceTask): Elem = {
    <serviceTask id={task.id} name={task.label} flowable:class={scala.xml.Unparsed(task.className)}>
      <extensionElements>
        {if (task.extElems.isDefined) {
        task.extElems.get.listeners.map(executionListener)
        //        extensionElements(task.extElems.get)
      }}
      </extensionElements>
    </serviceTask>
  }
  def userTask(task: WFTask): Elem = <userTask id={task.id} name={task.label} flowable:candidateGroups=""></userTask>

  def executionListener(listener: FlowableExecutionListener): Elem =
    <flowable:executionListener event={listener.event} class={scala.xml.Unparsed(listener.className)}></flowable:executionListener>


  // todo : why join and split are converted into paralell?
  def gateway(gt: WFGateway): Elem = gt match {
    case gt if GatewayType.withName(gt.gwtype) == GatewayType.Join =>
      <parallelGateway id={gt.id}></parallelGateway>

    case gt if GatewayType.withName(gt.gwtype) == GatewayType.Split =>
      <parallelGateway id={gt.id}></parallelGateway>

    case gt if GatewayType.withName(gt.gwtype) == GatewayType.Exclusive =>
      <exclusiveGateway id={gt.id}></exclusiveGateway>

    case _ =>
      <exclusiveGateway id={gt.id}></exclusiveGateway>
  }

  def event(ev: WFEvent): Elem = ev match {
    // ~~~~~ START EVENT ~~~~~
    case ev if EventType.withName(ev.eventtype) == EventType.Start =>
      <startEvent id={ev.id} name={ev.label}></startEvent>
    // ~~~~~ END EVENT ~~~~~
    case ev if EventType.withName(ev.eventtype) == EventType.End =>
      <endEvent id={ev.id} name={ev.label}></endEvent>
    // ~~~~~ BOUNDARY EVENT ~~~~~
    case ev if EventType.withName(ev.eventtype) == EventType.Boundary =>
      <boundaryEvent id={ev.id} name={ev.label} attachedToRef={getBoundaryEventAttachedRef(ev.definition)}>
        {if (ev.definition != null) {
        boundaryEventDefinition(ev.definition)
      }}
      </boundaryEvent>
    case _ =>
      <Event id={ev.id} name={ev.label}></Event>

  }

  def getBoundaryEventAttachedRef(evDef: EventDefinition): String = evDef match {
    case theDef: FlowableErrorEventDefinition => theDef.attachedToRef
    case theDef: FlowableTimerEventDefinition => theDef.attachedToRef
    case _ => ""
  }

  def boundaryEventDefinition(evtDef: EventDefinition): Elem = evtDef match {
    case t: FlowableErrorEventDefinition => <errorEventDefinition errorRef={t.errType}></errorEventDefinition>
    case t: FlowableTimerEventDefinition =>
      <timerEventDefinition>
        <timeDuration>
          {scala.xml.Unparsed(t.timecondition)}
        </timeDuration>
      </timerEventDefinition>
  }

  def writeSequenceFlows(s: SequenceFlow): Elem = {
    val sourceRef = s.start match {
      case s if s != null => s.id
      case _ => ""
    }
    val targetRef = s.end match {
      case s if s != null => s.id
      case _ => ""
    }
    <sequenceFlow id={s.id} sourceRef={sourceRef} targetRef={targetRef}>
      {if (s.condition.isDefined) {
      conditionalExpr(s.condition.get.toString)
    }}
    </sequenceFlow>
  }

  def conditionalExpr(expression: String): Elem =
    <conditionExpression xsi:type="tFormalExpression">
      {scala.xml.Unparsed("<![CDATA[%s]]>".format(expression))}
    </conditionExpression>


  /* can remove these
  def extensionElements(elems: FlowableExtentionElements): Elem = {
    <extensionElements>
      {elems.listeners.foreach(executionListener)}
    </extensionElements>
  }
  */

}