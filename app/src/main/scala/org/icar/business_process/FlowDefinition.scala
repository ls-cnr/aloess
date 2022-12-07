package org.icar.business_process

import org.icar.symbolic.LogicFormula

abstract class ConcreteWFFlowDef(id: String)
case class SequenceFlow(id: String, start: ConcreteWFItemDef, end: ConcreteWFItemDef, condition: Option[LogicFormula]) extends ConcreteWFFlowDef(id)
case class DataInputFlow(id: String, target: WFTask, data: WFDataObjectRef) extends ConcreteWFFlowDef(id)
case class DataOutputFlow(id: String, source: WFTask, data: WFDataObjectRef) extends ConcreteWFFlowDef(id)
case class BoundaryFlow(id: String, source: WFTask, boundary: WFEvent) extends ConcreteWFFlowDef(id)

abstract class MessageFlow(id: String) extends ConcreteWFFlowDef(id)
case class InMessageFlow(id: String, receiver: WFTask, mess: WFMessage) extends MessageFlow(id)
case class OutMessageFlow(id: String, sender: WFTask, mess: WFMessage) extends MessageFlow(id)
