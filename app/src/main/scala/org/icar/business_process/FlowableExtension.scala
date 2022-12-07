package org.icar.business_process

/* flowable */
case class FlowableServiceTask(override val id: String, label: String, className: String, extElems: Option[FlowableExtentionElements]) extends ConcreteWFItemDef(id)
case class FlowableErrorEventDefinition(attachedToRef: String, errType: String) extends EventDefinition
case class FlowableTimerEventDefinition(attachedToRef: String, timecondition: String) extends EventDefinition
case class FlowableExtentionElements(listeners: List[FlowableExecutionListener])
case class FlowableExecutionListener(event: String, className: String)
