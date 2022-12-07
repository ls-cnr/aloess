package org.icar.business_process

abstract class ConcreteWFItemDef(val id: String)

case class WFTask(override val id: String, label: String, tasktype: String, message_opt: Option[WFMessage] = None) extends ConcreteWFItemDef(id)
case class WFEvent(override val id: String, label: String, eventtype: String, definition: EventDefinition) extends ConcreteWFItemDef(id)
case class WFGateway(override val id: String, label: String, gwtype: String, direction: GatewayDirection) extends ConcreteWFItemDef(id)



abstract class GatewayDirection
case class Diverging() extends GatewayDirection
case class Converging() extends GatewayDirection
case class UnspecifiedDirection() extends GatewayDirection

abstract class EventDefinition
case class MessageEventDefinition(mess: WFMessage) extends EventDefinition
case class SignalEventDefinition(signal: WFSignal) extends EventDefinition
case class TimerEventDefinition(timertype: String, timecondition: String) extends EventDefinition
case class EmptyEventDefinition() extends EventDefinition


object EventType extends Enumeration {
  val Start, End, Boundary = Value

  override def toString() =
    this match {
      case Start => "start"
      case End => "end"
      case Boundary => "boundary"
      case _ => "undefined"
    }
}

object GatewayType extends Enumeration {
  val Join, Split, Exclusive = Value

  override def toString() =
    this match {
      case Join => "Join"
      case Split => "Split"
      case Exclusive => "Exclusive"
      case _ => "undefined"
    }
}
