package org.icar.business_process

abstract class WFDataDef(id: String)

case class WFDataObjectRef(id: String, objecttype: WFDataType, state: Option[String]) extends WFDataDef(id)
case class WFMessage(id: String, label: String) extends WFDataDef(id)
case class WFSignal(id: String, label: String) extends WFDataDef(id)
case class WFDataType(id: String, name: String)


