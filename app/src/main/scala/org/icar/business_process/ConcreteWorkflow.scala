package org.icar.business_process

case class ConcreteWorkflow(datatypes: Array[WFDataType], items: Array[ConcreteWFItemDef], flows: Array[ConcreteWFFlowDef], data: Array[WFDataDef]) {
  def complexity: Double = {
    var c: Double = 0
    for (i <- items if i.isInstanceOf[WFGateway]) {
      val g: WFGateway = i.asInstanceOf[WFGateway]

      g.gwtype match {
        case "exclusive" =>
          val n = outgoing_flows(g).length
          c += n

        case "parallel" =>
          c += 1

        case "inclusive" =>
          val n = outgoing_flows(g).length
          c += scala.math.pow(2, n) - 1

        case _ =>

      }

    }

    c
  }

  def control_flows: Array[ConcreteWFFlowDef] = for (f <- flows if f.isInstanceOf[SequenceFlow]) yield f

  def outgoing_flows(item: ConcreteWFItemDef): Array[SequenceFlow] = {
    var sel_flows: List[SequenceFlow] = List.empty

    for (f <- flows if f.isInstanceOf[SequenceFlow]) {
      val seq = f.asInstanceOf[SequenceFlow]
      if (seq.start == item)
        sel_flows = seq :: sel_flows
    }

    sel_flows.toArray
  }
}