package org.icar.business_process.parser

import org.icar.business_process._
import org.icar.business_process.converter.BPMN2Goal
import org.icar.symbolic.parser.PropositionFormulaParser
import org.icar.symbolic.{AbstractSequenceFlow => _, _}

import java.io.InputStream
import scala.collection.mutable.ArrayBuffer
import scala.xml.{Elem, Node, NodeSeq, XML}

class bpmn_parser(is: InputStream) {

  var datatypes: ArrayBuffer[WFDataType] = ArrayBuffer()
  var dataobjects: List[WFDataObjectRef] = List()
  var messages: List[WFMessage] = List()
  var signals: List[WFSignal] = List()
  var data: List[WFDataDef] = List()
  var items: List[ConcreteWFItemDef] = List()
  var flows: List[ConcreteWFFlowDef] = List()

  val node = XML.load(is)
  val w: Option[ConcreteWorkflow] = extract_workflow(node)


  def goals_from_InputStream: Array[LTLGoalSpec] = {
    var goals: List[LTLGoalSpec] = List.empty

    if (w.isDefined) {
      val workflow = w.get
      val wfstate = new BPMN2Goal(workflow)
      for (i <- workflow.items if (i.isInstanceOf[WFTask])) {
        val task = i.asInstanceOf[WFTask]
        val formula = wfstate.temporal_goal(i)
        if (formula.isDefined)
          goals = LTLGoalSpec(task.label.replaceAll(" ", "_"), True(), formula.get) :: goals
      }
    }
    goals.toArray
  }

  def initial_state: StateOfWorld = {
    def flatten_predicate_formula(pre: LogicFormula): List[Proposition] = {
      var preds: List[Proposition] = List.empty

      pre match {
        case Proposition(functional, terms) => preds = List(Proposition(functional, terms))
        case Conjunction(formulas) => for (f <- formulas) preds = preds ::: flatten_predicate_formula(f)
        case _ =>
      }

      preds
    }

    var preds: List[Proposition] = List.empty
    if (w.isDefined) {
      val workflow = w.get
      val wfstate = new BPMN2Goal(workflow)
      for (i <- workflow.items if (i.isInstanceOf[WFEvent])) {
        val event = i.asInstanceOf[WFEvent]
        if (event.eventtype == "start") {
          val outs = workflow.outgoing_flows(event)
          for (o <- outs) {
            val next = o.end
            val pre: LogicFormula = wfstate.goal_trigger_condition(next)
            preds = flatten_predicate_formula(pre)
          }
        }
      }
    }

    StateOfWorld(preds)
  }

  def goal_string_from_InputStream: String = {
    var goal_string = ""

    if (w.isDefined) {
      val workflow = w.get
      val wfstate = new BPMN2Goal(workflow)
      for (i <- workflow.items if (i.isInstanceOf[WFTask])) {
        val task = i.asInstanceOf[WFTask]
        val pre = wfstate.goal_trigger_condition(i)
        val post = wfstate.goal_final_state(i)

        goal_string += "GOAL: " + task.label.replace("\n", " ") + "\n"
        goal_string += "\tpre: " + pre + "\n"
        goal_string += "\tpost: " + post + "\n\n"

      }
    }
    goal_string
  }

  def fullFromInputStream: String = {
    var goal_string = ""

    if (w.isDefined) {
      val workflow = w.get
      val wfstate = new BPMN2Goal(workflow)
      for (i <- workflow.items if (i.isInstanceOf[WFTask])) {
        val task = i.asInstanceOf[WFTask]
        val ws = wfstate.waited_state(i)
        val gs = wfstate.generated_state(i)
        val pre_inf = wfstate.predecessors_influence(i)
        val post_inf = wfstate.successors_influence(i)

        val pre = wfstate.goal_trigger_condition(i)
        val post = wfstate.goal_final_state(i)

        val goal = wfstate.temporal_goal(i)

        goal_string += "GOAL: " + task.label.replace("\n", " ") + "\n"
        goal_string += "\tws: " + ws + "\n"
        goal_string += "\tgs: " + gs + "\n"
        goal_string += "\tpre-inf: " + pre_inf + "\n"
        goal_string += "\tpost-inf: " + post_inf + "\n"
        goal_string += "\tpre-cond: " + pre + "\n"
        goal_string += "\tfinal-state: " + post + "\n"
        if (goal.isDefined)
          goal_string += "\tLTL: " + goal.get + "\n"

        goal_string += "\n"

      }

    }
    goal_string
  }

  def fromFile(filename: String): Option[ConcreteWorkflow] = {
    val node = XML.loadFile(filename)
    extract_workflow(node)
  }

  private def extract_workflow(node: Elem) = {
    if (node.label == "definitions") {
      datatypes = parse_items(node \\ "dataObject")

      messages = parse_messages(node \\ "message")
      dataobjects = parse_dataobjects(node \\ "dataObjectReference") ::: parse_datastore(node \\ "dataStore")
      signals = parse_signals(node \\ "signal")

      data = messages ::: dataobjects ::: signals

      val items_task = parse_task(node \\ "task", "task")
      val items_receivetask = parse_task(node \\ "receiveTask", "receive")
      val items_sendtask = parse_task(node \\ "sendTask", "send")
      val items_usertask = parse_task(node \\ "userTask", "user")
      val items_manualtask = parse_task(node \\ "manualTask", "user")
      val items_servicetask = parse_task(node \\ "serviceTask", "services")
      val items_scripttask = parse_task(node \\ "scriptTask", "script")

      val event_start = parse_event(node \\ "startEvent", "start")
      val event_end = parse_event(node \\ "endEvent", "end")
      val event_boundary = parse_event(node \\ "boundaryEvent", "boundary")
      val intermediate = parse_event(node \\ "intermediateCatchEvent", "interm_catch")

      val exclusive_gateways = parse_gateway(node \\ "exclusiveGateway", "exclusive")
      val parallel_gateways = parse_gateway(node \\ "parallelGateway", "parallel")
      val inclusive_gateways = parse_gateway(node \\ "inclusiveGateway", "inclusive")

      items = items_task ::: items_receivetask ::: items_sendtask ::: items_usertask ::: items_manualtask ::: items_servicetask ::: items_scripttask :::
        event_start ::: event_end ::: event_boundary ::: intermediate :::
        exclusive_gateways ::: parallel_gateways ::: inclusive_gateways

      val sequence_flows = parse_sequence_flow(node \\ "sequenceFlow")
      val message_flows = parse_message_flow(node \\ "messageFlow")
      val indataflow = parse_in_data_flow(node \\ "task" ++ node \\ "receiveTask" ++ node \\ "sendTask" ++ node \\ "userTask" ++ node \\ "serviceTask", items, dataobjects)
      val outdataflow = parse_out_data_flow(node \\ "task" ++ node \\ "receiveTask" ++ node \\ "sendTask" ++ node \\ "userTask" ++ node \\ "serviceTask", items, dataobjects)
      val boundary_flows = parse_boundary_flow(node \\ "boundaryEvent", items)


      flows = sequence_flows ::: message_flows ::: indataflow ::: outdataflow ::: boundary_flows

      Some(ConcreteWorkflow(datatypes.toArray, items.toArray, flows.toArray, data.toArray))

    } else {

      None
    }
  }

  private def parse_items(nodes: NodeSeq): ArrayBuffer[WFDataType] = {
    var l = new ArrayBuffer[WFDataType]()

    for (p <- nodes) {
      val id = p \ "@id"
      val name = p \ "@name"

      l += WFDataType(id.text, name.text)
    }

    l
  }

  private def parse_messages(nodes: NodeSeq): List[WFMessage] = {
    var l = List[WFMessage]()

    for (p <- nodes) {
      val id = p \ "@id"
      val label = p \ "@name"

      l = WFMessage(id.text, label.text) :: l
    }

    l
  }

  private def parse_signals(nodes: NodeSeq): List[WFSignal] = {
    var l = List[WFSignal]()

    for (p <- nodes) {
      val id = p \ "@id"
      val label = p \ "@name"

      l = WFSignal(id.text, label.text) :: l
    }

    l
  }

  private def parse_dataobjects(nodes: NodeSeq): List[WFDataObjectRef] = {
    var l = List[WFDataObjectRef]()

    for (p <- nodes) {
      val id = p \ "@id"
      val ref = (p \ "@dataObjectRef").text

      val state_tag = (p \ "dataState")

      val datatype = search_datatype_by_id(ref)
      if (datatype != null) {
        if (state_tag.length > 0)
          l = WFDataObjectRef(id.text, datatype, Some((state_tag \ "@name").text)) :: l
        else
          l = WFDataObjectRef(id.text, datatype, None) :: l
      }
    }

    l
  }

  private def parse_datastore(nodes: NodeSeq): List[WFDataObjectRef] = {
    var l = List[WFDataObjectRef]()

    for (p <- nodes) {
      val id = (p \ "@id").text
      val name = (p \ "@name").text

      l = WFDataObjectRef(id, WFDataType(id, name), None) :: l
    }

    l
  }


  private def parse_task(nodes: NodeSeq, tasktype: String): List[WFTask] = {
    var l = List[WFTask]()

    for (p <- nodes) {
      val id = p \ "@id"
      val label = p \ "@name"

      val mess_id: String = (p \ "@messageRef").text
      if (!mess_id.isEmpty) {
        val message = search_message_by_id(mess_id)
        l = WFTask(id.text, label.text, tasktype, Some(message)) :: l
      } else {
        l = WFTask(id.text, label.text, tasktype) :: l
      }

    }

    l
  }

  private def parse_event(nodes: NodeSeq, eventtype: String): List[WFEvent] = {
    var l = List[WFEvent]()

    for (p <- nodes) {
      val id = p \ "@id"
      val label = p \ "@name"

      val mess_id = ((p \ "messageEventDefinition") \ "@messageRef").text
      val sign_id = ((p \ "signalEventDefinition") \ "@signalRef").text

      val duration = (p \\ "timeDuration").text
      val timedate = (p \\ "timeDate").text
      val ripet = (p \\ "timeCycle").text

      if (!mess_id.isEmpty) {
        val message = search_message_by_id(mess_id)
        l = WFEvent(id.text, label.text, eventtype, MessageEventDefinition(message)) :: l
      } else if (!sign_id.isEmpty) {
        val signal = search_signal_by_id(sign_id)
        l = WFEvent(id.text, label.text, eventtype, SignalEventDefinition(signal)) :: l
      } else if (!duration.isEmpty) {
        l = WFEvent(id.text, label.text, eventtype, TimerEventDefinition("duration", duration)) :: l
      } else if (!timedate.isEmpty) {
        l = WFEvent(id.text, label.text, eventtype, TimerEventDefinition("timedate", timedate)) :: l
      } else if (!ripet.isEmpty) {
        l = WFEvent(id.text, label.text, eventtype, TimerEventDefinition("repetition", ripet)) :: l
      } else if (eventtype == "start" || eventtype == "end") {
        l = WFEvent(id.text, label.text, eventtype, EmptyEventDefinition()) :: l
      }

    }
    l
  }


  private def parse_gateway(nodes: NodeSeq, gwtype: String): List[WFGateway] = {
    var l = List[WFGateway]()

    for (p <- nodes) {
      val id = p \ "@id"
      val label = p \ "@name"

      val dir: GatewayDirection = parse_gateway_direction(p)

      l = WFGateway(id.text, label.text, gwtype, dir) :: l
    }

    l
  }

  def parse_gateway_direction(p: Node): GatewayDirection = {
    val ins = p \\ "incoming"
    val outs = p \\ "outgoing"

    if (ins.size == 1)
      Diverging()
    else if (outs.size == 1)
      Converging()
    else
      UnspecifiedDirection()
  }


  private def parse_sequence_flow(nodes: NodeSeq): List[SequenceFlow] = {
    var l = List[SequenceFlow]()

    for (p <- nodes) {
      val id = p \ "@id"
      val start_id = (p \ "@sourceRef").text
      val end_id = (p \ "@targetRef").text

      val start = search_item_by_id(start_id)
      val end = search_item_by_id(end_id)

      val cond_tag = p \\ "conditionExpression"
      var optional_condition: Option[LogicFormula] = None
      val text = cond_tag.text
      if (text.trim.length > 0) {
        val par = new PropositionFormulaParser()
        val parsing_action = par.parseAll(par.formula, text)
        if (parsing_action.successful)
          optional_condition = Some(parsing_action.get)
      }

      l = SequenceFlow(id.text, start, end, optional_condition) :: l

    }

    l
  }

  private def parse_message_flow(nodes: NodeSeq): List[MessageFlow] = {
    var l = List[MessageFlow]()

    for (p <- nodes) {
      val id = p \ "@id"
      val start_id = (p \ "@sourceRef").text
      val end_id = (p \ "@targetRef").text
      val message_id = (p \ "@messageRef").text

      val message = search_message_by_id(message_id)
      val sender_task = search_item_by_id(start_id)
      val receiver_task = search_item_by_id(end_id)

      if (message != null && receiver_task != null && receiver_task.isInstanceOf[WFTask]) {
        l = InMessageFlow(id.text, receiver_task.asInstanceOf[WFTask], message) :: l
      } else if (message != null && sender_task != null && sender_task.isInstanceOf[WFTask]) {
        l = OutMessageFlow(id.text, sender_task.asInstanceOf[WFTask], message) :: l
      }
    }

    l
  }

  private def parse_in_data_flow(nodes: NodeSeq, items: List[ConcreteWFItemDef], dataobjects: List[WFDataObjectRef]): List[DataInputFlow] = {
    var l = List[DataInputFlow]()

    for (p <- nodes) {
      val taskid = (p \ "@id").text
      val task = search_item_by_id(taskid).asInstanceOf[WFTask]

      val data_inputs = p \\ "dataInputAssociation"
      for (d <- data_inputs if d != null) {
        val id = (d \ "@id").text
        val dataid = (d \ "sourceRef").text
        val data = search_dataobjectred_by_id(dataid)

        if (data != null && task != null)
          l = DataInputFlow(id, task, data) :: l
      }

    }

    l
  }

  private def parse_out_data_flow(nodes: NodeSeq, items: List[ConcreteWFItemDef], dataobjects: List[WFDataObjectRef]): List[DataOutputFlow] = {
    var l = List[DataOutputFlow]()

    for (p <- nodes) {
      val taskid = (p \ "@id").text
      val task = search_item_by_id(taskid).asInstanceOf[WFTask]

      val data_inputs = p \\ "dataOutputAssociation"
      for (d <- data_inputs if d != null) {
        val id = (d \ "@id").text
        val dataid = (d \ "targetRef").text
        val data = search_dataobjectred_by_id(dataid)

        if (data != null && task != null)
          l = DataOutputFlow(id, task, data) :: l
      }

    }

    l
  }

  private def parse_boundary_flow(nodes: NodeSeq, items: List[ConcreteWFItemDef]): List[BoundaryFlow] = {
    var l = List[BoundaryFlow]()

    for (p <- nodes) {
      val id = (p \ "@id").text
      val taskid = (p \ "@attachedToRef").text
      val task = search_item_by_id(taskid).asInstanceOf[WFTask]
      val event = search_item_by_id(id).asInstanceOf[WFEvent]

      l = BoundaryFlow(id, task, event) :: l
    }

    l
  }


  private def search_item_by_id(id: String): ConcreteWFItemDef = {
    var item: ConcreteWFItemDef = null
    var remaining = items

    while (remaining.length > 0 && item == null) {
      if (remaining.head.id == id)
        item = remaining.head
      remaining = remaining.tail
    }

    item
  }

  private def search_datatype_by_id(id: String): WFDataType = {
    var item: WFDataType = null
    var remaining = datatypes

    while (remaining.length > 0 && item == null) {
      if (remaining.head.id == id)
        item = remaining.head
      remaining = remaining.tail
    }

    item
  }

  private def search_message_by_id(id: String): WFMessage = {
    var item: WFMessage = null
    var remaining = messages

    while (remaining.length > 0 && item == null) {
      if (remaining.head.id == id)
        item = remaining.head
      remaining = remaining.tail
    }

    item
  }

  private def search_signal_by_id(id: String): WFSignal = {
    var item: WFSignal = null
    var remaining = signals

    while (remaining.length > 0 && item == null) {
      if (remaining.head.id == id)
        item = remaining.head
      remaining = remaining.tail
    }

    item
  }

  private def search_dataobjectred_by_id(id: String): WFDataObjectRef = {
    var item: WFDataObjectRef = null
    var remaining = dataobjects

    while (remaining.length > 0 && item == null) {
      if (remaining.head.id == id)
        item = remaining.head
      remaining = remaining.tail
    }

    item
  }
}