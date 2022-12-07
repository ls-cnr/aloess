package org.icar.business_process.converter

import org.icar.business_process._
import org.icar.symbolic._

import scala.collection.mutable.ArrayBuffer

class BPMN2Goal(wf : ConcreteWorkflow) {

  def temporal_goal(item : ConcreteWFItemDef) : Option[LogicFormula with LTLNature] = {
    item match {
      case  t : WFTask =>
        val ws = task_waited_state(t)
        val gs = task_generated_state(t)
        val pred = predecessors_influence(t)
        val succ = successors_influence(t)

        val goal = Conjunction(List(
          Until(
            Negation(gs),
            Conjunction(List(ws,pred)
            )),
          Finally(
            Conjunction(List(
              gs,
              succ
            ))
          )
        ))

        Some(goal)
      case _ => None
    }

  }

  def waited_state(item : ConcreteWFItemDef) : LogicFormula = {
    item match {
      case  t : WFTask => task_waited_state(t)
      case e : WFEvent => event_waited_state(e)
      case g : WFGateway => gateway_waited_state(g)
    }
  }

  def generated_state(item : ConcreteWFItemDef) : LogicFormula = {
    item match {
      case  t : WFTask => task_generated_state(t)
      case e : WFEvent => event_generated_state(e)
      case g : WFGateway => gateway_generated_state(g)
    }
  }

  def successors_influence(item : ConcreteWFItemDef) : LogicFormula = {
    val disj = ArrayBuffer[LogicFormula]()
    val out_seq = outgoing_seq(item)
    for (f <- out_seq)
      if (f.condition.isDefined)
        disj += conjunction_or_truth(inverse_propagation(f),f.condition.get)
      else
        disj += inverse_propagation(f)

    x_disjunction_or_truth(disj)
  }

  def predecessors_influence(item : ConcreteWFItemDef) : LogicFormula = {
    var disj = ArrayBuffer[LogicFormula]()
    val in_seq = incoming_seq(item)
    for (f <- in_seq)
      if (f.condition.isDefined)
        disj += conjunction_or_truth(direct_propagation(f),f.condition.get)
      else
        disj += direct_propagation(f)

    x_disjunction_or_truth(disj)
  }

  def goal_trigger_condition(item : ConcreteWFItemDef) : LogicFormula = {
    conjunction_or_truth(waited_state(item),predecessors_influence(item))
  }

  def goal_final_state(item : ConcreteWFItemDef) : LogicFormula = {
    conjunction_or_truth(generated_state(item),successors_influence(item))
  }

  def direct_propagation(f: SequenceFlow): LogicFormula = {
    val pred = f.start
    pred match {
      case  t : WFTask => generated_state(t)
      case e : WFEvent => generated_state(e)
      case g : WFGateway =>
        if (g.gwtype=="exclusive") {
          val disj = ArrayBuffer[LogicFormula]()
          val in_seq = incoming_seq(g)
          for (f <- in_seq)
            if (f.condition.isDefined)
              disj += conjunction_or_truth(direct_propagation(f),f.condition.get)
            else
              disj += direct_propagation(f)

          x_disjunction_or_truth(disj)

        } else if (g.gwtype=="inclusive") {
          val conj = ArrayBuffer[LogicFormula]()
          val in_seq = incoming_seq(g)
          for (f <- in_seq)
            if (f.condition.isDefined)
              conj += conjunction_or_truth(direct_propagation(f),f.condition.get)
            else
              conj += direct_propagation(f)

          disjunction_or_truth(conj)

        } else if (g.gwtype=="parallel") {
          val conj = ArrayBuffer[LogicFormula]()
          val in_seq = incoming_seq(g)
          for (f <- in_seq)
            if (f.condition.isDefined)
              conj += conjunction_or_truth(direct_propagation(f),f.condition.get)
            else
              conj += direct_propagation(f)

          conjunction_or_truth(conj)

        } else {
          True()
        }
      case _ => True()
    }

  }

  def inverse_propagation(f: SequenceFlow): LogicFormula = {
    val pred = f.end
    pred match {
      case  t : WFTask => waited_state(t)
      case e : WFEvent => waited_state(e)
      case g : WFGateway =>
        if (g.gwtype=="exclusive") {
          val disj = ArrayBuffer[LogicFormula]()
          val out_seq = outgoing_seq(g)
          for (f <- out_seq)
            if (f.condition.isDefined)
              disj += conjunction_or_truth(inverse_propagation(f),f.condition.get)
            else
              disj += inverse_propagation(f)

          x_disjunction_or_truth(disj)

        } else if (g.gwtype=="inclusive") {
          val disj = ArrayBuffer[LogicFormula]()
          val out_seq = outgoing_seq(g)
          for (f <- out_seq)
            if (f.condition.isDefined)
              disj += conjunction_or_truth(direct_propagation(f),f.condition.get)
            else
              disj += inverse_propagation(f)

          disjunction_or_truth(disj)

        } else if (g.gwtype=="parallel") {
          val conj = ArrayBuffer[LogicFormula]()
          val out_seq = outgoing_seq(g)
          for (f <- out_seq)
            if (f.condition.isDefined)
              conj += conjunction_or_truth(inverse_propagation(f),f.condition.get)
            else
              conj += inverse_propagation(f)

          conjunction_or_truth(conj)

        } else {
          True()
        }
      case _ => True()
    }

  }


  private def task_waited_state(t: ConcreteWFItemDef): LogicFormula = {
    val conj = ArrayBuffer[LogicFormula]()

    val data_in = expected_data(t)
    val mess_in = expected_messages(t)

    for (d<-data_in) {
      val state = if (d.state.isDefined) label(d.state.get) else "available"
      conj += Proposition(state, List(AtomTerm(label(d.objecttype.name))))

    }

    for (m<-mess_in) {
      val state = "received"
      conj += Proposition(state, List(AtomTerm(label(m.label))))

    }

    conjunction_or_truth(conj)
  }

  private def event_waited_state(t: ConcreteWFItemDef): LogicFormula = True()

  private def gateway_waited_state(g: WFGateway): LogicFormula = True()

  private def task_generated_state(t: ConcreteWFItemDef): LogicFormula = {
    val normal_termination_terms = ArrayBuffer[LogicFormula]()

    val data_out = produced_data(t)
    val mess_out = produced_messages(t)

    for (d<-data_out) {
      val state = if (d.state.isDefined) label(d.state.get) else "available"
      normal_termination_terms +=  Proposition(state, List(AtomTerm(label(d.objecttype.name))))
    }

    for (m<-mess_out) {
      val state = "sent"
      normal_termination_terms += Proposition(state, List(AtomTerm(label(m.label))))
    }

    if (normal_termination_terms.isEmpty && t.isInstanceOf[WFTask]) {
      val task = t.asInstanceOf[WFTask]
      normal_termination_terms += Proposition("done",List(AtomTerm(label(task.label))))
    }

    val normal_termination = conjunction_or_truth(normal_termination_terms)


    /* check for boundary conditions */
    val boundary_termination_terms = ArrayBuffer[LogicFormula](normal_termination)
    val boundaries = attached_bondary_conditions(t)
    for (bf <- boundaries) {
      val boundary_event = bf.boundary
      boundary_termination_terms += event_generated_state(boundary_event)
    }
    x_disjunction_or_truth(boundary_termination_terms)

  }

  private def attached_bondary_conditions(t: ConcreteWFItemDef) : List[BoundaryFlow] = {
    var att : List[BoundaryFlow] = List()

    for (f <- wf.flows if f.isInstanceOf[BoundaryFlow]) {
      val bf = f.asInstanceOf[BoundaryFlow]
      if (bf.source==t)
        att = bf :: att
    }

    att
  }

  private def event_generated_state(t: ConcreteWFItemDef): LogicFormula = {
    val conj = ArrayBuffer[LogicFormula]()

    if (t.isInstanceOf[WFEvent]) {
      val event = t.asInstanceOf[WFEvent]

      event.definition match {
        case e: EmptyEventDefinition =>

        case m: MessageEventDefinition =>
          val state = "received"
          if (!m.mess.label.isEmpty) {
            conj += Proposition(state, List(AtomTerm(label(m.mess.label))) )
          } else {
            conj += Proposition(state, List(AtomTerm(label(event.label))))
          }

        case s: SignalEventDefinition =>
          val state = "catched"
          conj += Proposition(state, List(AtomTerm(label(s.signal.label))))

        case x: TimerEventDefinition =>
          x.timertype match {
            case "duration" =>
              if (event.eventtype=="boundary") {
                val task = search_task_via_boundary(t)
                val pre_inf = predecessors_influence(task)
                conj += Proposition("after", List(AtomTerm(x.timecondition)))

              } else {
                val pre_inf = predecessors_influence(t)
                val p = Proposition("after", List(AtomTerm(x.timecondition)))

              }
            case "timedate" =>
              conj += Proposition("at", List(AtomTerm(x.timecondition)))

            case "repetition" =>
              conj += Proposition("every", List(AtomTerm(x.timecondition)))

            case _ =>
          }
      }
    }

    conjunction_or_truth(conj)
  }

  private def search_task_via_boundary(t: ConcreteWFItemDef) : WFTask = {
    var task : WFTask = null
    for (f <- wf.flows if f.isInstanceOf[BoundaryFlow]) {
      val bf = f.asInstanceOf[BoundaryFlow]
      if (bf.boundary==t)
        task = bf.source
    }
    task
  }

  private def gateway_generated_state(g: WFGateway): LogicFormula = True()

  private def incoming_seq(item : ConcreteWFItemDef) : List[SequenceFlow] = {
    var l = List[SequenceFlow]()

    for (f <- wf.flows if f.isInstanceOf[SequenceFlow]) {
      val flow = f.asInstanceOf[SequenceFlow]
      if (flow.end == item)
        l = flow :: l
    }

    l
  }

  private def outgoing_seq(item : ConcreteWFItemDef) : List[SequenceFlow] = {
    var l = List[SequenceFlow]()

    for (f <- wf.flows if f.isInstanceOf[SequenceFlow]) {
      val flow = f.asInstanceOf[SequenceFlow]
      if (flow.start == item)
        l = flow :: l
    }

    l
  }

  private def expected_data(t: ConcreteWFItemDef) : List[WFDataObjectRef] = {
    var l = List[WFDataObjectRef]()

    for (f <- wf.flows if f.isInstanceOf[DataInputFlow]) {
      val in_flow = f.asInstanceOf[DataInputFlow]
      if (in_flow.target == t)
        l = in_flow.data :: l
    }

    l
  }

  private def expected_messages(t: ConcreteWFItemDef) : List[WFMessage] = {
    var l = Set[WFMessage]()

    if (t.isInstanceOf[WFTask]) {
      val task = t.asInstanceOf[WFTask]
      if (task.tasktype=="receive" && task.message_opt.isDefined) {
        val mess = task.message_opt.get
        l += mess
      }
    }

    l.toList
  }

  private def produced_data(t: ConcreteWFItemDef) : List[WFDataObjectRef] = {
    var l = List[WFDataObjectRef]()

    for (f <- wf.flows if f.isInstanceOf[DataOutputFlow]) {
      val out_flow = f.asInstanceOf[DataOutputFlow]
      if (out_flow.source == t)
        l = out_flow.data :: l
    }

    l
  }

  private def produced_messages(t: ConcreteWFItemDef) : List[WFMessage] = {
    var l = Set[WFMessage]()

    if (t.isInstanceOf[WFTask]) {
      val task = t.asInstanceOf[WFTask]
      if (task.tasktype=="send" && task.message_opt.isDefined) {
        val mess = task.message_opt.get
        l += mess
      }
    }

    l.toList
  }

  private def label(s : String) : String = s.replace("\n","_").replace(" ","_").toLowerCase

  private def disjunction_or_truth(term1:LogicFormula,term2:LogicFormula) : LogicFormula = {
    disjunction_or_truth(ArrayBuffer(term1,term2))
  }

  private def disjunction_or_truth(array : ArrayBuffer[LogicFormula]) : LogicFormula = {
    val array_no_true : scala.collection.mutable.Set[LogicFormula] = scala.collection.mutable.Set()

    for (a<-array if a != True())
      array_no_true += a

    if (array_no_true.isEmpty) {
      True()
    }else if (array_no_true.size==1) {
      array_no_true.head
    }else {
      Disjunction(array_no_true.toList)
    }
  }

  private def x_disjunction_or_truth(term1:LogicFormula,term2:LogicFormula) : LogicFormula = {
    x_disjunction_or_truth(ArrayBuffer(term1,term2))
  }

  private def x_disjunction_or_truth(array : ArrayBuffer[LogicFormula]) : LogicFormula = {
    val array_no_true : scala.collection.mutable.Set[LogicFormula] = scala.collection.mutable.Set()

    for (a<-array if a != True())
      array_no_true += a

    if (array_no_true.isEmpty) {
      True()
    }else if (array_no_true.size==1) {
      array_no_true.head
    }else {
      ExclDisj(array_no_true.toList)
    }
  }


  private def conjunction_or_truth(term1:LogicFormula,term2:LogicFormula) : LogicFormula = {
    conjunction_or_truth(ArrayBuffer(term1,term2))
  }

  private def conjunction_or_truth(array : ArrayBuffer[LogicFormula]) : LogicFormula = {
    val array_no_true : scala.collection.mutable.Set[LogicFormula] = scala.collection.mutable.Set()

    for (a<-array if a != True())
      array_no_true += a

    if (array_no_true.isEmpty) {
      True()
    } else if (array_no_true.size==1) {
      array_no_true.head
    }else{
      Conjunction(array_no_true.toList)
    }
  }
}


