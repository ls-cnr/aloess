package org.icar.grounder

import org.icar.business_process._
import org.icar.grounder.grounding_strategy.GroundingStrategy
import org.icar.grounder.process_decorator.ProcessDecoratorStrategy
import org.icar.symbolic._

import scala.collection.mutable.ListBuffer


/**
 * This class "ground" an abstract workflow to a concrete one. The grounding operation maps every item within the
 * workflow to entities that can be later converted to BPMN.
 *
 * @author Davide Guastella
 */
class SolutionGrounder(
                        repository: CapabilityRepository,
                        groundingStrategy: GroundingStrategy,
                        opt_decorator_strategy: Option[ProcessDecoratorStrategy] = None
                      ) {

  def groundSolution(
                      abstractWF: AbstractWorkflow,
                      groundingConstraints: ListBuffer[GroundingConstraint] = ListBuffer()//,
                      /**
                       * List of constraints that can be applied when grounding tasks. These are used to avoid that certain concrete
                       * capability is grounded because, for example, it doesn't satisfy a temporal constraint, or simply because the
                       * service is not available.
                       */
                    ): ConcreteWorkflow = {

    /* STEP 1 -> 1. replace each abstract capability with a concrete one */
    val solutionTasks = abstractWF.getSolutionTasks
    val groundedTasks = groundSolutionTasks(solutionTasks, groundingConstraints)
    val gateways = groundGateways(abstractWF)
    val events = groundEvents(abstractWF.getStartEvents ++ abstractWF.getEndEvents)
    val flows = groundSequenceFlows(abstractWF.get_flows, groundedTasks ++ gateways ++ events)

    /* STEP 2 -> assemble a new "concrete" workflow */
    var destWF = ConcreteWorkflow(
                  Array[WFDataType](),
                  (groundedTasks ++ gateways ++ events).toArray,
                  flows.toArray,
                  Array[WFDataDef]()
                )

    /* STEP 3 -> apply process decorator (if any) */
    if (opt_decorator_strategy.isDefined) {
      val dec_strategy = opt_decorator_strategy.get
      destWF = dec_strategy.apply(destWF)
    }

    destWF
  }

  /**
   * Do grounding for solution tasks. This takes in input a list of [[SolutionTask]] extracted from
   * an abstract workflow, and create for each task a matching [[ConcreteCapability]] instance (if any). The
   * concrete capability specifies the complete path of the java/scala class that realizes the service in the
   * solution task. The mapping between services and [[ConcreteCapability]] are in the [[CapabilityRepository]]
   * of this class.
   */
  private def groundSolutionTasks(abstract_cap_list: List[CapabilityTask], groundingConstraints: ListBuffer[GroundingConstraint]) : List[ConcreteWFItemDef] = {
    var outputTasks : List[ConcreteWFItemDef] = List.empty

    for (abstract_task <- abstract_cap_list) {
      val abstract_id = abstract_task.cap.id

      // find the matching services
      var concreteCapabilities = findMatchingServices(abstract_id)

      // apply the grounding constraints, if any/specified
      if (groundingConstraints.nonEmpty) {
        concreteCapabilities = concreteCapabilities.filter(cc => checkConstraints(cc,groundingConstraints))
      }

      if (concreteCapabilities.length > 0) {
        //Get the first available capability according to the chosen grounding strategy
        //NOTE: concrete capabilities are SERVICE TASKS
        val solutionTaskID = abstract_task.id
        val concreteCapability = groundingStrategy.apply(concreteCapabilities).withID(solutionTaskID)
        val bpmnCapability = ConcreteCapability2BPMNTask(concreteCapability)
        outputTasks = bpmnCapability :: outputTasks
      } // todo: else raising an error?
    }

    outputTasks
  }

  private def ConcreteCapability2BPMNTask(cap : ConcreteCapability) : ConcreteWFItemDef = {
    cap match {
      case HumanConcreteCapability(id, title, _, _, _) =>
        WFTask(s"st_${id}", title, tasktype = "human")

      case ServiceConcreteCapability(id, title, serviceName, className, startEventClassName, endEventClassName) =>
        var listeners = List[FlowableExecutionListener]()

        startEventClassName match {
          case Some(s) => listeners = listeners ++ List(FlowableExecutionListener(EventType.Start.toString.toLowerCase(), s))
          case None =>
        }
        endEventClassName match {
          case Some(s) => listeners = listeners ++ List(FlowableExecutionListener(EventType.End.toString.toLowerCase(), s))
          case None =>
        }

        FlowableServiceTask(s"st_${id}", title, className, Some(FlowableExtentionElements(listeners)))
    }
  }


  /**
   * Ground the gateways in the input abstract workflow. Gateways can be of three types: join, split or exclusive. These
   * types are are mapped to [[WFGateway]], specifying their type using the enumeration [[GatewayType]]
   *
   * @param abstractWF
   * @return
   */
  def groundGateways(abstractWF: AbstractWorkflow): List[ConcreteWFItemDef] =
    abstractWF.getJoinGateways.map(gt => WFGateway(gt.getStringID(), gt.getStringID(), GatewayType.Join.toString, Converging())) ++
      abstractWF.getSplitGateways.map(gt => WFGateway(gt.getStringID(), gt.getStringID(), GatewayType.Split.toString, Diverging()))
      // ++ getExclusiveGateways(abstractWF).map(gt => Gateway(gt.getStringID(), gt.getStringID(), GatewayType.Exclusive.toString, UnspecifiedDirection()))

  /**
   * maps an endpoint(start/end) of a sequence flow from an abstract item ([[WorkflowItem]]) to a concrete item
   * ([[Item]] or [[WFEvent]] or [[WFGateway]]). All concrete items are assumed to be previously grounded and present
   * in the [[allItems]] list.
   *
   * @param workflowItem the abstract workflow item to be mapped
   * @param allItems     concrete workflow items ([[Item]] or [[WFEvent]] or [[WFGateway]])
   * @return Given the input abstract item (workflowItem), this function returns the corresponding concrete item
   *         (i.e., with the same ID), otherwise [[None]]
   */
  private def getSequenceFlowEndPoint(workflowItem: AbstractWorkflowItem, allItems: List[ConcreteWFItemDef]): Option[ConcreteWFItemDef] =
    allItems.find(st => st.id == workflowItem.getStringID())

  /**
   * Do the grounding of sequence flow in an abstract workflow, and returns sequence flows that can be used in a
   * (concrete) Workflow instance. All workflow items including events, tasks, gateways are also provided as
   * input: these are necessary to register start and end points in the sequence flows.
   *
   * @param flows
   * @param allItems a list including grounded [[WFTask]]s, [[WFEvent]]s and [[WFGateway]]s
   * @return
   */
  def groundSequenceFlows(flows: List[AbstractSequenceFlow], allItems: List[ConcreteWFItemDef]): List[SequenceFlow] = {
    def aux(flows: List[AbstractSequenceFlow], allItems: List[ConcreteWFItemDef], itemID: Int): List[SequenceFlow] = flows match {
      case Nil => List()
      case (head: AbstractSequenceFlow) :: tail =>
        /*map the endpoints (start/end) from abstract items ([[WorkflowItem]] ) to concrete items, that is, Item or
        Event or Gateway. */
        val fromItem = allItems.find(st => st.id == head.from.getStringID()).orNull
        val toItem = allItems.find(st => st.id == head.to.getStringID()).orNull

        aux(tail, allItems, itemID + 1) ++ List(SequenceFlow(s"SequenceFlow_${itemID}",
          fromItem,
          toItem,
          Some(head.condition))) // copy the condition
    }

    aux(flows, allItems, 0)
  }

  /**
   * Ground the events (start/end)
   *
   * @param eventsList
   * @return
   */
  def groundEvents(eventsList: List[AbstractWorkflowItem]): List[ConcreteWFItemDef] = {
    def aux(events: List[AbstractWorkflowItem]): List[ConcreteWFItemDef] = events match {
      case (head: StartEvent) :: tail => WFEvent(head.getStringID(), head.name, EventType.Start.toString, null) :: aux(tail)
      case (head: EndEvent) :: tail => WFEvent(head.getStringID(), head.name, EventType.End.toString, null) :: aux(tail)
      case _ :: tail => aux(tail)
      case Nil => List()
    }

    aux(eventsList)
  }




  /**
   * Check if the input capability satisfies the grounding constraints
   *
   * @param capability
   * @return
   */
  def checkConstraints(capability: ConcreteCapability, groundingConstraints: ListBuffer[GroundingConstraint]): Boolean = {
    def aux(constraints: List[GroundingConstraint]): Boolean = constraints match {
      case head :: tail if head.checkConstraint(capability) => true || aux(tail)
      case _ :: tail => false || aux(tail)
      case Nil => false
    }

    aux(groundingConstraints.toList)
  }

  /**
   * Returns a list of concrete capabilities that realize the input service name
   *
   * @param serviceName
   * @return
   */
  def findMatchingServices(serviceName: String): List[ConcreteCapability] =
    repository.getFromServiceName(serviceName)

}
