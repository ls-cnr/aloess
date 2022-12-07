package org.icar.grounder.process_decorator

import org.icar.business_process.ConcreteWorkflow

/**
 * A process decorator takes a concrete workflow as input and decorates it with additional elements. For example, a
 * decorator can add elements to the workflow needed for execution using a WFM.
 *
 * @author Davide Guastella
 */

abstract class ProcessDecorator {
  def apply(wf: ConcreteWorkflow): ConcreteWorkflow
}
