package org.icar.grounder

abstract class GroundingConstraint(serviceName: String, concreteCapability: ConcreteCapability) {
  def checkConstraint(cap: ConcreteCapability): Boolean
}
