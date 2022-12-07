package org.icar.grounder

abstract class ConcreteCapability(val id: Int, val title: String, val serviceName: String) {
  def withID(theID: Int): ConcreteCapability
}

case class ServiceConcreteCapability(
                                      override val id: Int, // the ID of this concrete capability.
                                      override val title: String,
                                      override val serviceName: String, //the id of the abstract capability
                                      className: String,
                                      startEventClassName: Option[String] = None,
                                      endEventClassName: Option[String] = None
                                    ) extends ConcreteCapability(id,title, serviceName) {

  override def withID(theID: Int): ConcreteCapability = ServiceConcreteCapability(theID, title, serviceName, className, startEventClassName, endEventClassName)
}

case class HumanConcreteCapability(
                                    override val id: Int, // the ID of this concrete capability.
                                    override val title: String,
                                    override val serviceName: String, //the id of the abstract capability
                                    startEventClassName: Option[String] = None,
                                    endEventClassName: Option[String] = None
                                  ) extends ConcreteCapability(id,title, serviceName) {

  override def withID(theID: Int): ConcreteCapability = HumanConcreteCapability(theID, title, serviceName)

}


//todo rename "grounding"
case class CapabilityRepository(groundings : List[ConcreteCapability]) {

  /**
   * Return the capability groundings that match the input service name.
   */
  def getFromServiceName(serviceName: String) : List[ConcreteCapability] = {
    groundings.filter( x => x.serviceName == serviceName)
  }

  def getFromServiceImplName(className: String): List[ServiceConcreteCapability] = {
    val services = for (f <- groundings if f.isInstanceOf[ServiceConcreteCapability]) yield f.asInstanceOf[ServiceConcreteCapability]
    services.filter( x=>x.className == className)
  }
}