package org.icar.domain.nettunit

import org.icar.grounder.{CapabilityRepository, ConcreteCapability, HumanConcreteCapability, ServiceConcreteCapability}

object netunit_concrete_repository {
  var repository : Map[String,ConcreteCapability] = Map.empty

  def get_repo : CapabilityRepository = {
    CapabilityRepository(repository.values.toList)
  }

  repository = repository + ("send_team_to_evaluate" ->
    HumanConcreteCapability(0,
      "c_send_team_to_evaluate",
      "send_team_to_evaluate",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  repository = repository + ("activate_internal_security_plan" ->
    HumanConcreteCapability(1,
      "Activate internal plan",
      "activate_internal_security_plann",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  repository = repository + ("notify_competent_body_internal_plan" ->
    ServiceConcreteCapability(2,
      "Notify competent body",
      "notify_competent_body_internal_plan",
      "nettunit.handler.notify_competent_body_internal_plan",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  repository = repository + ("notify_competent_body_internal_plan" ->
    ServiceConcreteCapability(14,
      "Notify competent body (ALT)",
      "notify_competent_body_internal_plan",
      "nettunit.handler.notify_competent_body_internal_plan",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
    )

  repository = repository + ("inform_technical_rescue_organisation_internal_plan" ->
    ServiceConcreteCapability(3,
      "Inform technical rescue orgs.",
      "inform_technical_rescue_organisation_internal_plan",
      "nettunit.handler.inform_technical_rescue_organisation_internal_plan",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
    )

  repository = repository + ("inform_technical_rescue_organisation_internal_plan" ->
    ServiceConcreteCapability(15,
      "[ALT] Inform technical rescue orgs.",
      "inform_technical_rescue_organisation_internal_plan",
      "nettunit.handler.alternative_services.inform_technical_rescue_organisation_internal_plan_alt",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
    )

  repository = repository + ("decide_response_type" ->
    HumanConcreteCapability(4,
      "Decide response type",
      "decide_response_type",
    Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
    Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  repository = repository + ("prepare_tech_report" ->
    ServiceConcreteCapability(5,
      "Prepare tech report",
      "prepare_tech_report",
      "nettunit.handler.prepare_tech_report",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
    )

  repository = repository + ("keep_update_involved_personnel" ->
    ServiceConcreteCapability(6,
      "Keep update involved personnel",
      "keep_update_involved_personnel",
      "nettunit.handler.keep_update_involved_personnel",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
    )

  repository = repository + ("declare_pre_alert_state" ->
    HumanConcreteCapability(7,
      "Declare pre-alert state",
      "declare_pre_alert_state",
    Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
    Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  repository = repository + ("inform_technical_rescue_organisation_alert" ->
    ServiceConcreteCapability(8,
      "Inform technical rescue orgs.",
      "inform_technical_rescue_organisation_alert",
      "nettunit.handler.inform_technical_rescue_organisation_alert",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
    )

  repository = repository + ("evaluate_fire_radiant_energy" ->
    HumanConcreteCapability(9,
      "Evaluate fire radiant energy",
      "evaluate_fire_radiant_energy",
    Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
    Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  repository = repository + ("declare_alarm_state" ->
    HumanConcreteCapability(10,
      "Declare alarm state",
      "declare_alarm_state",
    Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
    Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
  )

  repository = repository + ("ensure_presence_of_qualified_personnel" ->
    ServiceConcreteCapability(11,
      "Ensure presence qualified personnel",
      "ensure_presence_of_qualified_personnel",
      "nettunit.handler.ensure_presence_of_qualified_personnel",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
    )

  repository = repository + ("ensure_presence_of_representative" ->
    ServiceConcreteCapability(12,
      "Ensure presence representative",
      "ensure_presence_of_representative",
      "nettunit.handler.ensure_presence_of_representative",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
    )

  repository = repository + ("do_crossborder_communication" ->
    ServiceConcreteCapability(13,
      "Do crossborder communication",
      "do_crossborder_communication",
      "nettunit.handler.do_crossborder_communication",
      Some("nettunit.listener.TaskStartedExecutionListenerImpl"),
      Some("nettunit.listener.TaskEndedExecutionListenerImpl"))
    )

}
