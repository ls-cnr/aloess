package org.icar.domain.nettunit

import org.icar.business_process.converter.Solution2FlowableBPMN
import org.icar.grounder.SolutionGrounder
import org.icar.grounder.grounding_strategy.TabuGroundingStrategy
import org.icar.solver.best_first.BestFirstSolver
import org.icar.solver.{FullSolutions, IterationTermination, PartialTSWGraph, SolverError}
import org.icar.symbolic.builder.PropositionBuilder
import org.icar.symbolic.parser.{AbstractCapabilityParser, DomainOntologyParser, GoalTreeParser}
import org.icar.symbolic.{AbstractCapability, AtomTerm, Proposition, StateOfWorld}

import scala.io.Source

class nettunit {
  private val dir = "app/src/main/resources/domain/nettunit"
  private val domain_parser = new DomainOntologyParser
  private val goal_parser = new GoalTreeParser

  private val domain_string =Source.fromFile(s"$dir/domain.onto").getLines().mkString
  private val onto_parser_result = domain_parser.parseAll(domain_parser.domain,domain_string)
  val onto = onto_parser_result.get

  private val goal_model_string =Source.fromFile(s"$dir/goal_model.gm").getLines().mkString
  private val goal_parser_result = goal_parser.parseAll(goal_parser.goal_tree, goal_model_string)
  val goal_model = goal_parser_result.get

  private val act_int_security_plan = get_capability("activate_internal_security_plan.cap")
  private val decide_response_type = get_capability("decide_response_type.cap")
  private val declare_alarm_state = get_capability("declare_alarm_state.cap")
  private val declare_pre_alert_state = get_capability("declare_pre_alert_state.cap")
  private val do_crossborder_communication = get_capability("do_crossborder_communication.cap")
  private val ens_pre_qualified_personnel = get_capability("ensure_presence_of_qualified_personnel.cap")
  private val ens_pre_representative = get_capability("ensure_presence_of_representative.cap")
  private val eva_fire_radiant_energy = get_capability("evaluate_fire_radiant_energy.cap")
  private val inf_tech_rescue_organisation_alert = get_capability("inform_technical_rescue_organisation_alert.cap")
  private val inf_tech_rescue_organisation_internal_plan = get_capability("inform_technical_rescue_organisation_internal_plan.cap")
  private val keep_update_involved_personnel = get_capability("keep_update_involved_personnel.cap")
  private val notify_competent_body_internal_plan = get_capability("notify_competent_body_internal_plan.cap")
  private val prepare_tech_report = get_capability("prepare_tech_report.cap")
  private val send_team_to_evaluate = get_capability("send_team_to_evaluate.cap")

  val cap_repository = List(
    act_int_security_plan,
    decide_response_type,
    declare_alarm_state,
    declare_pre_alert_state,
    do_crossborder_communication,
    ens_pre_qualified_personnel,
    ens_pre_representative,
    eva_fire_radiant_energy,
    inf_tech_rescue_organisation_alert,
    inf_tech_rescue_organisation_internal_plan,
    keep_update_involved_personnel,
    notify_competent_body_internal_plan,
    prepare_tech_report,
    send_team_to_evaluate
  )

  private def get_capability(cap_name : String) : AbstractCapability = {
    val cap_string =Source.fromFile(s"$dir/$cap_name").getLines().mkString
    val capability_parser = new AbstractCapabilityParser
    val capability_parser_result = capability_parser.parseAll(capability_parser.capability,cap_string)
    capability_parser_result.get
  }
}


object RunNETTUNIT_Solver_Grounder extends App {
  val formula_builder = new PropositionBuilder()
  val mynet = new nettunit
  val myonto = mynet.onto
  val myrep = mynet.cap_repository
  val mygoal_model = mynet.goal_model
  val solver = new BestFirstSolver(myonto,myrep,mygoal_model,None)

  val emergency = formula_builder.proposition("emergency_location",List(AtomTerm("refinery"))).asInstanceOf[Proposition]
  val solver_result = solver.run(StateOfWorld(List(emergency)), IterationTermination(30))

  solver_result match {
    case FullSolutions(full, iterations, elapsed) =>
      println(s"*** ${full.size} FULL SOLUTIONS ($elapsed ms, $iterations its) ***")

      val selected = full.head

      val grounding_strategy = new TabuGroundingStrategy(1)
      val sol_grounder = new SolutionGrounder(netunit_concrete_repository.get_repo,grounding_strategy)
      val grounded = sol_grounder.groundSolution(selected)

      val converter = new Solution2FlowableBPMN(grounded)
      val process = converter.getBPMN(selected.name,"NETTUNITProcess")
      println(process)

    case PartialTSWGraph(partial, iterations, elapsed) =>
      println(s"*** ${solver.solution_set.size} PARTIAL SOLUTIONS ($elapsed ms, $iterations its) ***")
      println(solver.stringIterationGraphviz(6))
    case SolverError(msg, iterations, elapsed) =>
      println("*** ERROR ***")
    case _ =>
  }

}
