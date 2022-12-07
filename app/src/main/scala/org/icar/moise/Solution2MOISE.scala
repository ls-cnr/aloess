package org.icar.moise

import org.icar.domain.nettunit.nettunit
import org.icar.solver.{FullSolutions, IterationTermination, PartialTSWGraph, SolverError}
import org.icar.solver.best_first.BestFirstSolver
import org.icar.symbolic.builder.PropositionBuilder
import org.icar.symbolic.{AbstractCapability, AbstractWorkflow, AtomTerm, CapabilityTask, GoalModel, Proposition, StateOfWorld}

import scala.xml.Elem

case class YellowPageEntry(agent_id:String, abstractCapability: AbstractCapability)

class Solution2MOISE(yellowpage:List[YellowPageEntry]) {
  var group_num = 1
  var scheme_num = 1
  var mission_num = 1
  var norm_num = 1
  var dummy_goal_num = 0

  var management_mission : List[String] = List.empty
  var mission_map : Map[String,AbstractCapability] = Map.empty

  def moise_spec(abstract_workflows : List[AbstractWorkflow], goalmodel:GoalModel) : Elem = {
    apply_rule__main_template(abstract_workflows)
  }

  private def apply_rule__main_template(solutions: List[AbstractWorkflow]): Elem = {
    <organisational-specification>
      <structural-specification>
        <role-definitions>
          <role id="manager"/>
          <role id="worker"/>{yellowpage.map(i => <role id={i.abstractCapability.id + "_role"}>
          <extends role="worker"/>
        </role>)}
        </role-definitions>
        <group-specification>
          <roles>
            <role id="manager" min="1" max="1"></role>
          </roles>
          <subgroups>
            {solutions.map(sol => apply_rule__group_specification(sol))}
          </subgroups>
          <formation-constraints>
            <cardinality object="role" id="manager"/>
            <compatibility from="worker" to="worker" extends-subgroups="false"></compatibility>
          </formation-constraints>
        </group-specification>
      </structural-specification>

      <functional-specification>
        {solutions.map(s => apply_rule__scheme(s))}
      </functional-specification>

      <normative-specification>
        {management_mission.map( m => apply_rule__management_norm(m))++mission_map.map(m => apply_rule__mission_norm(m)) }
      </normative-specification>
    </organisational-specification>

  }

  private def apply_rule__group_specification(sol: AbstractWorkflow): Elem = {
    <group-specification id={get_group_id} min="0">
      {apply_rule__group_roles(sol) ++ apply_rule__group_links(sol)}
    </group-specification>
  }
  private def apply_rule__group_roles(s: AbstractWorkflow): Elem = {
    <roles>
      <role id="manager" min="1" max="1"/>{s.items.filter(i => i.isInstanceOf[CapabilityTask]).map(t =>
        <role id={t.asInstanceOf[CapabilityTask].cap.id + "_role"} min="1" max="1"/>
    )}
    </roles>
  }
  private def apply_rule__group_links(s: AbstractWorkflow): Elem = {
    <links>
      <link from="manager" to="worker" type="authority" extends-subgroups="false" bi-dir="false" scope="intra-group"/>
      <link from="worker" to="manager" type="acquaintance" extends-subgroups="false" bi-dir="false" scope="intra-group"/>
    </links>
  }

  private def apply_rule__scheme(s: AbstractWorkflow): Elem = {
    dummy_goal_num = 0
    val sol_tree = new SolutionPattern(s)
    val opt_tree: Option[WorkflowPattern] = sol_tree.root_pattern._1

    if (opt_tree.isDefined) {
      val tree : WorkflowPattern = opt_tree.get

      val capabilities = for (i<-s.items if i.isInstanceOf[CapabilityTask]) yield i.asInstanceOf[CapabilityTask].cap

      val scheme_id = get_scheme_id
      <scheme id={scheme_id}>
        {apply_rule__plan(tree)++apply_rule__management_mission(scheme_id)++capabilities.map(c=>apply_rule__mission(c))}
      </scheme>
    } else {
      <scheme id={get_scheme_id}>
      </scheme>
    }
  }

  private def apply_rule__plan(pattern: WorkflowPattern) : Elem = {
    pattern match {
      case SequencePattern(children) =>
        <goal id={get_dummy_goal_id}>
          <plan operator="sequence">
            {children.map(c => apply_rule__plan(c))}
          </plan>
        </goal>

      case ChoicePattern(children) =>
        <goal id={get_dummy_goal_id}>
          <plan operator="choice">
            {children.map(c => apply_rule__plan(c))}
          </plan>
        </goal>

      case LoopPattern(before_check, after_check, exit) =>
        <goal id={get_dummy_goal_id}>
          <plan operator="sequence" type="maintain">
            {before_check.map(c => apply_rule__plan(c))}
            <goal id={"check_exit_"+get_dummy_goal_id}></goal>
            {after_check.map(c => apply_rule__plan(c))}
          </plan>
        </goal>


      case ActivityPattern(task) =>
        val id = task.cap.id
        <goal id={id}></goal>

      case _ =>
        <goal id={"unsup_"+get_dummy_goal_id}></goal>
    }
  }

  private def apply_rule__mission(cap: AbstractCapability) : Elem = {
    val miss_id = get_mission_id
    mission_map += (miss_id -> cap)
    <mission id={miss_id} min="1" max="1">
      <goal id={cap.id}></goal>
    </mission>
  }

  private def apply_rule__management_mission(scheme_id:String) : Elem = {
    val mng_id = "management_"+scheme_id
    management_mission = mng_id :: management_mission
    <mission id={"management_"+scheme_id} min="1" max="1">
      <goal id="root_goal"></goal>
    </mission>
  }

  private def apply_rule__management_norm(m: String): Elem = {
      <norm id={get_norm_id} type="permission" role="manager" mission={m}/>
  }

  private def apply_rule__mission_norm(m: (String, AbstractCapability)): Elem = {
      <norm id={get_norm_id} type="obligation" role={"role_"+m._2.id} mission={m._1}/>
  }

  private def get_group_id = {
    val id = "group" + group_num
    group_num += 1
    id
  }

  private def get_scheme_id = {
    val id = "scheme" + scheme_num
    scheme_num += 1
    id
  }

  private def get_mission_id = {
    val id = "mission" + mission_num
    mission_num += 1
    id
  }
  private def get_norm_id = {
    val id = "norm" + norm_num
    norm_num += 1
    id
  }

  private def get_dummy_goal_id = {
    val id = if (dummy_goal_num==0) "root_goal" else "goal_" + dummy_goal_num
    dummy_goal_num += 1
    id
  }

}




object RunNETTUNIT_Sol2MOISE extends App {
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

      val conv = new Solution2MOISE(List())
      val moise_schema = conv.moise_spec(full,mygoal_model)
      println(moise_schema)


    case PartialTSWGraph(partial, iterations, elapsed) =>
      println(s"*** ${solver.solution_set.size} PARTIAL SOLUTIONS ($elapsed ms, $iterations its) ***")
      println(solver.stringIterationGraphviz(6))
    case SolverError(msg, iterations, elapsed) =>
      println("*** ERROR ***")
    case _ =>
  }

}
