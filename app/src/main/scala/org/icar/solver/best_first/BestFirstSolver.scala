package org.icar.solver.best_first

import org.icar.solver._
import org.icar.subsymbolic.{RawAction, RawState}
import org.icar.symbolic._

class BestFirstSolver(onto:DomainOntology,abstract_repo : List[AbstractCapability], goal_model : GoalTree, opt_metric : Option[DomainMetric]) extends AbstractSolver(onto,abstract_repo,goal_model) {
  var solution_set : List[WTSGraph] = List.empty
  val available_actions = sub_actions.actions
  val node_builder = new GlobalNodeBuilder(opt_metric)

  def get_complete_wts: List[WTSGraph] = solution_set.filter(x => x.is_full_solution)
  def get_full_solutions (wi: StateOfWorld): List[AbstractWorkflow] = {
    var output : List[AbstractWorkflow] = List.empty
    val complete = get_complete_wts
    for (wts <- complete) {
      val converter = new WTS2Solution(wts,sub_actions.action_register,wi,goal_model.meta)
      val solution = converter.build_abstract
      output = solution :: output
    }
    output
  }
  def num_complete_solutions: Int = solution_set.filter(x => x.is_full_solution).size

  override def run(start:StateOfWorld, termination:TerminationCondition) : SolverOutcome = {
    val start_timestamp: Long = System.currentTimeMillis
    var n_iteration = 0

    solution_set = init_solution_set(start)

    while (!termination.check_termination(start_timestamp,n_iteration,num_complete_solutions)) {
      iteration
      //println(stringIterationGraphviz(n_iteration))
      n_iteration += 1
    }

    val end_timestamp: Long = System.currentTimeMillis
    val elapsed = end_timestamp-start_timestamp
    val compl_solutions = get_full_solutions(start)

    if (compl_solutions.nonEmpty)
      FullSolutions(compl_solutions,n_iteration,elapsed)
    else {
      PartialTSWGraph(solution_set,n_iteration,elapsed)
      //SolverError("in-work",n_iteration,elapsed)
    }
  }
  def manual_run(start:StateOfWorld) : Unit = {
    solution_set = init_solution_set(start)
  }

  def stringIterationGraphviz(it:Int): String = {
    var string = s"/** iteration $it **/ \n"
    for (wts <- solution_set) string += wts.stringGraphviz + "\n"+wts.graph_label.wts_goals.stringGraphviz(goal_model)+"\n"
    string
  }

  /**
   * initialize the list of wts graphs by creating a new wts with just 1 node
   */
  private def init_solution_set(start:StateOfWorld) : List[WTSGraph] = {
    val raw_start: RawState = sub_logic.state(start)
    val memory = rete_builder.rete.reset_memory(raw_start)
    val start_node = node_builder.get_or_create_node(memory)
    var goal_map = goal_state_builder.create_goalmodel_map
    goal_map = goal_state_builder.update_goalmap(start_node.memory.stable_state,goal_map)

    List(WTSGraph.start_graph(start_node,goal_map))
  }

  /**
   * manage 1 iteration of the best-first search algorithm
   * 1) select the best node to expand
   * 2) select all the applicable actions
   * 3) apply the expansion
   */
  private def iteration : Unit = {
    val frontier_node : Option[WTSNode] = get_next_node
    if (frontier_node.isDefined) {
      val focus_node = frontier_node.get
      //println(focus_node)
      val actions : List[RawAction] = applicable_capabilities(focus_node)

      update_solution_set(focus_node,actions)
    }
  }
  def manual_iteration(focus_node: WTSNode, action : RawAction) : Unit = {
    update_solution_set(focus_node,List(action))
  }


  private def get_next_node : Option[WTSNode] = {
    if (opt_metric.isDefined)
      most_promising_node_by_score
    else {
      val optwts = most_promising_wts
      if (optwts.isDefined)
        optwts.get.most_promising_node(opt_metric)
      else
        None
    }
  }
  def manual_get_node(id : Int) : Option[WTSNode] = {
    var optnode : Option[WTSNode] = None

    for (wts <- solution_set if !optnode.isDefined) {
      optnode = wts.manual_get_node(id)
    }

    optnode
  }
  def manual_get_action(id : Int) : Option[RawAction] = {
    var optaction : Option[RawAction] = None

    for (a <- sub_actions.actions)
      if (a.id == id)
        optaction = Some(a)

    optaction
  }
  private def most_promising_wts : Option[WTSGraph] = {
    var best_graph_until_now : Option[WTSGraph] = None
    for (wts <- solution_set if !wts.is_full_solution) {
      if (best_graph_until_now.isDefined) {
        if (wts.graph_label.quality_of_solution < best_graph_until_now.get.graph_label.quality_of_solution)
          best_graph_until_now = Some(wts)
      } else {
        best_graph_until_now = Some(wts)
      }
    }
    best_graph_until_now
  }
  private def most_promising_node_by_score : Option[WTSNode] = {
    var score_until_now : Double = 0
    var best_node_until_node : Option[WTSNode] = None
    for (wts <- solution_set if !wts.is_full_solution) {
      val best_node_for_wts = wts.most_promising_node(opt_metric)
      if (best_node_for_wts.isDefined)
        if (best_node_for_wts.get.score > score_until_now) {
          score_until_now = best_node_for_wts.get.score
          best_node_until_node = best_node_for_wts
        }
    }
    best_node_until_node
  }

  /** a capability is applicable if the current state of world satisfies its precondition */
  private def applicable_capabilities(node : WTSNode) : List[RawAction] = {
    val state = node.memory.stable_state
    for (a<-available_actions if a.pre.satisfied_in(state) ) yield a
  }

//  private def encode_expansion(node:RawNode, action : RawAction) : RawExpansion = {
//    val rete = rete_builder.rete
//    val trajectory_list = for (effect <- action.effects) yield rete.evolution(node.memory,effect)
//    RawExpansion(node,action.id,trajectory_list,action.invariant)
//  }
//
  /**
   * for each pre-built wts that contains the current focus node
   * (and the wts is not yet a full solution)
   * this function apply the expansion potentially splitting a wts
   * into a set of branch wts
   */
  private def update_solution_set(node:WTSNode, exp_list:List[RawAction]) : Unit = {
    var new_solution_set : List[WTSGraph] = List.empty
    for (wts <- solution_set ) {
      val flag = wts.is_interested_to(node)
      if (flag) {
        val split_wts = apply_expansion(wts,node,exp_list)
        new_solution_set = new_solution_set ::: split_wts
      } else {
        new_solution_set = wts.wts_without_node_in_frontier(node) :: new_solution_set
      }
    }
    solution_set = new_solution_set
  }

  /**
   * each selected action produces a different version of the same wts
   * valid wts are those that
   * 1) do not violate node invariants (rules that must always hold)
   * 2) do not generate goal violations
   * if any of the actions produce a valid wts, then the original wts is sent back
   * */
  private def apply_expansion(wts:WTSGraph, focusnode:WTSNode, exp_list: List[RawAction]) : List[WTSGraph] = {
    var result : List[WTSGraph] = List.empty

    var counter : Int = 0
    for (exp <- exp_list) {
      val wts_exp_list = prepare_wts_expansion(wts,focusnode,exp)
      val pre_exp_test = check_goal_violations(wts_exp_list)
      if (pre_exp_test) {
         val post_exp_test = check_invariants(wts,focusnode,wts_exp_list)
          if (post_exp_test) {
            val updated_wts : WTSGraph = wts.expand_wts(counter.toString,focusnode,wts_exp_list,goal_map_merger,opt_metric)
            counter += 1
            result = updated_wts :: result
        }
      }
    }

//    result = wts.wts_without_node_in_frontier(focusnode) :: result
//    result
//
    if (result.nonEmpty)
      result
    else
      List(wts.wts_without_node_in_frontier(focusnode))
  }

  /**
   * decoding an expansion means calculating dest_node, transition and corresponding labels
   * the destination node is calculated by considering the action scenario trayectories
   * the node label is calculated by considering how goals changes
   * invariants are the sum of focus node invariants and action's ones
   * transition and its label are calculates straightforward
   */
  private def prepare_wts_expansion(wts: WTSGraph, focusnode: WTSNode, exp: RawAction) : List[DecodeExpansion] = {
    val rete = rete_builder.rete
    var result : List[DecodeExpansion] = List.empty

    var tx_counter = wts.transitions.size
    for (effect <- exp.effects) {

      val trajectory = rete.evolution(focusnode.memory,effect.evo)

      val dest_node : WTSNode = node_builder.get_or_create_node(trajectory)

      val updated_map = update_goal_map(wts,focusnode,dest_node)

      val invariants = exp.invariant :: wts.node_label(focusnode.id).invariants

      val node_label = NodeLabelling(updated_map,invariants)
      val tx = WTSTransition(tx_counter,exp.name,focusnode.id,dest_node.id)
      val txlabel = TxLabelling(exp.id,effect.name)
      tx_counter += 1

      result = DecodeExpansion(dest_node,node_label,tx,txlabel) :: result
    }

    result
  }

  /**
   * it simply delegates the GoalMapBuilder to update the goal-state according
   * to the new situation
   */
  private def update_goal_map(wts: WTSGraph, focusnode: WTSNode, dest_node: WTSNode) : GoalModelMap = {

    val focusmap = wts.node_label(focusnode.id).node_goals
    goal_state_builder.update_goalmap(dest_node.memory.stable_state,focusmap)
  }


  /** this test passes if all the new nodes respects all the invariants of the previous node */
  private def check_invariants(wts: WTSGraph, focusnode:WTSNode, exp_list: List[DecodeExpansion]) : Boolean = {
    var respect_invariants = true
    val invariants = wts.node_label(focusnode.id).invariants
    for (e <- exp_list if respect_invariants)
      for (form <- invariants)
        if (!form.satisfied_in(e.node.memory.stable_state))
          respect_invariants=false
    respect_invariants
  }
  /** this test passes if the root-goal does not produce violation in any of the new nodes  */
  private def check_goal_violations(exp_list : List[DecodeExpansion]): Boolean = {
    var no_goal_violation = true
    for (e <- exp_list ) {
      if (no_goal_violation) {
        val root_state = e.nodelabel.node_goals.map(goal_model.root.id)
        root_state.sat_state match {
          case Violation() => no_goal_violation = false
          case _ =>
        }
      }
    }
    no_goal_violation
  }



}

case class DecodeExpansion(node:WTSNode, nodelabel:NodeLabelling, tx:WTSTransition, txlabel:TxLabelling)
