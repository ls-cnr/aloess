package org.icar.solver.best_first

import org.icar.solver._
import org.icar.subsymbolic.{RawAction, RawState}
import org.icar.symbolic._

import scala.collection.immutable.TreeSet

class BestFirstSolver(onto:DomainOntology,abstract_repo : List[AbstractCapability], goal_model : GoalTree, metric : DomainMetric) extends AbstractSolver(onto,abstract_repo,goal_model) {
  var solution_set : List[WTSGraph] = List.empty
  val available_actions = sub_actions.actions
  val node_builder = new GlobalNodeBuilder(metric)
  val goal_map_merger = new GoalMapMerger(goal_model.root)

  def num_complete_solutions: Int = 0
  def get_full_solutions: List[AbstractWorkflow] = List.empty

  override def run(start:StateOfWorld, termination:TerminationCondition) : SolverOutcome = {
    val start_timestamp: Long = System.currentTimeMillis
    var n_iteration = 0

    solution_set = init_solution_set(start)

    while (!termination.check_termination(start_timestamp,n_iteration,num_complete_solutions)) {
      iteration
      println(s"/** iteration $n_iteration **/")
      println(stringGraphviz)
      n_iteration += 1
    }

    val end_timestamp: Long = System.currentTimeMillis
    val elapsed = end_timestamp-start_timestamp
    get_full_solutions

    SolverError("in-work",n_iteration,elapsed)
  }

  def stringGraphviz: String = {
    var string = ""
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

    List(WTSGraph.start_graph(start_node,goal_model.root,goal_map))
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

      val actions : List[RawAction] = applicable_capabilities(focus_node)

      if (actions.nonEmpty) {
        update_solution_set(focus_node,actions)
      }
    }
  }

  private def get_next_node : Option[WTSNode] = {
    val optwts = most_promising_wts
    if (optwts.isDefined)
      optwts.get.most_promising_node
    else
      None
  }
  private def most_promising_wts : Option[WTSGraph] = {
    var best_node_until_now : Option[WTSGraph] = None
    for (wts <- solution_set if !wts.is_full_solution) {
      if (best_node_until_now.isDefined) {
        if (wts.graph_label.quality_of_solution > best_node_until_now.get.graph_label.quality_of_solution)
          best_node_until_now = Some(wts)
      } else {
        best_node_until_now = Some(wts)
      }
    }
    best_node_until_now
  }

  /** a capability is applicable if the current state of world satisfies its precondition */
  private def applicable_capabilities(node : WTSNode) : List[RawAction] = {
    val state = node.memory.stable_state
    for (a<-available_actions if a.pre.satisfied_in(state)) yield a
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
      if (wts.is_interested_to(node)) {
        val split_wts = apply_expansion(wts,node,exp_list)
        new_solution_set = new_solution_set ::: split_wts
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

    for (exp <- exp_list) {
      val wts_exp_list = prepare_wts_expansion(wts,focusnode,exp)
      val pre_exp_test = check_goal_violations(wts_exp_list)
      if (pre_exp_test) {
         val post_exp_test = check_invariants(wts,focusnode,wts_exp_list)
          if (post_exp_test) {
          val updated_wts : WTSGraph = expand_wts(wts,focusnode,wts_exp_list)
          result = updated_wts :: result
        }
      }
    }

    if (result.nonEmpty)
      result
    else
      List(wts)
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

      val trajectory = rete.evolution(focusnode.memory,effect)

      val dest_node : WTSNode = node_builder.get_or_create_node(trajectory)

      if (!wts.node_label.contains(focusnode.id)) {
        println("error here")
      }
      val updated_map = update_goal_map(wts,focusnode,dest_node)

      val invariants = exp.invariant :: wts.node_label(focusnode.id).invariants

      val node_label = NodeLabelling(updated_map,true,invariants)
      val tx = WTSTransition(tx_counter,focusnode.id,dest_node.id)
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

  /**
   * apply all the expansions to the same wts thus to generate its updated version
   * each expansion adds
   * 1) a new frontier node
   * 2) a new transition from the focus node to the new node
   * 3) labels for node and trransition
   * note: the whole graph labeling is changed consequently to
   * 1) update the frontier data structure
   * 2) check if the wts represents a full solution
   */
  private def expand_wts(wts: WTSGraph, focusnode:WTSNode, exp_list: List[DecodeExpansion]) : WTSGraph = {
    var nodes : List[WTSNode] = wts.nodes
    var transitions : List[WTSTransition] = wts.transitions
    var node_labels : Map[Int,NodeLabelling] = wts.node_label
    var tx_labels : Map[Int,TxLabelling] = wts.tx_label
    var frontier : TreeSet[WTSNode] = wts.frontier
    var visited : List[WTSNode] = wts.visited

    var wts_goal_map : Map[String,GoalState] = wts.graph_label.wts_goals.map

    for (exp <- exp_list) {
      nodes = exp.node :: nodes
      transitions = exp.tx :: transitions
      node_labels = node_labels + (exp.node.id -> exp.nodelabel)
      val node_goal_state = exp.nodelabel.node_goals.map

      wts_goal_map = goal_map_merger.merge_maps(wts_goal_map,node_goal_state)

      tx_labels = tx_labels + (exp.tx.id -> exp.txlabel)
      frontier = frontier - focusnode + exp.node
      visited = focusnode :: visited
    }

    val quality_sol: Double = calculate_quality_of_solution(wts_goal_map)
    val graph_label = GraphLabelling(wts.graph_label.root_goal,GoalModelMap(goal_model.root.id,wts_goal_map),quality_sol)
    WTSGraph(wts.start,nodes,transitions,graph_label,node_labels,tx_labels,frontier,visited)
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
        root_state.satisf match {
          case Violation() => no_goal_violation = false
          case _ =>
        }
      }
    }
    no_goal_violation
  }

  def calculate_quality_of_solution(goal_map: Map[String, GoalState]): Double = {
    val root_state = goal_map(goal_model.root.id)
    root_state.satisf match {
      case FullSatisfaction() => metric.max
      case PartialSatisfaction(degree) => degree
      case Violation() => metric.min
      case _ => metric.min
    }
  }


  case class DecodeExpansion(node:WTSNode, nodelabel:NodeLabelling, tx:WTSTransition, txlabel:TxLabelling)
}

