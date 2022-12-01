package org.icar.solver.montecarlo

import org.icar.solver.{AbstractSolver, FullSolutions, PartialTree, SolverError, SolverOutcome, TerminationCondition}
import org.icar.symbolic.{AbstractCapability, DomainOntology, GoalTree, StateOfWorld}

import scala.util.Random

class MonteCarloSolver(onto:DomainOntology,abstract_repo : List[AbstractCapability], goal_model : GoalTree) extends AbstractSolver(onto,abstract_repo,goal_model) {
  val random = new Random()

  var new_nodes_in_frontier : List[WTSTreeNode] = List.empty
  var root_iterator : Int = 0

  override def run(start: StateOfWorld, termination: TerminationCondition): SolverOutcome = {
    var solution_nodes : List[WTSTreeNode] = List.empty

    var frontier : List[WTSTreeNode] = List()
    var root_probability : List[Double] = List()

    val wi = sub_logic.state(start)
    val tree = new WTSTreeBuilder(wi, rete_builder, sub_actions, goal_state_builder)

    var best_partial_solution : Option[WTSTreeNode] = Some(tree.root)

    val start_timestamp: Long = System.currentTimeMillis
    var n_iteration = 0

    while (!termination.check_termination(start_timestamp, n_iteration, solution_nodes.size )) {
      if (frontier.isEmpty) {
        /* add n times the root as frontier, where n is the branch factor */
        tree.root.children.foreach({
          _ => frontier = tree.root :: frontier
        })
      }

      /* calculate the probability to pick a root node from the frontier */
      var counter = 0
      for (f <- frontier if f == tree.root) counter += 1
      val prob = counter.toDouble / frontier.size.toDouble
      root_probability = prob :: root_probability

      /* pick a node from the frontier */
      val focus_node = frontier.head
      frontier = frontier.tail
      if (focus_node.is_root)
        root_iterator += 1

      /* explore in the depth a path of the wts until there is a change in goal satisfaction */
      val deep_node = simulation_until_delta(tree, focus_node)

      /* check if it is a full solution */
      if (deep_node.is_exit) {
        solution_nodes = deep_node :: solution_nodes

        /* otherwise check if the deep_node improves the best_partial_solution so far */
      } else if (deep_node.improve_goal_satisfaction) {

          if (deep_node.supervisor.degree < best_partial_solution.get.supervisor.degree)
            best_partial_solution = Some(deep_node)

      }

      backpropagation_with_frontier(deep_node, deep_node.improve_goal_satisfaction, deep_node.children.size)
      if (new_nodes_in_frontier.nonEmpty) {
        frontier = new_nodes_in_frontier ::: frontier
        new_nodes_in_frontier = List.empty
      }

      frontier = Random.shuffle(frontier)
      n_iteration += 1

    }

    val end_timestamp: Long = System.currentTimeMillis
    val elapsed = end_timestamp - start_timestamp

    val converter = new WTSTree2Solution(sub_actions.action_register)
    if (solution_nodes.nonEmpty) {
      val solutions = for (s <- solution_nodes) yield converter.build_abstract(s)
      FullSolutions(solutions,n_iteration,elapsed)
    } else if (best_partial_solution.isDefined) {
      val best_partial = converter.build_abstract(best_partial_solution.get)
      PartialTree(tree.root, best_partial,  n_iteration, elapsed)
    } else {
      SolverError("Error",n_iteration, elapsed)
    }
  }

  def simulation_until_delta(tree:WTSTreeBuilder, node: WTSTreeNode):WTSTreeNode = {
    val r2s_ref:Double = node.supervisor.get_root_state.sat_degree
    var delta:Double = 0
    var focus_node = node

    /* go into depth until there is a change and it is possible */
    while (delta==0 && focus_node.is_nonterminal) {
      focus_node = expand_or_child(tree,focus_node)
      val focus_r2s = focus_node.supervisor.get_root_state.sat_degree
      delta = r2s_ref-focus_r2s
    }

    /* if delta > 0 then we want to remember new node is better than the previous one */
    if (delta>0)
      focus_node.improve_goal_satisfaction=true

    focus_node
  }

  def expand_or_child(tree:WTSTreeBuilder, node: WTSTreeNode) : WTSTreeNode = {
    val untried: Array[Int] = node.untried_actions
    if (!untried.isEmpty) {
      /* pick a possible action among the possible ones */
      val action_index : Int = random.nextInt(untried.size)

      /* expand the current node by the selected action */
      val expanded_child: WTSTreeNode = tree.get_node_child(node,untried(action_index))


      expanded_child

    } else {
      /* if the current node has been fully explored, then it returns one of its children */
      val child_index : Int = random.nextInt(node.children.size)
      val child = tree.get_node_child(node,child_index)
      child
    }
  }

  def backpropagation_with_frontier(node: WTSTreeNode, win : Boolean, magnitude : Int) : Unit = {

    // increment the node 'interest'
    node.visit += 1

    /* mark if coming from an increment of goal sat */
    if (win){
      node.win += 1

      /* increase chances to be selected again */
      if (node.improve_goal_satisfaction) {
        new_nodes_in_frontier = List.fill(magnitude)(node)
      }

    }
    if (!node.is_root) {
      val half_magnitude : Double = magnitude.toDouble / 2.0
      val round_magnitude : Long = Math.round(half_magnitude)
      val round_min1 : Int = Math.max( round_magnitude.toInt, 1)

      /* repeat until the root node */
      backpropagation_with_frontier(node.parent, win, round_min1)
    }

  }






/*
  private def tree_policy(tree: WTSTreeBuilder):WTSTreeNode = {
    val root = tree.root
    var focus : WTSTreeNode = root
    var new_node : WTSTreeNode = root
    while (new_node==root && focus.is_nonterminal) {
      if (focus.is_notfullyexpanded) {
        new_node = expand(tree, focus)
      } else {
        focus = best_child(focus)
      }
    }
    if (new_node != root)
      new_node
    else
      focus
  }

  private def expand(tree: WTSTreeBuilder, node: WTSTreeNode):WTSTreeNode = {
    val untried : Array[Int] = node.untried_actions
    val i : Int = random.nextInt(untried.size)
    val child = tree.get_node_child(node,untried(i))
    if (child.is_exit) num_complete_solutions += 1
    child
  }

  private def best_child(node: WTSTreeNode): WTSTreeNode = {
    var best = node
    var best_score : Double = -1
    for (some_child<-node.children if some_child.isDefined) {
      val child = some_child.get
      val win = child.win
      val visit = child.visit
      val R : Double = if (child.visit>0) win/visit else 0
      if (R>best_score) {
        best_score = R
        best = child
      }
    }
    best
  }

  def simulation_policy(tree: WTSTreeBuilder, node: WTSTreeNode):Int = {
    var focus_node = node
    while (focus_node.is_notfullyexpanded && focus_node.is_nonterminal) {
      focus_node = expand(tree,focus_node)
    }
    if (focus_node.is_exit)
      1
    else
      0
  }
  def backpropagation(node: WTSTreeNode, win: Int): Unit = {
    node.visit += 1
    node.win += win

    if (!node.is_root)
      backpropagation(node.parent,win)
  }
*/

}
