package org.icar.solver

import org.icar.solver.best_first.WTSGraph
import org.icar.solver.montecarlo.WTSTreeNode
import org.icar.symbolic.AbstractWorkflow

abstract class SolverOutcome

case class FullSolutions(full:List[AbstractWorkflow],iterations:Int,elapsed:Long) extends SolverOutcome
case class PartialSolutions(partial:List[WTSGraph],iterations:Int,elapsed:Long) extends SolverOutcome

case class PartialTree(tree_root:WTSTreeNode, solutions: List[WTSTreeNode], best_partial_solution : Option[WTSTreeNode], iterations:Int, elapsed:Long) extends SolverOutcome

case class SolverError(msg:String,iterations:Int,elapsed:Long) extends SolverOutcome
