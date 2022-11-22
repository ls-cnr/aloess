package org.icar.solver

import org.icar.solver.best_first.WTSGraph
import org.icar.symbolic.AbstractWorkflow

abstract class SolverOutcome

case class FullSolutions(full:List[WTSGraph], iterations:Int, elapsed:Long) extends SolverOutcome
//case class FullSolutions(full:List[AbstractWorkflow],iterations:Int,elapsed:Long) extends SolverOutcome
case class PartialSolutions(partial:List[WTSGraph],iterations:Int,elapsed:Long) extends SolverOutcome
//case class PartialSolutions(partial:List[AbstractWorkflow],iterations:Int,elapsed:Long) extends SolverOutcome
case class SolverError(msg:String,iterations:Int,elapsed:Long) extends SolverOutcome
