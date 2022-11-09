package org.icar.solver

import org.icar.solver.best_first.{WTSNode, WTSGraph}
import org.icar.subsymbolic.builder.SubLogicBuilder
import org.icar.subsymbolic.rete.RETE
import org.icar.symbolic.AbstractWorkflow

import scala.collection.immutable.TreeSet

abstract class SolverOutcome

case class FullSolutions(full:List[AbstractWorkflow],iterations:Int,elapsed:Long) extends SolverOutcome
case class PartialSolutions(partial:List[AbstractWorkflow],iterations:Int,elapsed:Long) extends SolverOutcome
case class SolverError(msg:String,iterations:Int,elapsed:Long) extends SolverOutcome
