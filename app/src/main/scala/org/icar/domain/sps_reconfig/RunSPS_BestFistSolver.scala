package org.icar.domain.sps_reconfig

import org.icar.solver.{FullSolutions, IterationTermination, PartialSolutions, SolverError}
import org.icar.solver.best_first.BestFirstSolver

object RunSPS_BestFistSolver extends App {
  // Medium Circuit "circuit/circuit3.txt"
  val builder = new SPSCircuitBuilder
  val circuit = builder.build_from_file("app/src/main/resources/domain/sps_reconfig/circuit/circuit3.txt") //prepare_circuit
  val mission = builder.circuit_3_mission
  val initial = builder.circuit_3_initial_totally_switched_off//circuit_3_initial_simple_failure

  val sps_metric = new ForceField(circuit,mission)
  val solver = new BestFirstSolver(circuit.onto,circuit.cap_repository , circuit.goal_model(mission),Some(sps_metric))

  val solver_result = solver.run(initial, IterationTermination(30))
  solver_result match {
    case FullSolutions(full, iterations, elapsed) =>
      println("*** FULL SOLUTIONS ***")
      full.foreach( s => println(s.stringGraphviz) )
    case PartialSolutions(partial, iterations, elapsed) =>
      println("*** 0 FULL SOLUTIONS ***")
      println(s"*** ${partial.size} PARTIAL SOLUTIONS ***")
    case SolverError(msg, iterations, elapsed) =>
      println("*** ERROR ***")
    case _ =>
  }


}
