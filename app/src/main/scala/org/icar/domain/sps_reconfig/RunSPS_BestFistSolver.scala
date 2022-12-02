package org.icar.domain.sps_reconfig

import org.icar.solver.{FullSolutions, IterationTermination, PartialTSWGraph, SolverError}
import org.icar.solver.best_first.BestFirstSolver


object Simulate_SmallCircuit extends App {
  val builder = new SPSCircuitBuilder
  val circuit = builder.sample_circuit
  val mission = builder.sample_circuit_mission
  val initial = builder.sample_circuit_initial
  val sps_metric = new ForceField(circuit,mission)

  val solver = new BestFirstSolver(circuit.onto,circuit.cap_repository , circuit.goal_model(mission),Some(sps_metric))
  solver.manual_run(initial)

  val zero_node = solver.manual_get_node(0).get
  val close_sw_1 = solver.manual_get_action(4).get
  solver.manual_iteration(zero_node,close_sw_1)

  println(solver.stringIterationGraphviz(0))

  val uno_node = solver.manual_get_node(1).get
  val mutex_1_pos2 = solver.manual_get_action(0).get
  solver.manual_iteration(uno_node,mutex_1_pos2)

  println(solver.stringIterationGraphviz(1))
}


object RunSPS_sample_circuit_BestFistSolver extends App {
  // Small Circuit
  val builder = new SPSCircuitBuilder
  val circuit = builder.sample_circuit
  val mission = builder.sample_circuit_mission
  val initial = builder.sample_circuit_initial

  //circuit.print_for_graphviz()

  val sps_metric = new ForceField(circuit,mission)
  val solver = new BestFirstSolver(circuit.onto,circuit.cap_repository , circuit.goal_model(mission),Some(sps_metric))

  val solver_result = solver.run(initial, IterationTermination(6))
  solver_result match {
    case FullSolutions(full, iterations, elapsed) =>
      println("*** FULL SOLUTIONS ***")
      full.foreach( s => println(s.stringGraphviz()) )
    case PartialTSWGraph(partial, iterations, elapsed) =>
      println("*** 0 FULL SOLUTIONS ***")
      println(s"*** ${partial.size} PARTIAL SOLUTIONS ***")
      println(solver.stringIterationGraphviz(6))
    case SolverError(msg, iterations, elapsed) =>
      println("*** ERROR ***")
    case _ =>
  }


}


object RunSPS_circuit3_BestFistSolver extends App {
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
      full.foreach( s => println(s.stringGraphviz()) )
    case PartialTSWGraph(partial, iterations, elapsed) =>
      println("*** 0 FULL SOLUTIONS ***")
      println(s"*** ${partial.size} PARTIAL SOLUTIONS ***")
    case SolverError(msg, iterations, elapsed) =>
      println("*** ERROR ***")
    case _ =>
  }
}
