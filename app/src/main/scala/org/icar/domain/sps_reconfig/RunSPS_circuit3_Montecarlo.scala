package org.icar.domain.sps_reconfig

import org.icar.solver.montecarlo.MonteCarloSolver
import org.icar.solver.{FullSolutions, PartialTree, SolverError, TimeTermination}

import java.io.{File, PrintWriter}

object RunSPS_circuit3_Montecarlo extends App {
  // Medium Circuit "circuit/circuit3.txt"
  val builder = new SPSCircuitBuilder
  val circuit = builder.build_from_file("app/src/main/resources/domain/sps_reconfig/circuit/circuit3.txt") //prepare_circuit
  val mission = builder.circuit_3_mission
  val initial = builder.circuit_3_initial_totally_switched_off//circuit_3_initial_simple_failure

  val sps_metric = new ForceField(circuit,mission)

  val goal_tree = circuit.goal_model(mission)

  val solver = new MonteCarloSolver(circuit.onto, circuit.cap_repository, goal_tree)
  val solver_result = solver.run(initial,TimeTermination(1000))//IterationTermination(400))//
  solver_result match {
    case FullSolutions(full, iterations, elapsed) =>
      println("*** FULL SOLUTIONS ***")
      full.foreach( x => println(x.stringGraphviz()))
    case PartialTree(tree_root, best_partial, iterations, elapsed) =>
      val file = "app/src/main/resources/domain/sps_reconfig/mcs/tree.dot"
      println("*** BEST PARTIAL SOLUTION ***")
      println(best_partial.stringGraphviz())
      println(s"complete tree in file $file")
      val pw = new PrintWriter(new File(file))
      pw.write(tree_root.stringGraphviz())
      pw.close
    case SolverError(msg, iterations, elapsed) =>
      println("*** ERROR ***")
  }
}
