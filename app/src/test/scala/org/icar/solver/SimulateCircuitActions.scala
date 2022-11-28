package org.icar.solver

import org.icar.domain.sps_reconfig.RunSPS_sample_circuit_BestFistSolver.{circuit, initial, mission, solver, sps_metric}
import org.icar.domain.sps_reconfig.{ForceField, SPSCircuitBuilder}
import org.icar.solver.best_first.BestFirstSolver
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class SimulateCircuitActions extends AnyFunSuite {


  test("small circuit apply solution") {
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

    println(solver.stringIterationGraphviz(1))

  }
}
