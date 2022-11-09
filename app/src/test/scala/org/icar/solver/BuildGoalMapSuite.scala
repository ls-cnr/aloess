package org.icar.solver

import org.icar.subsymbolic._
import org.icar.subsymbolic.builder.SubLogicBuilder
import org.icar.symbolic.builder.PropositionBuilder
import org.icar.symbolic.parser.{DomainOntologyParser, GoalTreeParser}
import org.icar.symbolic.{AtomTerm, Proposition, StateOfWorld}
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class BuildGoalMapSuite extends AnyFunSuite {


  test("build goal map") {

    val p = new DomainOntologyParser
    val pp = new GoalTreeParser

    val onto_parser = p.parseAll(p.domain, "domain \"prova5\" {  " +
      "category params atom [ p1,p2,p3,p4,p5,p6,p7,p8 ] " +
      "define func(enum[params])" +
      "define result(enum[params])" +
      "}")

    val onto = onto_parser.get
    val goal_model_string = "model { " +
      "goal g0 is and [" +
      "goal g1 is or [" +
      "goal g3 when func(p1) should address result(p2)   " +
      "goal g4 when func(p3) should address result(p4) " +
      "]  " +
      "goal g2 is and [" +
      "goal g5 when func(p5) should address result(p6)  " +
      "goal g6 when func(p7) should address result(p8)  " +
      "] " +
      "]  " +
      "}"
    val result = pp.parseAll(pp.goal_tree, goal_model_string)

    val goal_model = result.get

    val formula_builder = new PropositionBuilder()
    val ontobuilder = new SubLogicBuilder(onto)
    val mapbuilder = new GoalMapBuilder(ontobuilder, goal_model,new EffortToSatisf((goal_model)))
    val goalmodelmap_step1 = mapbuilder.create_goalmodel_map

    assert(goalmodelmap_step1.map.size == 7)
    assert(goalmodelmap_step1.map("g0").achievement == DependsOnSubgoals())
    assert(goalmodelmap_step1.map("g0").satisf == PartialSatisfaction(0.0))
    assert(goalmodelmap_step1.map("g1").achievement == DependsOnSubgoals())
    assert(goalmodelmap_step1.map("g1").satisf == PartialSatisfaction(0.0))
    assert(goalmodelmap_step1.map("g2").achievement == DependsOnSubgoals())
    assert(goalmodelmap_step1.map("g2").satisf == PartialSatisfaction(0.0))

    assert(goalmodelmap_step1.map("g3").achievement == Ready(RawProposition(0)))
    assert(goalmodelmap_step1.map("g3").satisf == PartialSatisfaction(0.0))
    assert(goalmodelmap_step1.map("g4").achievement == Ready(RawProposition(2)))
    assert(goalmodelmap_step1.map("g4").satisf == PartialSatisfaction(0.0))
    assert(goalmodelmap_step1.map("g5").achievement == Ready(RawProposition(4)))
    assert(goalmodelmap_step1.map("g5").satisf == PartialSatisfaction(0.0))
    assert(goalmodelmap_step1.map("g6").achievement == Ready(RawProposition(6)))
    assert(goalmodelmap_step1.map("g6").satisf == PartialSatisfaction(0.0))

    val f_p1 = formula_builder.proposition("func", List(AtomTerm("p1"))).asInstanceOf[Proposition]
    val r_p2 = formula_builder.proposition("result", List(AtomTerm("p2"))).asInstanceOf[Proposition]
    val raw_rp2 = ontobuilder.formula(r_p2)

    val node: RawState = ontobuilder.state(StateOfWorld(List(f_p1)))
    val goalmodelmap_step2 = mapbuilder.update_goalmap(node, goalmodelmap_step1)
    assert(goalmodelmap_step2.map("g3").achievement == Committed(RawUntil(RawTT(),RawProposition(9))))
    assert(goalmodelmap_step2.map("g3").switch_to_committ)

    val node2: RawState = ontobuilder.state(StateOfWorld(List(f_p1, r_p2)))
    val goalmodelmap_step3 = mapbuilder.update_goalmap(node2, goalmodelmap_step2)
    assert(goalmodelmap_step3.map("g3").achievement == Completed())
    assert(goalmodelmap_step3.map("g3").satisf == FullSatisfaction())
    assert(goalmodelmap_step3.map("g3").switch_to_satisf)
    assert(goalmodelmap_step3.map("g1").achievement == Completed())
    assert(goalmodelmap_step3.map("g1").satisf == FullSatisfaction())

    val f_p5 = formula_builder.proposition("func", List(AtomTerm("p5"))).asInstanceOf[Proposition]
    val r_p6 = formula_builder.proposition("result", List(AtomTerm("p6"))).asInstanceOf[Proposition]
    val raw_rp6 = ontobuilder.formula(r_p6)

    val node3: RawState = ontobuilder.state(StateOfWorld(List(r_p2, f_p5, r_p6)))
    val goalmodelmap_step4 = mapbuilder.update_goalmap(node3, goalmodelmap_step3)
    assert(goalmodelmap_step4.map("g5").achievement == Completed())
    assert(goalmodelmap_step4.map("g5").switch_to_committ)
    assert(goalmodelmap_step4.map("g5").switch_to_satisf)

  }


  test("merge goal map until satisfaction") {
    val p = new DomainOntologyParser
    val pp = new GoalTreeParser

    val onto_parser = p.parseAll(p.domain,"domain \"prova5\" {  " +
      "category params atom [ p1,p2,p3,p4,p5,p6,p7,p8 ] " +
      "define func(enum[params])" +
      "define result(enum[params])" +
      "}")

    val onto = onto_parser.get
    val goal_model_string = "model { " +
      "goal g0 is and [" +
      "goal g1 is or [" +
      "goal g3 when func(p1) should address result(p2)   " +
      "goal g4 when func(p3) should address result(p4) " +
      "]  " +
      "goal g2 is and [" +
      "goal g5 when func(p5) should address result(p6)  " +
      "goal g6 when func(p7) should address result(p8)  " +
      "] " +
      "]  " +
      "}"
    val result = pp.parseAll(pp.goal_tree,goal_model_string)

    val goal_model = result.get

    val ontobuilder = new SubLogicBuilder(onto)
    val mapbuilder = new GoalMapBuilder(ontobuilder,goal_model,new EffortToSatisf((goal_model)))
    val left_1 = mapbuilder.create_goalmodel_map
    val right_1 = mapbuilder.create_goalmodel_map

    val left_map = left_1.map - "g3" + ("g3" -> GoalState(Completed(), FullSatisfaction(),false,false))
    val right_map = right_1.map - "g4" + ("g4" -> GoalState(Completed(), FullSatisfaction(),false,false))

    val merger = new GoalMapMerger(goal_model.root)
    val merged = merger.merge_maps(left_map,right_map)
    assert(merged("g1").satisf==FullSatisfaction())
    assert(merged("g0").achievement==DependsOnSubgoals())

    val right_2 = mapbuilder.create_goalmodel_map
    val right_map2 = right_2.map - "g5" + ("g5" -> GoalState(Completed(), FullSatisfaction(),false,false)) - "g6" + ("g6" -> GoalState(Completed(), FullSatisfaction(),false,false))

    val merged2 = merger.merge_maps(merged,right_map2)
    assert(merged2("g0").satisf==FullSatisfaction())
  }

}
