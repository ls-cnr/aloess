package org.icar.solver

import org.icar.domain.AAL4E
import org.icar.partial_sat.EffortToSatisf
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
    assert(goalmodelmap_step1.map("g0").internal_state == DependsOnSubgoals())
    assert(goalmodelmap_step1.map("g0").sat_state == PartialSatisfaction())
    assert(goalmodelmap_step1.map("g1").internal_state == DependsOnSubgoals())
    assert(goalmodelmap_step1.map("g1").sat_state == PartialSatisfaction())
    assert(goalmodelmap_step1.map("g2").internal_state == DependsOnSubgoals())
    assert(goalmodelmap_step1.map("g2").sat_state == PartialSatisfaction())

    assert(goalmodelmap_step1.map("g3").internal_state == Ready(RawProposition(0)))
    assert(goalmodelmap_step1.map("g3").sat_state == PartialSatisfaction())
    assert(goalmodelmap_step1.map("g4").internal_state == Ready(RawProposition(2)))
    assert(goalmodelmap_step1.map("g4").sat_state == PartialSatisfaction())
    assert(goalmodelmap_step1.map("g5").internal_state == Ready(RawProposition(4)))
    assert(goalmodelmap_step1.map("g5").sat_state == PartialSatisfaction())
    assert(goalmodelmap_step1.map("g6").internal_state == Ready(RawProposition(6)))
    assert(goalmodelmap_step1.map("g6").sat_state == PartialSatisfaction())

    val f_p1 = formula_builder.proposition("func", List(AtomTerm("p1"))).asInstanceOf[Proposition]
    val r_p2 = formula_builder.proposition("result", List(AtomTerm("p2"))).asInstanceOf[Proposition]
    //val raw_rp2 = ontobuilder.formula(r_p2)

    val node: RawState = ontobuilder.state(StateOfWorld(List(f_p1)))
    val goalmodelmap_step2 = mapbuilder.update_goalmap(node, goalmodelmap_step1)
    assert(goalmodelmap_step2.map("g3").internal_state == Committed(RawUntil(RawTT(),RawProposition(9))))
    assert(goalmodelmap_step2.map("g3").switch_to_committ)

    val node2: RawState = ontobuilder.state(StateOfWorld(List(f_p1, r_p2)))
    val goalmodelmap_step3 = mapbuilder.update_goalmap(node2, goalmodelmap_step2)
    assert(goalmodelmap_step3.map("g3").internal_state == Completed())
    assert(goalmodelmap_step3.map("g3").sat_state == FullSatisfaction())
    assert(goalmodelmap_step3.map("g3").switch_to_satisf)
    assert(goalmodelmap_step3.map("g1").internal_state == Completed())
    assert(goalmodelmap_step3.map("g1").sat_state == FullSatisfaction())

    val f_p5 = formula_builder.proposition("func", List(AtomTerm("p5"))).asInstanceOf[Proposition]
    val r_p6 = formula_builder.proposition("result", List(AtomTerm("p6"))).asInstanceOf[Proposition]
    val raw_rp6 = ontobuilder.formula(r_p6)

    val node3: RawState = ontobuilder.state(StateOfWorld(List(r_p2, f_p5, r_p6)))
    val goalmodelmap_step4 = mapbuilder.update_goalmap(node3, goalmodelmap_step3)
    assert(goalmodelmap_step4.map("g5").internal_state == Completed())
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

    val left_map = left_1.map - "g3" + ("g3" -> GoalState(Completed(), FullSatisfaction(),100,false,false))
    val right_map = right_1.map - "g4" + ("g4" -> GoalState(Completed(), FullSatisfaction(),100,false,false))

    val merger = new GoalMapMerger(goal_model.root, new EffortToSatisf(goal_model))
    val merged = merger.merge_maps(left_map,right_map)
    assert(merged("g1").sat_state==FullSatisfaction())
    assert(merged("g0").internal_state==DependsOnSubgoals())

    val right_2 = mapbuilder.create_goalmodel_map
    val right_map2 = right_2.map - "g5" + ("g5" -> GoalState(Completed(), FullSatisfaction(),100,false,false)) - "g6" + ("g6" -> GoalState(Completed(), FullSatisfaction(),100,false,false))

    val merged2 = merger.merge_maps(merged,right_map2)
    assert(merged2("g0").sat_state==FullSatisfaction())
  }


  test("distance to satisfaction") {
    val onto = AAL4E.onto
    val goal_model = AAL4E.goal_model

    val ontobuilder = new SubLogicBuilder(onto)
    val mapbuilder = new GoalMapBuilder(ontobuilder, goal_model,new EffortToSatisf((goal_model)))
    val goalmodelmap_template = mapbuilder.create_goalmodel_map
    val formula_builder = new PropositionBuilder()

    val w0: RawState = ontobuilder.state(StateOfWorld(List()))
    val goalmodelmap_step1 = mapbuilder.update_goalmap(w0, goalmodelmap_template)

    val user_engage = formula_builder.proposition("user_engagement",List(AtomTerm("social"))).asInstanceOf[Proposition]

    val w1: RawState = ontobuilder.state(StateOfWorld(List(user_engage)))
    val goalmodelmap_step2 = mapbuilder.update_goalmap(w1, goalmodelmap_step1)

    val performed_social = formula_builder.proposition("performed",List(AtomTerm("social_activity"))).asInstanceOf[Proposition]
    val w2: RawState = ontobuilder.state(StateOfWorld(List(user_engage,performed_social)))
    val goalmodelmap_step3 = mapbuilder.update_goalmap(w2, goalmodelmap_step2)

    val registered_social = formula_builder.proposition("activity_registered",List(AtomTerm("done"))).asInstanceOf[Proposition]
    val w3: RawState = ontobuilder.state(StateOfWorld(List(user_engage,performed_social,registered_social)))
    val goalmodelmap_step4 = mapbuilder.update_goalmap(w3, goalmodelmap_step3)

    assert(goalmodelmap_step2.degree < goalmodelmap_step1.degree)
    assert(goalmodelmap_step3.degree < goalmodelmap_step2.degree)
    assert(goalmodelmap_step4.degree < goalmodelmap_step3.degree)

    val goalmodelmap_2_step1 = mapbuilder.update_goalmap(w0, goalmodelmap_template)
    val user_engage_2 = formula_builder.proposition("user_engagement",List(AtomTerm("open_mind"))).asInstanceOf[Proposition]
    val performed_social_2 = formula_builder.proposition("performed",List(AtomTerm("cognitive_exercise"))).asInstanceOf[Proposition]
    val w1_2: RawState = ontobuilder.state(StateOfWorld(List(user_engage_2,performed_social_2)))
    val goalmodelmap_2_step2 = mapbuilder.update_goalmap(w1_2, goalmodelmap_2_step1)

    val registered_exercise = formula_builder.proposition("activity_registered",List(AtomTerm("done"))).asInstanceOf[Proposition]
    val w2_2: RawState = ontobuilder.state(StateOfWorld(List(user_engage_2,performed_social_2,registered_exercise)))
    val goalmodelmap_2_step3 = mapbuilder.update_goalmap(w2_2, goalmodelmap_2_step2)

    assert(goalmodelmap_2_step2.degree < goalmodelmap_2_step1.degree)
    assert(goalmodelmap_2_step3.degree < goalmodelmap_2_step2.degree)

    val goalmodelmap_3_step1 = mapbuilder.update_goalmap(w0, goalmodelmap_template)
    val user_engage_3 = formula_builder.proposition("user_engagement",List(AtomTerm("passive"))).asInstanceOf[Proposition]
    val performed_entertainment_2 = formula_builder.proposition("performed",List(AtomTerm("entertainment"))).asInstanceOf[Proposition]
    val registered_entert = formula_builder.proposition("activity_registered",List(AtomTerm("done"))).asInstanceOf[Proposition]

    val w2_3: RawState = ontobuilder.state(StateOfWorld(List(user_engage_3,performed_entertainment_2,registered_entert)))
    val goalmodelmap_3_step2 = mapbuilder.update_goalmap(w2_3, goalmodelmap_3_step1)

    val merger = new GoalMapMerger(goal_model.root, new EffortToSatisf(goal_model))
    val merged_map1 = merger.merge_maps(goalmodelmap_step4.map,goalmodelmap_2_step3.map)
    val merged_map2 = merger.merge_maps(merged_map1,goalmodelmap_3_step2.map)

    val final_goal_map = GoalModelMap(goal_model.root.id,merged_map2)

    assert(final_goal_map.degree < goalmodelmap_step4.degree)
    assert(final_goal_map.degree < goalmodelmap_2_step3.degree)
    assert(final_goal_map.degree < goalmodelmap_3_step2.degree)

  }
}
