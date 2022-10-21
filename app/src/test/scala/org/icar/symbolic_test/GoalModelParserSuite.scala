package org.icar.symbolic_test

import org.icar.symbolic._
import org.icar.symbolic.builder.DomainOntologyBuilder
import org.icar.symbolic.parser.{DomainOntologyParser, FOLFormulaParser, GoalTreeParser}
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

import java.time.{Duration, LocalDateTime}

@RunWith(classOf[JUnitRunner])
class GoalModelParserSuite extends AnyFunSuite {
  test("parsing full goal tree") {
    val p = new GoalTreeParser

    val result = p.parseAll(p.goal_tree,"model { " +
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
      "meta [" +
      "start g0 at \"2007-12-03T10:15:30\"    " +
      "end g0 when bad(user) with capability check_health    " +
      "allow g3 parallel    " +
      "duration g5 max \"PT1M30S\" " +
      "] " +
      "}")
    assert(result.successful)
    val pp= new FOLFormulaParser
    val funcp1 = pp.parseAll(pp.fol_predicate,"func(p1)").get
    val resultp2 = pp.parseAll(pp.fol_predicate,"result(p2)").get
    val funcp3 = pp.parseAll(pp.fol_predicate,"func(p3)").get
    val resultp4 = pp.parseAll(pp.fol_predicate,"result(p4)").get
    val funcp5 = pp.parseAll(pp.fol_predicate,"func(p5)").get
    val resultp6 = pp.parseAll(pp.fol_predicate,"result(p6)").get
    val funcp7 = pp.parseAll(pp.fol_predicate,"func(p7)").get
    val resultp8 = pp.parseAll(pp.fol_predicate,"result(p8)").get
    val baduser = pp.parseAll(pp.fol_predicate,"bad(user)").get
    assert(result.get ==
      GoalTree(
        AndGoalDecomposition("g0",List(
          OrGoalDecomposition("g1",List(FOLGoalSpec("g3",funcp1,resultp2), FOLGoalSpec("g4",funcp3,resultp4))),
          AndGoalDecomposition("g2",List(FOLGoalSpec("g5",funcp5,resultp6), FOLGoalSpec("g6",funcp7,resultp8)))
        )),
        List(
          ExplicitStartTime("g0",LocalDateTime.parse("2007-12-03T10:15:30")),
          ExplicitEndEvent("g0",baduser,"check_health"),
          AllowParallel("g3"),
          GoalDuration("g5",Duration.parse("PT1M30S"))
        )
      ))
  }

}


