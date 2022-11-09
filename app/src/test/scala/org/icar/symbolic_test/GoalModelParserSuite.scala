package org.icar.symbolic_test

import org.icar.symbolic._
import org.icar.symbolic.parser.{FOLFormulaParser, GoalTreeParser}
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
      "goal g3 when func(p1) should address result(p2) and result(p4)    " +
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
    val resultp2 = pp.parseAll(pp.fol_formula,"result(p2) and result(p4)").get
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
          OrGoalDecomposition("g1",List(LTLGoalSpec("g3",funcp1,Finally(resultp2)), LTLGoalSpec("g4",funcp3,Finally(resultp4)))),
          AndGoalDecomposition("g2",List(LTLGoalSpec("g5",funcp5,Finally(resultp6)), LTLGoalSpec("g6",funcp7,Finally(resultp8))))
        )),
        List(
          ExplicitStartTime("g0",LocalDateTime.parse("2007-12-03T10:15:30")),
          ExplicitEndEvent("g0",baduser,"check_health"),
          AllowParallel("g3"),
          GoalDuration("g5",Duration.parse("PT1M30S"))
        )
      ))
  }


  test("parse AAL4E goal model") {
    val goal_model_string = "model { " +
      "goal g0 is and [" +
      "  goal g1 when true should address user_engagement(not_interested) or user_engagement(social) or user_engagement(open_mind) or user_engagement(passive) " +
      "  goal g2 is and [" +
      "    goal g2_1 when user_engagement(social) should address performed(social_activity) " +
      "    goal g2_2 when user_engagement(open_mind) should address performed(cognitive_exercise) " +
      "    goal g2_3 when user_engagement(passive) should address performed(entertainment) " +
      "  ]  " +
      "  goal g3 is and [" +
      "    goal g3_1 when user_localization(otherwise) should address activity_registered(done) " +
      "    goal g3_2 when user_engagement(not_interested) should address activity_registered(done) " +
      "    goal g3_3 when performed(social_activity) should address activity_registered(done) " +
      "    goal g3_4 when performed(cognitive_exercise) should address activity_registered(done) " +
      "    goal g3_5 when performed(entertainment) should address activity_registered(done) " +
      "  ]  " +
      "]  " +
      "}"

    val p = new GoalTreeParser

    val result = p.parseAll(p.goal_tree,goal_model_string)
    assert(result.successful)

  }

  test("parse AAL4E LTL goal model") {
    val goal_model_string = "model { " +
      "goal g0 is and [" +
      "  goal g1 when true should ensure F (user_engagement(not_interested) or user_engagement(social) or user_engagement(open_mind) or user_engagement(passive)) " +
      "  goal g2 is and [" +
      "    goal g2_1 when user_engagement(social) should ensure F performed(social_activity) " +
      "    goal g2_2 when user_engagement(open_mind) should ensure F performed(cognitive_exercise) " +
      "    goal g2_3 when user_engagement(passive) should ensure F performed(entertainment) " +
      "  ]  " +
      "  goal g3 is and [" +
      "    goal g3_1 when user_localization(otherwise) should ensure F activity_registered(done) " +
      "    goal g3_2 when user_engagement(not_interested) should ensure F activity_registered(done) " +
      "    goal g3_3 when performed(social_activity) should ensure F activity_registered(done) " +
      "    goal g3_4 when performed(cognitive_exercise) should ensure F activity_registered(done) " +
      "    goal g3_5 when performed(entertainment) should ensure F activity_registered(done) " +
      "  ]  " +
      "]  " +
      "}"

    val p = new GoalTreeParser

    val result = p.parseAll(p.goal_tree,goal_model_string)
    assert(result.successful)

  }

  test("parse AAL4E MTL goal model") {
    val goal_model_string = "model { " +
      "goal g0 is and [" +
      "  goal g1 when true should grant F[0,1] (user_engagement(not_interested) or user_engagement(social) or user_engagement(open_mind) or user_engagement(passive)) " +
      "  goal g2 is and [" +
      "    goal g2_1 when user_engagement(social) should grant F[0,1] performed(social_activity) " +
      "    goal g2_2 when user_engagement(open_mind) should grant F[0,1] performed(cognitive_exercise) " +
      "    goal g2_3 when user_engagement(passive) should grant F[0,1] performed(entertainment) " +
      "  ]  " +
      "  goal g3 is and [" +
      "    goal g3_1 when user_localization(otherwise) should grant F[0,1] activity_registered(done) " +
      "    goal g3_2 when user_engagement(not_interested) should grant F[0,1] activity_registered(done) " +
      "    goal g3_3 when performed(social_activity) should grant F[0,1] activity_registered(done) " +
      "    goal g3_4 when performed(cognitive_exercise) should grant F[0,1] activity_registered(done) " +
      "    goal g3_5 when performed(entertainment) should grant F[0,1] activity_registered(done) " +
      "  ]  " +
      "]  " +
      "}"

    val p = new GoalTreeParser

    val result = p.parseAll(p.goal_tree,goal_model_string)
    assert(result.successful)

  }
}


