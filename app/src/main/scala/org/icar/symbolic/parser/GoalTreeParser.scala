package org.icar.symbolic.parser

import org.icar.symbolic._

import java.time.{Duration, LocalDateTime}
import scala.util.parsing.combinator.JavaTokenParsers

class GoalTreeParser extends JavaTokenParsers with FOLFormulaParserTrait with LTLFormulaParserTrait with MTLFormulaParserTrait {
  def goal_tree : Parser[GoalTree] = "model"~"{"~goal~opt(meta)~"}" ^^ {
    case _~_~g~Some(m)~_ => GoalTree(g,m)
    case _~_~g~None~_ => GoalTree(g,List.empty)
  }

  def goal : Parser[GoalNode] = and_decomposition | or_decomposition | leaf_goal

  def and_decomposition : Parser[AndGoalDecomposition] = "goal"~ident~"is"~"and"~"["~rep(goal)~"]" ^^ {
    case _~name~_~_~_~sub~_ => AndGoalDecomposition(name,sub)
  }
  def or_decomposition : Parser[OrGoalDecomposition] = "goal"~ident~"is"~"or"~"["~rep(goal)~"]" ^^ {
    case _~name~_~_~_~sub~_ => OrGoalDecomposition(name,sub)
  }

  def leaf_goal : Parser[GoalNode] = fol_goal | ltl_goal | mtl_goal

  def fol_goal : Parser[LTLGoalSpec] = "goal"~ident~"when"~fol_formula~"should"~"address"~fol_formula ^^ {
    case _~name~_~trigger~_~_~fs => LTLGoalSpec(name,trigger,Finally(fs))
  }
  def ltl_goal : Parser[LTLGoalSpec] = "goal"~ident~"when"~fol_formula~"should"~"ensure"~ltl_formula ^^ {
    case _~name~_~trigger~_~_~fs => LTLGoalSpec(name,trigger,fs)
  }
  def mtl_goal : Parser[MTLGoalSpec] = "goal"~ident~"when"~fol_formula~"should"~"grant"~mtl_formula ^^ {
    case _~name~_~trigger~_~_~fs => MTLGoalSpec(name,trigger,fs)
  }



  def meta : Parser[List[MetaGoal]] = "meta"~"["~rep(metagoal)~"]" ^^ {
    case _~_~metalist~_ => metalist
  }
  def metagoal : Parser[MetaGoal] = pattern | duration | failure_rate

  def pattern : Parser[GoalPattern] = explicit_start | explicit_end | repeat_until | multiinstance | parallel
  // Local Time:
  //text – the text to parse such as "2007-12-03T10:15:30"
  def explicit_start : Parser[GoalPattern] = "start"~ident~"at"~stringLiteralDrimmed ^^ {
    case _~name~_~string => ExplicitStartTime(name,LocalDateTime.parse(string))
  } | "start"~ident~"when"~fol_formula~opt("with"~"capability"~ident) ^^ {
    case _~name~_~form~Some(_~_~cap) => ExplicitStartEvent(name,form,cap)
    case _~name~_~form~None => ExplicitStartEvent(name,form)
  }
  def explicit_end : Parser[GoalPattern] = "end"~ident~"at"~stringLiteralDrimmed ^^ {
    case _~name~_~string => ExplicitEndTime(name,LocalDateTime.parse(string))
  } | "end"~ident~"when"~fol_formula~opt("with"~"capability"~ident) ^^ {
    case _~name~_~form~Some(_~_~cap) => ExplicitEndEvent(name,form,cap)
    case _~name~_~form~None => ExplicitEndEvent(name,form)
  }
  def repeat_until : Parser[GoalPattern] = "repeat"~ident~"until"~fol_formula~opt("with"~"capability"~ident) ^^ {
    case _~name~_~form~Some(_~_~cap) => RepeatUntil(name,form,cap)
    case _~name~_~form~None => RepeatUntil(name,form)
  }
  def multiinstance : Parser[GoalPattern] = "allow"~ident~"multi-instance" ^^ {
    case _~name~_ => AllowMultiInstance(name)
  }
  def parallel : Parser[GoalPattern] = "allow"~ident~"parallel" ^^ {
    case _~name~_ => AllowParallel(name)
  }

  //NOTE: duration is in ISO8601 format. Ex.
  // - P1Y - 1 year
  // - P2M4D - 2 months and 4 days
  // - P3Y6M4DT12H30M5S - 3 years, 7 months, 4 days, 12 hours, 30 minutes, and 5 seconds
  def duration : Parser[GoalDuration] = "duration"~ident~"max"~stringLiteralDrimmed ^^ {
    case _~name~_~string =>
      val duration = Duration.parse(string)
      GoalDuration(name,duration)
  }

  def failure_rate : Parser[FailureRate] = probabilistic
  def probabilistic : Parser[ProbabilisticFailureRate] = "failure"~"of"~ident~"below"~floatingPointNumber~"%" ^^ {
    case _~_~name~_~prob~_ => ProbabilisticFailureRate(name,prob.toDouble)
  }

}

object GoalTreeParser extends App {
  val p = new GoalTreeParser

  val s = "model { " +
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
  "}"

  val result = p.parseAll(p.goal_tree,s)

  result match {
    case p.Success(matched,_) => println(matched)
    case p.Failure(msg,_) => println(s"FAILURE: $msg")
    case p.Error(msg,_) => println(s"ERROR: $msg")
  }

}
