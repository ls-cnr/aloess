package org.icar.solver

import org.icar.partial_sat.EffortToSatisf
import org.icar.subsymbolic.builder.SubLogicBuilder
import org.icar.symbolic.parser.{DomainOntologyParser, GoalTreeParser}
import org.icar.symbolic._

class GoalMapMerger(val root: GoalNode, d2s : DistanceToSatisfMetric) {
  private var result : Map[String,GoalState] = Map.empty

  def merge_maps(left: Map[String,GoalState], right: Map[String,GoalState]) : Map[String,GoalState] = {
    result = left
    merge_generic_node(root,right)
    result
  }

  private def merge_generic_node(goal_node: GoalNode,right: Map[String,GoalState]) : Unit = {
    goal_node match {
      case AndGoalDecomposition(name, subgoals) => merge_and_node(name, subgoals,right)
      case OrGoalDecomposition(name, subgoals) => merge_or_node(name, subgoals,right)
      case LTLGoalSpec(name, _, _) => merge_leaf_node(name,right)
      case FOLGoalSpec(name, _, _) => merge_leaf_node(name,right)
      case GoalSpec(name, _, _) => merge_leaf_node(name,right)
      case _ =>
    }
  }

  private def merge_and_node(name: String, subgoals: List[GoalNode],right: Map[String,GoalState]): Unit = {
    var fullsat=true
    var average_degree : Double = 0
    for (g<-subgoals) {
      merge_generic_node(g,right)
      val state_sg = result(g.id)
      average_degree += state_sg.sat_degree
      state_sg.sat_state match {
        case FullSatisfaction() =>
        case _ => fullsat = false
      }
    }
    average_degree = average_degree/subgoals.size
    if (fullsat)
      result = result - name + (name -> GoalState(Completed(), FullSatisfaction(),d2s.success_value,false,false))
    else {
      result = result - name + (name -> GoalState(DependsOnSubgoals(), PartialSatisfaction(), average_degree, false, false))
    }
  }


  private def merge_or_node(name: String, subgoals: List[GoalNode],right: Map[String,GoalState]): Unit = {
      var fullsat=true
      var max_degree : Double = 0
      for (g<-subgoals) {
        merge_generic_node(g,right)
        val state_sg = result(g.id)
        max_degree = Math.max(max_degree,state_sg.sat_degree)
        state_sg.sat_state match {
          case FullSatisfaction() =>
          case _ => fullsat = false
        }
      }
      if (fullsat)
        result = result - name + (name -> GoalState(Completed(), FullSatisfaction(),d2s.success_value,false,false))
      else {
        result = result - name + (name -> GoalState(DependsOnSubgoals(), PartialSatisfaction(),max_degree, false, false))
      }
    }

  def merge_leaf_node(name: String,right: Map[String,GoalState]): Unit = {
    val right_state = right(name)
    val actual_state = result(name)

    actual_state.sat_state match {
      case FullSatisfaction() =>
      case Violation() =>

      case PartialSatisfaction() =>
        right_state.sat_state match {
          case FullSatisfaction() =>
            result = result - name + (name -> right_state)
          case PartialSatisfaction() =>
            if (right_state.sat_degree > actual_state.sat_degree)
              result = result - name + (name -> GoalState(right_state.internal_state, PartialSatisfaction(),right_state.sat_degree,false,false))
          case Violation() =>
          case _ =>
        }

      case _ =>
    }
  }

}

object RunGoalMerger extends App {
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
  println(merged)

  val right_2 = mapbuilder.create_goalmodel_map
  val right_map2 = right_2.map - "g5" + ("g5" -> GoalState(Completed(), FullSatisfaction(),100,false,false)) - "g6" + ("g6" -> GoalState(Completed(), FullSatisfaction(),100,false,false))

  val merged2 = merger.merge_maps(merged,right_map2)
  println(merged2)


}