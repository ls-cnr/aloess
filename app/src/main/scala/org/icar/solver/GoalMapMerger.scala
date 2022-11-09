package org.icar.solver

import org.icar.subsymbolic.builder.SubLogicBuilder
import org.icar.symbolic.parser.{DomainOntologyParser, GoalTreeParser}
import org.icar.symbolic._

class GoalMapMerger(val root: GoalNode) {
  private var result : Map[String,GoalState] = Map.empty

  def merge_maps(left: Map[String,GoalState], right: Map[String,GoalState]) : Map[String,GoalState] = {
    result = left
    merge_generic_node(root,right)
    result
  }

  private def merge_generic_node(goal_node: GoalNode,right: Map[String,GoalState]) : Unit = {
    goal_node match {
      case AndGoalDecomposition(name, subgoals) => merge_and_or_node(name, subgoals,right)
      case OrGoalDecomposition(name, subgoals) => merge_and_or_node(name, subgoals,right)
      case LTLGoalSpec(name, _, _) => merge_leaf_node(name,right)
      case FOLGoalSpec(name, _, _) => merge_leaf_node(name,right)
      case GoalSpec(name, _, _) => merge_leaf_node(name,right)
      case _ =>
    }
  }

    private def merge_and_or_node(name: String, subgoals: List[GoalNode],right: Map[String,GoalState]): Unit = {
      var fullsat=true
      for (g<-subgoals) {
        merge_generic_node(g,right)
        val state_sg = result(g.id)
        state_sg.satisf match {
          case FullSatisfaction() =>
          case _ => fullsat = false
        }
      }
      if (fullsat)
        result = result - name + (name -> GoalState(Completed(), FullSatisfaction(),false,false))
      else
        result = result - name + (name -> GoalState(DependsOnSubgoals(), PartialSatisfaction(0),false,false))
    }

  def merge_leaf_node(name: String,right: Map[String,GoalState]): Unit = {
    val right_state = right(name)
    if (right_state.satisf == FullSatisfaction())
      result = result - name + (name -> right_state)
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

  val left_map = left_1.map - "g3" + ("g3" -> GoalState(Completed(), FullSatisfaction(),false,false))

  val right_map = right_1.map - "g4" + ("g4" -> GoalState(Completed(), FullSatisfaction(),false,false))

  val merger = new GoalMapMerger(goal_model.root)

  val merged = merger.merge_maps(left_map,right_map)
  println(merged)

  val right_2 = mapbuilder.create_goalmodel_map
  val right_map2 = right_2.map - "g5" + ("g5" -> GoalState(Completed(), FullSatisfaction(),false,false)) - "g6" + ("g6" -> GoalState(Completed(), FullSatisfaction(),false,false))

  val merged2 = merger.merge_maps(merged,right_map2)
  println(merged2)


}