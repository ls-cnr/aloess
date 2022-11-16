package org.icar.solver

import org.icar.subsymbolic.builder.SubLogicBuilder
import org.icar.subsymbolic.{RawFuture, RawState, RawTT}
import org.icar.symbolic._

class GoalMapBuilder(builder : SubLogicBuilder, goal_model : GoalTree, d2s : DistanceToSatisfMetric) {

  def create_goalmodel_map : GoalModelMap = {
    var map : Map[String,GoalState] = Map.empty
    map = map ++ goal_entry(goal_model.root)
    GoalModelMap(goal_model.root.id,map)
  }

  private def goal_entry(node: GoalNode):Map[String,GoalState] = {
    node match {
      case x:LTLGoalSpec =>
        val form = builder.formula(x.trigger)
        Map(x.name -> GoalState(Ready(form),PartialSatisfaction(),d2s.failure_value,false,false))
      case x:FOLGoalSpec =>
        val form = builder.formula(x.trigger)
        Map(x.name -> GoalState(Ready(form),PartialSatisfaction(),d2s.failure_value,false,false))
      case x:GoalSpec =>
        val form = builder.formula(x.trigger)
        Map(x.name -> GoalState(Ready(form),PartialSatisfaction(),d2s.failure_value,false,false))
      case x:OrGoalDecomposition =>
        var map : Map[String,GoalState] = Map.empty
        for (g <- x.subgoals) map = map ++ goal_entry(g)
        map += (x.name -> GoalState(DependsOnSubgoals(),PartialSatisfaction(),d2s.failure_value,false,false))
        map
      case x:AndGoalDecomposition =>
        var map : Map[String,GoalState] = Map.empty
        for (g <- x.subgoals) map = map ++ goal_entry(g)
        map += (x.name -> GoalState(DependsOnSubgoals(),PartialSatisfaction(),d2s.failure_value,false,false))
        map
    }
  }

  def update_goalmap(current: RawState, goalmap : GoalModelMap) : GoalModelMap = {
    val map = update_map_generic_node(goal_model.root,current,goalmap.map)
    GoalModelMap(goalmap.root,map)
  }

  private def update_map_generic_node(current_goal_node: GoalNode, current: RawState, map : Map[String,GoalState]): Map[String,GoalState] = {
    current_goal_node match {
      case AndGoalDecomposition(name, subgoals) => update_map_and_node(name, subgoals,current,map)
      case OrGoalDecomposition(name, subgoals) => update_map_or_node(name, subgoals,current,map)
      case LTLGoalSpec(name,trigger,formula) => update_map_leaf_node(name,trigger,formula,current,map)
      case FOLGoalSpec(name,trigger,formula) =>update_map_leaf_node(name,trigger,formula,current,map)
      case GoalSpec(name,trigger,formula) =>update_map_leaf_node(name,trigger,formula,current,map)
      case _ => map
    }
  }

  private def update_map_and_node(name : String, subgoals : List[GoalNode], current: RawState, map: Map[String, GoalState]) : Map[String,GoalState] = {
    val goalstate = map(name)
    goalstate.sat_state match {
      case FullSatisfaction() => map
      case Violation() => map
      case PartialSatisfaction() =>
        var sub_map = map

        var fullsat=true
        var violat= false
        for (sg <- subgoals if !violat) {
          sub_map = update_map_generic_node(sg, current, sub_map)
          val state_sg = sub_map(sg.id)
          state_sg.sat_state match {
            case PartialSatisfaction() =>fullsat=false
            case Violation() =>violat=true
            case _ =>
          }
        }
        if (violat)
          sub_map - name + (name -> GoalState(Completed(), Violation(),d2s.failure_value,false,false))
        else if (fullsat)
          sub_map - name + (name -> GoalState(Completed(), FullSatisfaction(),d2s.success_value,false,true))
        else {
          val new_degree = d2s.resistance_and_node(name,subgoals,map,current)
          sub_map - name + (name -> GoalState(DependsOnSubgoals(),PartialSatisfaction(),new_degree,false,false))
        }
    }
  }

  private def update_map_or_node(name : String, subgoals : List[GoalNode], current: RawState, map: Map[String, GoalState]) : Map[String,GoalState] = {
    val goalstate = map(name)
    goalstate.sat_state match {
      case FullSatisfaction() => map
      case Violation() => map
      case PartialSatisfaction() =>
        var sub_map = map

        var fullsat=false
        var violat= false
        for (sg <- subgoals if !violat) {
          sub_map = update_map_generic_node(sg, current, sub_map)
          val state_sg = sub_map(sg.id)
          state_sg.sat_state match {
            case FullSatisfaction() =>fullsat=true
            case Violation() =>violat=true
            case _ =>
          }
        }
        if (violat)
          sub_map - name + (name -> GoalState(Completed(), Violation(),d2s.failure_value,false,false))
        else if (fullsat)
          sub_map - name + (name -> GoalState(Completed(), FullSatisfaction(),d2s.success_value,false,true))
        else {
          val new_degree = d2s.resistance_or_node(name,subgoals,map,current)
          sub_map - name + (name -> GoalState(DependsOnSubgoals(),PartialSatisfaction(),new_degree,false,false))
        }
    }
  }

  private def update_map_leaf_node(name: String, trigger: LogicFormula, fs: LogicFormula, current: RawState, map: Map[String, GoalState]) : Map[String,GoalState] = {
    val goalstate = map(name)
    goalstate.sat_state match {
      case FullSatisfaction() =>map
      case Violation() =>map
      case PartialSatisfaction() =>
        goalstate.internal_state match {
          case Ready(cond) =>
            if (cond.satisfied_in(current)) {
              val form = builder.formula(fs).next(current)
              if (form.success_until_now && form.future_formula!=RawTT()) {
                val new_degree = d2s.resistance_leaf_node(name,map,current)
                map - name + (name -> GoalState(Committed(form.future_formula), PartialSatisfaction(),new_degree, true, false))
              } else {
                map - name + (name -> GoalState(Completed(),FullSatisfaction(),d2s.success_value,true,true ))
              }
            } else
              map
          case Committed(cond) =>
            val next: RawFuture = cond.next(current)
            if (!next.success_until_now)
              map - name + (name -> GoalState(Completed(),Violation(),d2s.failure_value,false,false))
            else if (next.future_formula==RawTT())
              map - name + (name -> GoalState(Completed(),FullSatisfaction(),d2s.success_value,false,true))
            else {
              val new_degree = d2s.resistance_leaf_node(name,map,current)
              map - name + (name -> GoalState(Committed(next.future_formula),PartialSatisfaction(),new_degree,false,false))
            }
        }
    }

  }

}

//
//object RunGoalMapBuilder extends App {
//  val p = new DomainOntologyParser
//  val pp = new GoalTreeParser
//
//  val onto_parser = p.parseAll(p.domain,"domain \"prova5\" {  " +
//    "category params atom [ p1,p2,p3,p4,p5,p6,p7,p8 ] " +
//    "define func(enum[params])" +
//    "define result(enum[params])" +
//    "}")
//
//  val onto = onto_parser.get
//  val goal_model_string = "model { " +
//    "goal g0 is and [" +
//    "goal g1 is or [" +
//    "goal g3 when func(p1) should address result(p2)   " +
//    "goal g4 when func(p3) should address result(p4) " +
//    "]  " +
//    "goal g2 is and [" +
//    "goal g5 when func(p5) should address result(p6)  " +
//    "goal g6 when func(p7) should address result(p8)  " +
//    "] " +
//    "]  " +
//    "}"
//  println(goal_model_string)
//  val result = pp.parseAll(pp.goal_tree,goal_model_string)
//
//  val goal_model = result.get
//
//  val ontobuilder = new SubLogicBuilder(onto)
//  val mapbuilder = new GoalMapBuilder(ontobuilder,goal_model)
//  val map = mapbuilder.create_goalmodel_map
//  println(map)
//}