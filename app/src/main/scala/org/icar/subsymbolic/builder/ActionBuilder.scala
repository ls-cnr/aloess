package org.icar.subsymbolic.builder

import org.icar.subsymbolic._
import org.icar.symbolic._
import org.icar.symbolic.parser.{AbstractCapabilityParser, DomainOntologyParser}

class ActionBuilder(val builder : SubLogicBuilder, repository : List[AbstractCapability]) {
  var actions : List[RawAction] = List.empty
  var action_register : Map[Int,CapabilityEntry] = Map.empty

  init

  private def init: Unit = {
    for (cap <- repository) {
      val params: List[CapabilityParameter] = cap.params
      unroll_actions(cap,params,List.empty)
    }
  }

  private def unroll_actions(cap : AbstractCapability, param_to_assign:List[CapabilityParameter], assigned_params:List[CapabilityParameterEntry]):Unit = {
    if (param_to_assign.isEmpty) {
      register_action(cap,assigned_params)
    } else {
      val param: CapabilityParameter = param_to_assign.head
      val cat_name = param.category
      val cat: ObjectCategory = builder.onto.get_category_by_name(cat_name)
      for (value <- cat.range) {
        val assign_entry = CapabilityParameterEntry(param.variable,value)
        unroll_actions(cap,param_to_assign.tail,assign_entry::assigned_params)
      }
    }
  }
  def register_action(cap: AbstractCapability, assigned: List[CapabilityParameterEntry]): Unit = {
    val entry = CapabilityEntry(cap,assigned)

    var map : Map[VariableTerm,ConstantTerm] = Map.empty
    for (p <- assigned) map += (p.variable -> p.value)

    val raw_pre = builder.formula(cap.pre.apply_substitution(map))
    val raw_effects = convert_effects(cap.effects,map)
    val raw_future = builder.formula(cap.future.apply_substitution(map))
    val action = RawAction(actions.size,raw_pre,raw_effects,raw_future)

    actions = action :: actions
    action_register += (action.id -> entry)
  }

  private def convert_effects(effects : List[EvolutionGrounding], map : Map[VariableTerm,ConstantTerm]) : List[RawEvolution] = {
    for (e <- effects) yield RawEvolution(e.name,convert_evo(e.evo,map),e.probability)
  }

  private def convert_evo(evo: List[EvoOperator], map: Map[VariableTerm,ConstantTerm]): List[RawEvoOperator] = {
    for (e <- evo) yield e match {
      case AddOperator(p) =>
        val pp = p.apply_substitution(map)
        RawAdd( builder.formula_predicate(pp))
      case RmvOperator(p) =>
        val pp = p.apply_substitution(map)
        RawRem( builder.formula_predicate(pp))
    }
  }

}


object RunActionBuilder extends App {
  val domain_parser = new DomainOntologyParser

  val onto_parser = domain_parser.parseAll(domain_parser.domain,"domain \"prova5\" {  " +
    "category users atom [ luca,john,claudia ] " +
    "category rooms string [ \"livingroom\",\"kitchen\",\"bedroom\" ]  " +
    "category sensor_id number [ 1,2,3 ]  " +

    "define input(enum[users])" +
    "define output(enum[users])" +
    "define room(enum[rooms])" +

    "}")

  val onto = onto_parser.get

  val capability_parser = new AbstractCapabilityParser

  val result = capability_parser.parseAll(capability_parser.capability,"capability \"cap1\" [ " +
    "params : ?x is users, ?y is rooms " +
    "pre: input(?x)   " +
    "post: output(claudia)   " +
    "evolution \"evo1\" : add output(claudia), rmv room(?y)   " +
    "evolution \"evo2\" : add output(john), rmv room(?y)   " +
    "future: G output(?x)   " +
    "]")

  val cap = result.get
  println(cap)

  val sub_logic = new SubLogicBuilder(onto)
  val sub_actions = new ActionBuilder(sub_logic,List(cap))

  println(sub_actions.action_register)
  println(sub_actions.actions)



}