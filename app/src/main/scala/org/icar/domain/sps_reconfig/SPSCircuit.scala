package org.icar.domain.sps_reconfig

import org.icar.subsymbolic.RawState
import org.icar.subsymbolic.builder.SubLogicBuilder
import org.icar.symbolic._
import org.icar.symbolic.builder.DomainOntologyBuilder
import org.icar.symbolic.parser.AbstractCapabilityParser

import scala.io.Source
import scala.util.Random

class SPSCircuit (
    var nodes: List[SimpleNode],
    var loads: List[Load],
    var generators: List[Generator],
    var possible_failures: List[ConnectionWithFailure],
    var switchers: List[Switcher],
    var twowayselectors: List[TwoWaySelector],
    var mutex_switchers : List[(Switcher,Switcher)]
  ) {
  private val builder = new DomainOntologyBuilder("sps_circuit")
  private val generator_cat = builder.atom_category("gen_id",for (g<-generators) yield g.name)
  private val load_cat = builder.atom_category("load_id",for (l<-loads) yield l.name)
  private val switcher_cat = builder.atom_category("sw_id",for (sw<-switchers) yield sw.name)
  private val selector_cat = builder.atom_category("sel_id",for (s<-twowayselectors) yield s.name)
  private val node_cat = builder.number_category("node_id",for (n<-nodes) yield n.id)
  private val failure_cat = builder.number_category("failure_id",for (f<-possible_failures) yield f.id)
  private val free_sw_cat = builder.atom_category("free_sw_id",for (sw<-switchers if !is_mutex(sw)) yield sw.name)

  generate_predicates
  generate_axioms

  def onto : DomainOntology = builder.build()
  val cap_repository : List[AbstractCapability] = generate_actions

  def getLoadByID(id:Int) : Option[Load] = {
    var ret : Option[Load] = None
    for (l<-loads if l.id==id) ret = Some(l)
    ret
  }
  def getLoadByName(name:String) : Option[Load] = {
    var ret : Option[Load] = None
    for (l<-loads if l.name==name) ret = Some(l)
    ret
  }
  def is_mutex(switcher: Switcher) : Boolean = {
    var f=false
    for (m<-mutex_switchers) if (m._1==switcher || m._2==switcher) f=true
    f
  }

  private def generate_predicates : Unit = {
    builder.signature("on_gen").with_enum_arg(generator_cat).create()
    builder.signature("up_load").with_enum_arg(load_cat).create()
    builder.signature("up_node").with_enum_arg(node_cat).create()
    builder.signature("failure").with_enum_arg(failure_cat).create()
    builder.signature("pos1_sel").with_enum_arg(selector_cat).create()
    builder.signature("pos2_sel").with_enum_arg(selector_cat).create()
    builder.signature("closed_sw").with_enum_arg(switcher_cat).create()
  }

  private def generate_axioms : Unit = {

    for (sw<-switchers) {
      // source ^ closed => dest
      if (!sw.dest.isInstanceOf[Generator]){
        val ante1 = PredicateCondition( sw.source.up_condition.to_predicate )
        val ante2 = PredicateCondition( sw.closed_predicate.to_predicate )
        val cons = sw.dest.up_condition.to_predicate

        builder.axiom(cons , RuleAntecedent(List(ante1,ante2)))
      }

      // dest ^ closed => source
      if (!sw.source.isInstanceOf[Generator]){
        val c_ante1 = PredicateCondition( sw.dest.up_condition.to_predicate )
        val c_ante2 = PredicateCondition( sw.closed_predicate.to_predicate )
        val c_cons = sw.source.up_condition.to_predicate

        builder.axiom(c_cons , RuleAntecedent(List(c_ante1,c_ante2)))
      }
    }

    for (sel<-twowayselectors) {
      val pos1_is_up = sel.pos1.up_condition.to_predicate
      val pos2_is_up = sel.pos2.up_condition.to_predicate
      val central_is_up = sel.middle.up_condition.to_predicate
      val sel_is_pos1 = sel.pos1_predicate.to_predicate
      val sel_is_pos2 = sel.pos2_predicate.to_predicate

      // pos1_node ^ pos1 => central_node
      if (!sel.middle.isInstanceOf[Generator]) {
        builder.axiom(central_is_up , RuleAntecedent(List(PredicateCondition(pos1_is_up),PredicateCondition(sel_is_pos1))))
      }

      // central_node ^ pos1 => pos1_node
      if (!sel.pos1.isInstanceOf[Generator]) {
        builder.axiom(pos1_is_up , RuleAntecedent(List(PredicateCondition(central_is_up),PredicateCondition(sel_is_pos1))))
      }

      // pos2_node ^ pos2 => central_node
      if (!sel.middle.isInstanceOf[Generator]) {
        builder.axiom(central_is_up , RuleAntecedent(List(PredicateCondition(pos2_is_up),PredicateCondition(sel_is_pos2))))
      }

      // central_node ^ pos2 => pos2_node
      if (!sel.pos2.isInstanceOf[Generator]) {
        builder.axiom(pos2_is_up , RuleAntecedent(List(PredicateCondition(central_is_up),PredicateCondition(sel_is_pos2))))
      }
    }

    for (fail<-possible_failures) {
      // source ^ !failure => dest
      if (!fail.dest.isInstanceOf[Generator]){
        val ante1 = PredicateCondition( fail.source.up_condition.to_predicate )
        val ante2 = NegateCondition( fail.fired_predicate.to_predicate )
        val cons = fail.dest.up_condition.to_predicate
        builder.axiom(cons , RuleAntecedent(List(ante1,ante2)))

      }

      // dest ^ closed => source
      if (!fail.source.isInstanceOf[Generator]){
        val c_ante1 = PredicateCondition( fail.dest.up_condition.to_predicate )
        val c_ante2 = NegateCondition( fail.fired_predicate.to_predicate )
        val c_cons = fail.source.up_condition.to_predicate
        builder.axiom(c_cons , RuleAntecedent(List(c_ante1,c_ante2)))
      }
    }
  }
  private def generate_actions : List[AbstractCapability] = {

    val gen_on_string =Source.fromFile("app/src/main/resources/domain/sps_reconfig/switch_on_gen.cap").getLines().mkString
    val capability_parser1 = new AbstractCapabilityParser
    val capability_parser_result = capability_parser1.parseAll(capability_parser1.capability,gen_on_string)
    val gen_on_cap = capability_parser_result.get

    val gen_off_string =Source.fromFile("app/src/main/resources/domain/sps_reconfig/switch_off_gen.cap").getLines().mkString
    val capability_parser2 = new AbstractCapabilityParser
    val capability_parser_result2 = capability_parser2.parseAll(capability_parser2.capability,gen_off_string)
    val gen_off_cap = capability_parser_result2.get

    val close_sw_string =Source.fromFile("app/src/main/resources/domain/sps_reconfig/close_switcher.cap").getLines().mkString
    val capability_parser3 = new AbstractCapabilityParser
    val capability_parser_result3 = capability_parser3.parseAll(capability_parser3.capability,close_sw_string)
    val close_sw_cap = capability_parser_result3.get

    val open_sw_string =Source.fromFile("app/src/main/resources/domain/sps_reconfig/open_switcher.cap").getLines().mkString
    val capability_parser4 = new AbstractCapabilityParser
    val capability_parser_result4 = capability_parser4.parseAll(capability_parser4.capability,open_sw_string)
    val open_sw_cap = capability_parser_result4.get

    val selector_pos1_string =Source.fromFile("app/src/main/resources/domain/sps_reconfig/selector_pos1.cap").getLines().mkString
    val capability_parser5 = new AbstractCapabilityParser
    val capability_parser_result5 = capability_parser5.parseAll(capability_parser5.capability,selector_pos1_string)
    val selector_pos1_cap = capability_parser_result5.get

    val selector_pos2_string =Source.fromFile("app/src/main/resources/domain/sps_reconfig/selector_pos2.cap").getLines().mkString
    val capability_parser6 = new AbstractCapabilityParser
    val capability_parser_result6 = capability_parser6.parseAll(capability_parser6.capability,selector_pos2_string)
    val selector_pos2_cap = capability_parser_result6.get

    var list_of_actions : List[AbstractCapability] = List(gen_on_cap,gen_off_cap,close_sw_cap,open_sw_cap,selector_pos1_cap,selector_pos2_cap)

    var mutex_counter = 1
    for (m<-mutex_switchers) {
      val sw1=m._1.closed_predicate.to_predicate
      val sw2=m._2.closed_predicate.to_predicate

      val mutex_action_pos1 = AbstractCapability(
        id = "mutex_"+mutex_counter+"_pos1",
        params = List.empty,

        pre = Conjunction(List(Negation(sw1),sw2)),

        post = Conjunction(List(sw1,Negation(sw2))),

        effects = List(
          EvolutionGrounding(s"mutex_${mutex_counter}_pos1",List(
            AddOperator(sw1),
            RmvOperator(sw2)
          ))),

        future = Conjunction(List(sw1,Negation(sw2)))
      )

      val mutex_action_pos2 = AbstractCapability(
        id = "mutex_"+mutex_counter+"_pos2",
        params = List.empty,

        pre = Conjunction(List(sw1,Negation(sw2))),

        post = Conjunction(List(sw2,Negation(sw1))),

        effects = List(
          EvolutionGrounding(s"mutex_${mutex_counter}_pos2",List(
            AddOperator(sw2),
            RmvOperator(sw1)
          ))),

        future = Conjunction(List(sw2,Negation(sw1)))
      )

      list_of_actions = mutex_action_pos1 :: mutex_action_pos2 :: list_of_actions
      mutex_counter += 1
    }


    list_of_actions
  }

  def goal_model(mission: SPSMission) : GoalTree = {
    var formulas : List[LogicFormula] = List.empty
    for (vital<-mission.vitals){
      val l = getLoadByName(vital)
      if (l.isDefined) {
        val up_pred = l.get.up_condition
        formulas = up_pred :: formulas
      }
    }
    val final_state = Finally(Conjunction(formulas))

    GoalTree(LTLGoalSpec("root",True(),final_state),List.empty)
  }

  def print_for_graphviz_with_field(f:ForceFieldLayer) : Unit = {
    println("digraph Circuit {")

    //nodes.foreach( n=> println(s"${n.node_string} [label=\"(${f.map(n)})\"]; ") )
    nodes.foreach( n=> println(s"${n.node_string}; ") )
    loads.foreach( n=> println(s"${n.node_string} [shape=invtriangle,color=black]; ") )
    generators.foreach( n=> println(s"${n.node_string} [shape=box,color=red];") )

    //possible_failures.foreach( pf => println(s"${pf.source.node_string} -> ${pf.dest.node_string} [label=\"failure=${pf.id}\"];"))
    //switcher.foreach( s=> println(s"${s.source.node_string} -> ${s.dest.node_string} [label=\"switch=${s.id}\"];"))

    println("}")
  }
  def print_for_graphviz() : Unit = {
    println("digraph Circuit {")

    nodes.foreach( n=> println(s"${n.node_string}; ") )
    loads.foreach( n=> println(s"${n.node_string} [shape=invtriangle,color=black]; ") )
    generators.foreach( n=> println(s"${n.node_string} [shape=box,color=red];") )

    possible_failures.foreach( f => {
      val src: String = f.source.node_string
      val dst: String = f.dest.node_string
      val id: Int = f.id
      println(src+" -> "+dst+"[label=\"failure="+id+"\",dir=none];")
    })
    switchers.foreach( s=> {
      val src: String = s.source.node_string
      val dst: String = s.dest.node_string
      val id: String = s.name
      println(src+" -> "+dst+"[label=\"switch="+id+"\",dir=none];")
    })

    println("}")
  }

  def pretty_string(map:SubLogicBuilder)(s:RawState):String = {
    var first = true
    var str = "["
    for (index<-0 until s.satisfies.length)
      if (s.satisfies(index))
        if (map.inverse(index).functional!="up_node"){
          if (first) first = false else str+=","
          str+=map.inverse(index).terms(0)
        }
    str+"]"
  }
}




