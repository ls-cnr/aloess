package org.icar.subsymbolic.rete

import org.icar.subsymbolic.builder.{RETEBuilder, SubLogicBuilder}
import org.icar.subsymbolic.{RawAdd, RawEvolution, RawProposition, RawRem, RawState}
import org.icar.symbolic.parser.DomainOntologyParser

class RETE {
  /* static part */
  var alphas : List[AlphaNode] = List.empty
  var betas : List[BetaNode] = List.empty
  var prods : List[ProductionNode] = List.empty
  var id_to_use = 0

  def reset_memory(wi: RawState):Memory = {
    var zero_memory = Memory(wi,Map.empty,Map.empty,Map.empty)
    for (a<-alphas)
      if (!a.inverse && wi.satisfies(a.activation))
        zero_memory = a.check_activation(zero_memory)
      else if (a.inverse && !wi.satisfies(a.activation))
        zero_memory = a.check_activation(zero_memory)
    zero_memory
  }

  def add_fact(memory:Memory, prop : RawProposition) : Memory = {
    if (!memory.stable_state.satisfies(prop.index)) {
      val result = memory.touch(prop, true)
      check_all_alpha(result)
    } else
      memory
  }
  def rmv_fact(memory:Memory, prop : RawProposition) : Memory = {
    if (memory.stable_state.satisfies(prop.index)) {
      val result = memory.touch(prop, false)
      check_all_alpha(result)
    } else
      memory
  }
  def evolution(memory: Memory, evo: RawEvolution) : Memory = {
    var result = memory
    for (op <- evo.evo) op match {
      case RawAdd(add) => result = add_fact(result,add)
      case RawRem(rmv) => result = rmv_fact(result,rmv)
      case _ =>
    }
    result
  }

  private def check_all_alpha(memory: Memory) : Memory = {
    var result = memory
    for (a <- alphas) result = a.check_activation(result)
    result
  }

  def get_or_create_alpha_node(activation: Int, inverse:Boolean) : AlphaNode = {
    val selected = alphas.filter(x => (x.activation == activation && x.inverse==inverse) )
    if (selected.nonEmpty)
      selected.head
    else {
      val alpha = new AlphaNode(this,id_to_use,activation,inverse,List.empty)
      id_to_use += 1
      alphas = alpha :: alphas
      alpha
    }
  }

  def get_or_create_beta_node(consequence : ReteNode) : BetaNode = {
    val beta = new BetaNode(this,id_to_use,consequence)
    id_to_use += 1
    betas = beta :: betas
    beta
  }

  def get_or_create_prod_node(production: Int) : ProductionNode = {
    val selected = prods.filter(x => x.production == production)
    if (selected.nonEmpty)
      selected.head
    else {
      val pnode = new ProductionNode(this,id_to_use,production)
      id_to_use += 1
      prods = pnode :: prods
      pnode
    }

  }


  def stringGraphviz : String = {
    var string = "digraph Rete {\n"

    for (n <- alphas) {
      string += "\"" + n.node_label + "\"; \n"
      for (c <- n.consequences) {
        string += "\"" + n.node_label + "\""
        string += "->"
        string += "\"" + c.node_label + "\" ;\n"
      }
    }
    for (n <- betas) {
      string += "\"" + n.node_label + "\"; \n"
      string += "\"" + n.node_label + "\""
      string += "->"
      string += "\"" + n.consequence.node_label + "\" ;\n"
    }
    for (n <- prods)
      string += "\"" + n.node_label + "\"; \n"

    string + "}\n"

  }

}



object RunRete extends App {
  val domain_parser = new DomainOntologyParser

  val onto_parser = domain_parser.parseAll(domain_parser.domain,"domain \"prova5\" {  " +
    "category users atom [ luca,john,claudia ] " +
    "category rooms string [ \"livingroom\",\"kitchen\",\"bedroom\" ]  " +
    "category sensor_id number [ 1,2,3 ]  " +

    "define input(enum[users])" +
    "define output(enum[users])" +
    "define room(enum[rooms])" +

    "rule input(?a), output(?b) => room(\"kitchen\")" +

    "}")

  val onto = onto_parser.get
  val logic_builder = new SubLogicBuilder(onto)
  val rete_builder = new RETEBuilder(logic_builder,onto.axioms)

  val myrete = rete_builder.rete
  println(myrete.stringGraphviz)
  val wi = RawState(Array(false,false,false,false,false,false,false,false,false))
  val start_memory = Memory(wi,Map.empty,Map.empty,Map.empty)
  println(start_memory)
  val updated_memory1 = myrete.add_fact(start_memory,RawProposition(0))
  println(updated_memory1)
  val updated_memory2 = myrete.add_fact(updated_memory1,RawProposition(4))
  println(updated_memory2)
  val updated_memory3 = myrete.add_fact(updated_memory2,RawProposition(5))
  println(updated_memory3)
  val updated_memory4 = myrete.rmv_fact(updated_memory3,RawProposition(4))
  println(updated_memory4)
  val updated_memory5 = myrete.rmv_fact(updated_memory4,RawProposition(5))
  println(updated_memory5)


}
//
//object RunRete2 extends App {
//  val domain_parser = new DomainOntologyParser
//
//  val onto_parser = domain_parser.parseAll(domain_parser.domain,"domain \"prova5\" {  " +
//    "category users atom [ luca,john,claudia ] " +
//    "category rooms string [ \"livingroom\",\"kitchen\",\"bedroom\" ]  " +
//    "category sensor_id number [ 1,2,3 ]  " +
//
//    "define input(enum[users])" +
//    "define output(enum[users])" +
//    "define room(enum[rooms])" +
//
//    "rule input(?a), not output(?b) => room(\"kitchen\")" +
//
//    "}")
//
//  val onto = onto_parser.get
//  val logic_builder = new SubLogicBuilder(onto)
//  val rete_builder = new RETEBuilder(logic_builder,onto.axioms)
//
//  val myrete = rete_builder.rete
//  println(myrete.stringGraphviz)
//  val wi = RawState(Array(false,false,false,false,false,false,false,false,false))
//  val start_memory = myrete.reset_memory(wi)
//  println(start_memory)
//  val updated_memory1 = myrete.add_fact(start_memory,RawProposition(0))
//  println(updated_memory1)
//  val updated_memory2 = myrete.add_fact(updated_memory1,RawProposition(4))
//  println(updated_memory2)
//  val updated_memory3 = myrete.rmv_fact(updated_memory2,RawProposition(4))
//  println(updated_memory3)
//
//
//}
