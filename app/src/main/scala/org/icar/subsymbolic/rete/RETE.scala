package org.icar.subsymbolic.rete

import org.icar.subsymbolic.builder.{RETEBuilder, SubLogicBuilder}
import org.icar.subsymbolic._
import org.icar.symbolic.Proposition
import org.icar.symbolic.parser.DomainOntologyParser

import scala.collection.mutable.ArrayBuffer

class UnstableRETECondition extends Exception
class UnavailableRETEOperator extends Exception
case class Agenda(memory:Memory,todo:List[RawEvoOperator],done:List[RawEvoOperator]) {
  def check_flip: Boolean = {
    var flag : Boolean = false
    for (op <- todo)// if !flag)
      if (done.contains(op))
        flag = true
    flag
  }

}

class RETE {
  /* static part */
  var dir_alpha_map : Map[Int,DirectAlphaNode] = Map.empty
  var inv_alpha_map : Map[Int,InverseAlphaNode] = Map.empty
  var betas : List[BetaNode] = List.empty
  var prods : List[ProductionNode] = List.empty
  var id_to_use = 0

  def add_fact(memory:Memory, prop : RawProposition) : Memory = {
    val agenda = Agenda(memory,List(RawAdd(prop)),List.empty)
    update(agenda)
  }
  def rmv_fact(memory:Memory, prop : RawProposition) : Memory = {
    val agenda = Agenda(memory,List(RawRem(prop)),List.empty)
    update(agenda)
  }
  def evolution(memory: Memory, evo_list: List[RawEvoOperator]) : Memory = {
    val agenda = Agenda(memory,evo_list,List.empty)
    update(agenda)
  }
  def reset_memory(wi: RawState):Memory = {
    val zero_memory = Memory(wi, Map.empty, Map.empty, Map.empty)
    var agenda = Agenda(zero_memory, List.empty, List.empty)
    for (index <- 0 to wi.satisfies.size - 1) {
      if (wi.satisfies(index)) {
        if (dir_alpha_map.contains(index)) {
          val alpha = dir_alpha_map(index)
          agenda = alpha.check_activation(agenda)
        }
      } else {
        if (inv_alpha_map.contains(index)) {
          val alpha = inv_alpha_map(index)
          agenda = alpha.check_activation(agenda)
        }
      }
    }
    agenda = activate_beta_layer(agenda)
    update(agenda)
  }

  def update(agenda : Agenda) : Memory = {
    var updated_agenda = agenda

    var last_pass = false
    while (!updated_agenda.todo.isEmpty) {
      // check flip condition that reveals unstable state
      if (!last_pass)
        if (updated_agenda.check_flip)
          throw new UnstableRETECondition()

      val op = updated_agenda.todo.head

      updated_agenda = Agenda(updated_agenda.memory,updated_agenda.todo.tail,updated_agenda.done)
      updated_agenda = process_operator(updated_agenda,op)

      if (!last_pass && updated_agenda.todo.isEmpty) {
        last_pass = true
        updated_agenda = activate_beta_layer(updated_agenda)
      }
    }

    updated_agenda.memory
  }
  def process_operator(agenda:Agenda,operator: RawEvoOperator) : Agenda = {
    var activation = -1
    val updated_memory = agenda.memory.apply_operator(operator)
    operator match {
      case RawAdd(add) =>
        activation = add.index
      case RawRem(rmv) =>
        activation = rmv.index
    }
    var updated_agenda = Agenda(updated_memory, agenda.todo, operator :: agenda.done)
    if (dir_alpha_map.contains(activation)) {
      val alpha = dir_alpha_map(activation)
      updated_agenda = alpha.check_activation(updated_agenda)
    }
    if (inv_alpha_map.contains(activation)) {
      val alpha = inv_alpha_map(activation)
      updated_agenda = alpha.check_activation(updated_agenda)
    }
    updated_agenda
  }
  def activate_beta_layer(agenda:Agenda) : Agenda = {
    var updated_agenda = agenda
    for (b <- betas) {
      updated_agenda = b.check_activation(updated_agenda)
    }
    updated_agenda
  }

  def get_or_create_alpha_node(activation: Int, inverse:Boolean) : AlphaNode = {
    if (!inverse)
      dir_alpha_map.getOrElse(activation, {
        val alpha = new DirectAlphaNode(this, id_to_use, activation, List.empty)
        id_to_use += 1
        dir_alpha_map += activation -> alpha
        alpha
      })
    else
      inv_alpha_map.getOrElse(activation, {
        val alpha = new InverseAlphaNode(this, id_to_use, activation, List.empty)
        id_to_use += 1
        inv_alpha_map += activation -> alpha
        alpha
      })
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

    for (n <- dir_alpha_map.values ++ inv_alpha_map.values) {
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

  def stringGraphviz(b : ArrayBuffer[Proposition]) : String = {
    var string = "digraph Rete {\n"

    for (n <- dir_alpha_map.values ++ inv_alpha_map.values) {
      n match {
        case node: DirectAlphaNode =>
          val act = b(node.activation)
          string += s"\"${n.node_label}\"[label=\"a${node.aid}:$act[x${node.activation}]\"]; \n"// "\"" + n.node_label + "\"; \n"
        case node: InverseAlphaNode =>
          val act = b(node.activation)
          string += s"\"${n.node_label}\"[label=\"a${node.aid}:$act[-x${node.activation}]\"]; \n"// "\"" + n.node_label + "\"; \n"
        case _ =>
      }
      for (c <- n.consequences) {
        string += s"\"${n.node_label}\"" //"\"" + n.node_label + "\""
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
    for (n <- prods) {
      val prod = b(n.production)
      string += s"\"${n.node_label}\"[label=\"p${n.pid}:$prod[x${n.production}]\"]; \n" // "\"" + n.node_label + "\"; \n"
    }
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
  val start_memory = myrete.reset_memory(wi)
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

object RunRete2 extends App {
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
  println(myrete.stringGraphviz(logic_builder.inverse))
  val wi = RawState(Array(false,false,false,false,false,true,false,false,false))
  val start_memory = myrete.reset_memory(wi)

  println(start_memory)

  val evolution_scenario = RawEvolution("act",List(RawAdd(RawProposition(0)),RawAdd(RawProposition(4)),RawRem(RawProposition(5))),0)
  val updated_memory = myrete.evolution(start_memory,evolution_scenario.evo)
  println(updated_memory)
}
