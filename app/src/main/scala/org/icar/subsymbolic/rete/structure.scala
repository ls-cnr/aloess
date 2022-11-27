package org.icar.subsymbolic.rete

import org.icar.subsymbolic.{RawAdd, RawProposition, RawRem}


abstract class ReteNode {
  def node_label : String

  def add_token(agenda : Agenda, from:ReteNode) : Agenda
  def rmv_token(agenda : Agenda, from:ReteNode) : Agenda
}

abstract class AlphaNode extends ReteNode {
  val aid:Int
  var consequences:List[ReteNode]

  def check_activation(agenda : Agenda) : Agenda

  override def add_token(agenda : Agenda, from:ReteNode) : Agenda = {
    val updated_memory = agenda.memory.set_alpha_token(aid,true)
    var updated_agenda = Agenda(updated_memory,agenda.todo,agenda.done)
    for (c <- consequences) updated_agenda = c.add_token(updated_agenda,this)
    updated_agenda
  }
  override def rmv_token(agenda : Agenda, from:ReteNode) : Agenda = {
    val updated_memory = agenda.memory.set_alpha_token(aid,false)
    var updated_agenda = Agenda(updated_memory,agenda.todo,agenda.done)
    for (c <- consequences) updated_agenda = c.rmv_token(updated_agenda,this)
    updated_agenda
  }
}

class DirectAlphaNode(val myrete:RETE,val aid:Int,val activation:Int,var consequences:List[ReteNode]) extends AlphaNode {
  def node_label : String = s"a$aid=>$activation"

  override def check_activation(agenda : Agenda): Agenda = {
    var updated_agenda = agenda

    val memory = updated_agenda.memory
    val alpha_memory = memory.alpha_tokens.getOrElse(aid,AlphaToken(false))

    if ( alpha_memory.token==false ) {
      // alpha node not yet activated
      if (memory.stable_state.satisfies(activation)) {
        updated_agenda = add_token(updated_agenda,this)
      }
    } else {
      // alpha node already activated
      if (!memory.stable_state.satisfies(activation)) {
        updated_agenda = rmv_token(updated_agenda,this)
      }
    }
    updated_agenda
  }
}

class InverseAlphaNode(val myrete:RETE,val aid:Int,val activation:Int,var consequences:List[ReteNode]) extends AlphaNode {
  def node_label : String = s"a$aid=>!$activation"

  override def check_activation(agenda : Agenda): Agenda = {
    var updated_agenda = agenda

    val memory = updated_agenda.memory
    val alpha_memory = memory.alpha_tokens.getOrElse(aid,AlphaToken(false))

    if ( alpha_memory.token==false ) {
      // alpha node not yet activated
      if (!memory.stable_state.satisfies(activation)) {
        updated_agenda = add_token(updated_agenda,this)
      }
    } else {
      // alpha node already activated
      if (memory.stable_state.satisfies(activation)) {
        updated_agenda = rmv_token(updated_agenda,this)
      }
    }
    updated_agenda
  }
}


class BetaNode(val myrete:RETE,val bid:Int,val consequence:ReteNode) extends ReteNode {
  def node_label: String = s"b$bid"

  var left_up: Option[ReteNode] = None
  var riglt_up: Option[ReteNode] = None

  def set_left(n: ReteNode): Unit = left_up = Some(n)

  def set_right(n: ReteNode): Unit = riglt_up = Some(n)

  override def add_token(agenda: Agenda, from: ReteNode): Agenda = {
    val updated_memory: Memory =
      if (from == left_up.get) agenda.memory.set_beta_left_token(bid, true)
      else agenda.memory.set_beta_right_token(bid, true)

    val updated_agenda = Agenda(updated_memory, agenda.todo, agenda.done)
    consequence.add_token(updated_agenda, this)
  }

  override def rmv_token(agenda: Agenda, from: ReteNode): Agenda = {
    val updated_memory: Memory =
      if (from == left_up.get) agenda.memory.set_beta_left_token(bid, false)
      else agenda.memory.set_beta_right_token(bid, false)

    val updated_agenda = Agenda(updated_memory, agenda.todo, agenda.done)
    consequence.rmv_token(updated_agenda, this)
  }

  def check_activation(agenda : Agenda): Agenda = {
    var propagate : Boolean = false
    if (agenda.memory.beta_tokens.contains(bid))
      if (agenda.memory.beta_tokens(bid).left == true && agenda.memory.beta_tokens(bid).right == true)
        propagate = true

    if (propagate)
      consequence.add_token(agenda, this)
    else
      agenda
  }
}

class ProductionNode(val myrete:RETE,val pid:Int,val production:Int) extends ReteNode {
  def node_label : String = s"p$pid=>$production"

  override def add_token(agenda : Agenda, from:ReteNode) : Agenda = {
    val prodtoken = agenda.memory.prod_tokens.getOrElse(pid,ProdToken(false))
    if (prodtoken.token==false) {
      val updated_memory = agenda.memory.set_prod_token(pid, true)
      val updated_todo = RawAdd(RawProposition(production)) :: agenda.todo
      Agenda(updated_memory,updated_todo,agenda.done)
    } else {
      agenda
    }
  }

  override def rmv_token(agenda : Agenda, from:ReteNode) : Agenda = {
    val prodtoken = agenda.memory.prod_tokens.getOrElse(pid,ProdToken(false))
    if (prodtoken.token==true) {
      val updated_memory = agenda.memory.set_prod_token(pid, false)
      val updated_todo = RawRem(RawProposition(production)) :: agenda.todo
      Agenda(updated_memory,updated_todo,agenda.done)
    } else {
      agenda
    }
  }

}