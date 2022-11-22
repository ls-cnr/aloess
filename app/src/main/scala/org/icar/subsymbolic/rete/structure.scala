package org.icar.subsymbolic.rete

import org.icar.subsymbolic.RawProposition


abstract class ReteNode(val id:Int) {
  def node_label : String

  def add_token(memory:Memory, from:ReteNode):Memory
  def rmv_token(memory:Memory, from:ReteNode):Memory
}

class AlphaNode(val myrete:RETE,val aid:Int,val activation:Int,val inverse:Boolean,var consequences:List[ReteNode]) extends ReteNode(aid) {
  def check_activation(memory: Memory): Memory = {
    var updated_memory = memory
    if (!inverse) {
      val alpha_memory = memory.alpha_tokens.getOrElse(aid,AlphaToken(false))
      // la condition (prima falsa) ora diventa vera
      if ( alpha_memory.token==false && memory.stable_state.satisfies(activation) ) {
        // aggiunta del token
        updated_memory =add_token(memory,this)
      // la condition (prima vera) ora diventa falsa
      } else if ( alpha_memory.token==true && !memory.stable_state.satisfies(activation) ) {
        // rimozione del token
        updated_memory =rmv_token(memory,this)
      }

    } else {
      val alpha_memory = memory.alpha_tokens.getOrElse(aid,AlphaToken(true))

      // la condition (prima vera) ora diventa falsa
      if ( alpha_memory.token==true && !memory.stable_state.satisfies(activation) ) {
        // aggiunta del token
        updated_memory =add_token(memory,this)
        // la condition (prima falsa) ora diventa vera
      } else if ( alpha_memory.token==false && memory.stable_state.satisfies(activation) ) {
        // rimozione del token
        updated_memory =rmv_token(memory,this)
      }
    }
    updated_memory
  }

  override def add_token(memory:Memory, from:ReteNode):Memory = {
    var updated_memory = memory.set_alpha_token(aid,true)
    for (c <- consequences) updated_memory = c.add_token(updated_memory,this)
    updated_memory
  }
  override def rmv_token(memory:Memory, from:ReteNode):Memory = {
    var updated_memory = memory.set_alpha_token(aid,false)
    for (c <- consequences) updated_memory = c.rmv_token(updated_memory,this)
    updated_memory
  }

  def node_label : String = if (!inverse) s"a($id)=$activation" else s"a($id)=!$activation"
}
class BetaNode(val myrete:RETE,val bid:Int,val consequence:ReteNode) extends ReteNode(bid) {
  def node_label : String = s"b($id)"

  var left_up : Option[ReteNode] = None
  var riglt_up : Option[ReteNode] = None

  def set_left(n:ReteNode) : Unit = left_up=Some(n)
  def set_right(n:ReteNode) : Unit = riglt_up=Some(n)

  override def add_token(memory: Memory, from:ReteNode): Memory = {
    if (from==left_up.get) {
      val updated_memory = memory.set_beta_left_token(bid,true)
      if (updated_memory.beta_tokens(bid).left==true && updated_memory.beta_tokens(bid).right==true)
        consequence.add_token(updated_memory,this)
      else
        updated_memory

    } else {
      val updated_memory = memory.set_beta_right_token(bid,true)
      if (updated_memory.beta_tokens(bid).left==true && updated_memory.beta_tokens(bid).right==true)
        consequence.add_token(updated_memory,this)
      else
        updated_memory
    }
  }
  override def rmv_token(memory:Memory, from:ReteNode):Memory = {
    if (from==left_up.get) {
      val updated_memory = memory.set_beta_left_token(bid,false)
      consequence.rmv_token(updated_memory,this)

    } else {
      val updated_memory = memory.set_beta_right_token(bid,false)
      consequence.rmv_token(updated_memory,this)
    }
  }
}
class ProductionNode(val myrete:RETE,val pid:Int,val production:Int) extends ReteNode(pid) {
  def node_label : String = s"p($id)=$production"

  override def add_token(memory: Memory, from:ReteNode): Memory = {
    val updated_memory = memory.touch(RawProposition(production),true).set_prod_token(pid,from.id,true)
    val previous_tokens = count_tokens(memory)
    val current_tokens = count_tokens(updated_memory)
    if ( previous_tokens==0 && current_tokens>0 )
      myrete.add_fact(updated_memory,RawProposition(production))
    else
      updated_memory
  }
  override def rmv_token(memory:Memory, from:ReteNode):Memory = {
    val updated_memory = memory.set_prod_token(pid,from.id,false)
    val previous_tokens = count_tokens(memory)
    val current_tokens = count_tokens(updated_memory)
    if (previous_tokens>0 && current_tokens==0) {
      val memory_with_retract = memory.touch(RawProposition(production),false)
      myrete.rmv_fact(memory_with_retract, RawProposition(production))
    }else
      updated_memory
  }

  def count_tokens(memory: Memory) : Int = {
    if (memory.prod_tokens.isEmpty || !memory.prod_tokens.contains(pid) )
      0
    else {
      memory.prod_tokens(pid).tokens.filter(x => x._2==true).size
    }
  }
}

