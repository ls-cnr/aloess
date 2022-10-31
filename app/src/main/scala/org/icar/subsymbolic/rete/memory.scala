package org.icar.subsymbolic.rete

import org.icar.subsymbolic.{RawProposition, RawState}

case class Memory(
                   stable_state:RawState,
                   alpha_tokens:Map[Int,AlphaToken],
                   beta_tokens:Map[Int,BetaToken],
                   prod_tokens:Map[Int,ProdToken]
                 ) {
  def touch(prop: RawProposition, value : Boolean) : Memory = {
    val updated_state = stable_state.touch(prop.index,value)
    Memory(updated_state,alpha_tokens,beta_tokens,prod_tokens)
  }

  def set_alpha_token(aid: Int,value:Boolean) : Memory = {
    val updated_alpha = alpha_tokens + (aid -> AlphaToken(value))
    Memory(stable_state,updated_alpha,beta_tokens,prod_tokens)
  }

  def set_beta_left_token(bid: Int,value:Boolean) : Memory = {
    if (beta_tokens.contains(bid)) {
      val beta_memory = beta_tokens(bid)
      val updated_beta_memory = BetaToken(value,beta_memory.right)
      val updated_beta = beta_tokens - bid + (bid -> updated_beta_memory)
      Memory(stable_state,alpha_tokens,updated_beta,prod_tokens)
    } else {
      val updated_beta_memory = BetaToken(value,false)
      val updated_beta = beta_tokens + (bid -> updated_beta_memory)
      Memory(stable_state,alpha_tokens,updated_beta,prod_tokens)
    }
  }

  def set_beta_right_token(bid: Int,value:Boolean) : Memory = {
    if (beta_tokens.contains(bid)) {
      val beta_memory = beta_tokens(bid)
      val updated_beta_memory = BetaToken(beta_memory.left,value)
      val updated_beta = beta_tokens - bid + (bid -> updated_beta_memory)
      Memory(stable_state,alpha_tokens,updated_beta,prod_tokens)
    } else {
      val updated_beta_memory = BetaToken(false,value)
      val updated_beta = beta_tokens + (bid -> updated_beta_memory)
      Memory(stable_state,alpha_tokens,updated_beta,prod_tokens)
    }
  }

  def set_prod_token(pid: Int, bid:Int, value:Boolean) : Memory = {
    if (prod_tokens.contains(pid)) {
      val token_map = prod_tokens(pid).tokens
      val updated_prod_memory = ProdToken(token_map + (bid->value))
      val updated_prod = prod_tokens - pid + (pid -> updated_prod_memory)
      Memory(stable_state,alpha_tokens,beta_tokens,updated_prod)
    } else {
      val updated_prod_memory = ProdToken(Map(bid->value))
      val updated_prod = prod_tokens + (pid -> updated_prod_memory)
      Memory(stable_state,alpha_tokens,beta_tokens,updated_prod)
    }
  }

}

case class AlphaToken(token : Boolean)
case class BetaToken(left : Boolean, right:Boolean)
case class ProdToken(tokens : Map[Int,Boolean])

