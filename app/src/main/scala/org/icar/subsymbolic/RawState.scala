package org.icar.subsymbolic

import org.icar.symbolic.Proposition

case class RawState(bit_descr:Array[Boolean]) {
  def touch(prop: Int, value : Boolean) : RawState = {
    val updated_bit = bit_descr.clone()
    updated_bit(prop) = value
    RawState(updated_bit)
  }

  lazy val compact_description = calculate_compact_description
  lazy val hash : Int = bit_descr.toSeq.hashCode()

  override def toString: String = compact_description
  override def hashCode() : Int = hash
  override def equals(obj: Any): Boolean = {
    obj match {
      case that:RawState => this.hash == that.hash
      case _ => false
    }
  }

  private def calculate_compact_description : String = {
    var first = true
    var s ="["
    for (i<-0 until bit_descr.length)
      if (bit_descr(i)) {
        if (first)
          first = false
        else
          s+=","
        s+="x"+i
      }
    s+"]"
  }
}

case class RawPresent(success_now: Boolean, state_now: RawState)
case class RawFuture(success_until_now: Boolean, future_formula: RawLogicFormula)

object RunRswState extends App {
  val state = RawState(Array(true,true,false,false))
  val updated_sate = state.touch(2,true)
  println(state)
  println(updated_sate)
}