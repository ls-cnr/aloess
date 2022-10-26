package org.icar.subsymbolic

case class RawState(bit_descr:Array[Boolean]) {
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

