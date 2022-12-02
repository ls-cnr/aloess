package org.icar.subsymbolic

abstract class RawLogicFormula {
  def satisfied_in(current : RawState) : Boolean
  def next(state_now : RawState): RawFuture
}

case class RawProposition(index:Int) extends RawLogicFormula {
  override def satisfied_in(current: RawState): Boolean = current.satisfies(index)

  override def next(state_now : RawState): RawFuture = {
    if (satisfied_in(state_now))
      RawFuture(true,RawTT())
    else
      RawFuture(false,RawFF())
  }
}

case class RawTT() extends RawLogicFormula {
  override def satisfied_in(current: RawState): Boolean = true

  override def next(state_now : RawState): RawFuture = RawFuture(true,RawTT())
}
case class RawFF() extends RawLogicFormula {
  override def satisfied_in(current: RawState): Boolean = false

  override def next(state_now : RawState): RawFuture = RawFuture(false,RawFF())
}

case class RawConj(left:RawLogicFormula, right:RawLogicFormula) extends RawLogicFormula {
  override def satisfied_in(current: RawState): Boolean = left.satisfied_in(current) && right.satisfied_in(current)

  override def next(state_now : RawState): RawFuture = {
    val next_left = left.next(state_now)
    val next_right = right.next(state_now)

    if (next_left.future_formula != RawTT() && next_right.future_formula != RawTT())
      RawFuture(next_left.success_until_now && next_right.success_until_now, RawConj(next_left.future_formula, next_right.future_formula))

    else if (next_right.future_formula != RawTT())
      RawFuture(next_left.success_until_now && next_right.success_until_now, next_right.future_formula)

    else if (next_left.future_formula != RawTT())
      RawFuture(next_left.success_until_now && next_right.success_until_now, next_left.future_formula)

    else
      RawFuture(next_left.success_until_now && next_right.success_until_now, RawTT())
  }
}
case class RawDisj(left:RawLogicFormula, right:RawLogicFormula) extends RawLogicFormula {
  override def satisfied_in(current: RawState): Boolean = left.satisfied_in(current) || right.satisfied_in(current)

  override def next(state_now : RawState): RawFuture = {
    val next_left = left.next(state_now)
    val next_right = right.next(state_now)
    val left_test = next_left.success_until_now
    val next_a_formula = next_left.future_formula

    val right_test = next_right.success_until_now
    val next_b_formula = next_right.future_formula

    if (next_a_formula != RawTT() && next_b_formula != RawTT())
      RawFuture(left_test || right_test, RawDisj(next_a_formula, next_b_formula))
    else
      RawFuture(left_test || right_test, RawTT())
  }
}
case class RawNeg(op:RawLogicFormula) extends RawLogicFormula {
  override def satisfied_in(current: RawState): Boolean = !op.satisfied_in(current)

  override def next(state_now : RawState): RawFuture = {
    op match {
      case RawProposition(i) => if (RawProposition(i).satisfied_in(state_now)) RawFuture(false,RawFF()) else RawFuture(true,RawTT())
      case RawTT() => RawFuture(false,RawFF())
      case RawFF() => RawFuture(true,RawTT())
      case RawNeg(o) => o.next(state_now)
      case RawConj(l,r) => RawDisj(RawNeg(l),RawNeg(r)).next(state_now)
      case RawDisj(l,r) => RawConj(RawNeg(l),RawNeg(r)).next(state_now)
      case RawImpl(l,r) => RawConj(l,RawNeg(r)).next((state_now))
      case RawIff(l,r) => RawIff(l,RawNeg(r)).next(state_now)
      case RawNext(o) => RawNext(RawNeg(o)).next(state_now)
      case RawUntil(l,r) => RawRelease(RawNeg(l),RawNeg(r)).next(state_now)
      case RawRelease(l,r) => RawUntil(RawNeg(l),RawNeg(r)).next(state_now)
      case RawFinally(o) => RawNeg(RawUntil(RawTT(),o)).next(state_now)
      case RawGlobally(o) => RawNeg(RawFinally(RawNeg(o))).next(state_now)
      case _ => RawFuture(false,RawFF())
    }
  }
}
case class RawImpl(left:RawLogicFormula, right:RawLogicFormula) extends RawLogicFormula {
  override def satisfied_in(current: RawState): Boolean = RawNeg(left).satisfied_in(current)||right.satisfied_in(current)

  override def next(state_now : RawState): RawFuture = RawDisj(RawNeg(left),right).next(state_now)
}
case class RawIff(left:RawLogicFormula, right:RawLogicFormula) extends RawLogicFormula {
  override def satisfied_in(current: RawState): Boolean = (left.satisfied_in(current)&&right.satisfied_in(current)) || (RawNeg(left).satisfied_in(current)&&RawNeg(right).satisfied_in(current))

  override def next(state_now : RawState): RawFuture = RawConj(RawImpl(left,right),RawImpl(right,left)).next(state_now)
}

case class RawNext(op:RawLogicFormula) extends RawLogicFormula {
  override def satisfied_in(current: RawState): Boolean = false

  override def next(state_now : RawState): RawFuture = RawFuture(true,op)
}
case class RawUntil(left:RawLogicFormula, right:RawLogicFormula) extends RawLogicFormula {
  override def satisfied_in(current: RawState): Boolean = false

  override def next(state_now : RawState): RawFuture = {
    val left_next = left.next(state_now)
    val right_next = right.next(state_now)

    if (right_next.success_until_now)
      RawFuture(true, RawTT())
    else if (left_next.success_until_now)
      RawFuture(true, RawUntil(left, right))
    else
      RawFuture(false, RawFF())
  }
}
case class RawRelease(left:RawLogicFormula, right:RawLogicFormula) extends RawLogicFormula {
  override def satisfied_in(current: RawState): Boolean = false

  override def next(state_now : RawState): RawFuture = {
    val next_a = left.next(state_now)
    val next_b = right.next(state_now)
    val a_test = next_a.success_until_now
    val next_a_formula = next_a.future_formula
    val b_test = next_b.success_until_now
    val next_b_formula = next_b.future_formula

    if (b_test) {
      if (a_test)
        RawFuture(true, RawTT())
      else
        RawFuture(true, RawRelease(left, right))
    } else
      RawFuture(false, RawFF())
  }
}
case class RawGlobally(op:RawLogicFormula) extends RawLogicFormula {
  override def satisfied_in(current: RawState): Boolean = false

  override def next(state_now : RawState): RawFuture = RawNeg(RawFinally(RawNeg(op))).next(state_now)
}
case class RawFinally(op:RawLogicFormula) extends RawLogicFormula {
  override def satisfied_in(current: RawState): Boolean = false

  override def next(state_now : RawState): RawFuture = RawUntil(RawTT(),op).next(state_now)
}




