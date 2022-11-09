package org.icar.solver

abstract class TerminationCondition {
  def check_termination(start_time : Long, iterations : Int, complete_wts : Int) : Boolean
}
case class AndTermination(left:TerminationCondition, right:TerminationCondition) extends TerminationCondition {
  override def check_termination(start_time: Long, iterations: Int, complete_wts: Int): Boolean =
    left.check_termination(start_time,iterations,complete_wts) &&
      right.check_termination(start_time,iterations,complete_wts)
}
case class OrTermination(left:TerminationCondition, right:TerminationCondition) extends TerminationCondition {
  override def check_termination(start_time: Long, iterations: Int, complete_wts: Int): Boolean =
    left.check_termination(start_time,iterations,complete_wts) ||
      right.check_termination(start_time,iterations,complete_wts)
}
case class SolutionTermination(n_sol: Int) extends TerminationCondition {
  override def check_termination(start_time: Long, iterations: Int, complete_wts: Int): Boolean = complete_wts >= n_sol
}
case class TimeTermination(millisec: Long) extends TerminationCondition {
  override def check_termination(start_time: Long, iterations: Int, complete_wts: Int): Boolean = {
    val c_time = System.currentTimeMillis
    val delta_time = c_time-start_time
    delta_time >= millisec
  }
}
case class IterationTermination(its: Int) extends TerminationCondition {
  override def check_termination(start_time: Long, iterations: Int, complete_wts: Int): Boolean = iterations >= its
}
