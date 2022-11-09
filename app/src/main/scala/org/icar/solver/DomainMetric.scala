package org.icar.solver

import org.icar.subsymbolic.RawState

abstract class DomainMetric {
  def evaluate_state(state:RawState) : Double
  def max : Double
  def min : Double
}
